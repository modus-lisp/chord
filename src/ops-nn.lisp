;;;; ops-nn.lisp — the ops that do the actual work: matmul and convolution.
;;;;
;;;; Everything else in this graph is bookkeeping around these.  All of this
;;;; model's convolutions are 1-D — VITS is a waveform model, and its "images"
;;;; are (batch, channels, time) — so that is what is written here, with the
;;;; grouped and dilated cases the decoder needs.
;;;;
;;;; The loops are ordered so the innermost one walks time, which is contiguous
;;;; in memory and is the axis with 17152 elements in the last decoder layer.
;;;; That ordering is also what a vectorized version wants, so this is the shape
;;;; the SIMD rewrite starts from rather than something it has to undo.

(in-package #:chord)

;;; ---- matmul ----------------------------------------------------------------

(defop "MatMul" (node ins)
  (destructuring-bind (a b) ins
    (let* ((ashape (tensor-shape a)) (bshape (tensor-shape b))
           (ra (length ashape)) (rb (length bshape)))
      (when (or (< ra 2) (< rb 2))
        (error "MatMul with a rank-~d operand is not implemented" (min ra rb)))
      (let* ((m (aref ashape (- ra 2))) (k (aref ashape (1- ra)))
             (k2 (aref bshape (- rb 2))) (n (aref bshape (1- rb))))
        (unless (= k k2)
          (error "MatMul inner dimensions disagree: ~d vs ~d" k k2))
        (let* ((batch (broadcast-shapes (subseq ashape 0 (- ra 2))
                                        (subseq bshape 0 (- rb 2))))
               ;; a step along a batch axis skips a whole matrix
               (sa (map 'vector (lambda (s) (* s m k))
                        (broadcast-strides (subseq ashape 0 (- ra 2)) batch)))
               (sb (map 'vector (lambda (s) (* s k n))
                        (broadcast-strides (subseq bshape 0 (- rb 2)) batch)))
               (so (map 'vector (lambda (s) (* s m n)) (row-major-strides batch)))
               (out-shape (concatenate 'simple-vector batch (vector m n)))
               (res (make-tensor (tensor-dtype a) out-shape))
               (da (the (simple-array single-float (*)) (tensor-data a)))
               (db (the (simple-array single-float (*)) (tensor-data b)))
               (dr (the (simple-array single-float (*)) (tensor-data res))))
          (declare (type index m k n))
          (do-broadcast ((ia sa) (ib sb) (io so) batch)
            (dotimes (row m)
              (let ((arow (+ ia (* row k)))
                    (orow (+ io (* row n))))
                ;; k on the outside, n contiguous on the inside: one row of B is
                ;; streamed per accumulation step instead of strided over
                (dotimes (kk k)
                  (let ((x (aref da (+ arow kk)))
                        (brow (+ ib (* kk n))))
                    (unless (zerop x)
                      (dotimes (col n)
                        (incf (aref dr (+ orow col)) (* x (aref db (+ brow col)))))))))))
          res)))))

;;; ---- convolution -----------------------------------------------------------

(defun conv1d-attrs (node)
  (values (first (node-attr node "strides" '(1)))
          (first (node-attr node "dilations" '(1)))
          (node-attr node "pads" '(0 0))
          (node-attr node "group" 1)))

;;; The two kernels below are split out of their ops for one reason: they run at
;;; (speed 3) (safety 0) with every index typed, while the op around them keeps
;;; normal safety for the shape checks.  Before the split, the profile of one
;;; synthesis put 14.1% of the whole run in GENERIC-+ and 5.3% in GENERIC-* —
;;; the accumulation was dispatching on operand type per tap.
;;;
;;; Every offset variable is declared DIM, and the row bases are hoisted to the
;;; loop level where they stop changing, so no expression ever multiplies three
;;; extents together (that overflows a fixnum and puts the generic call back).
;;;
;;; Both kernels take a range of OUTPUT ROWS — the (batch item, output channel)
;;; pairs, flattened — rather than the whole tensor, because rows are what the
;;; worker pool splits on.  Rows write disjoint slices of DR, and the loops that
;;; accumulate into one output element (input channel, then kernel position) are
;;; still nested in that order, so a row's answer does not depend on how the
;;; range was cut.  Same summation order as before the split, therefore the same
;;; float result; inspect/node-gate.lisp is what says so.

(defun conv1d-core (dx dw dr row-lo row-hi cin len cout cgroup kw out-len
                    stride dilation pad-begin per-group-out)
  "Accumulate the 1-D convolution of DX by DW into rows [ROW-LO, ROW-HI) of DR,
which the caller has already filled with the bias (or with zeros)."
  (declare (type (simple-array single-float (*)) dx dw dr)
           (type dim row-lo row-hi cin len cout cgroup kw out-len stride dilation
                 per-group-out)
           (type (signed-byte 30) pad-begin)
           (optimize (speed 3) (safety 0)))
  (let ((xplane (* cin len))            ; elements per batch item, input
        (oplane (* cout out-len))       ;                          output
        (wplane (* cgroup kw)))         ; elements per output channel, weights
    (declare (type dim xplane oplane wplane))
    (loop for row of-type dim from row-lo below row-hi do
      (multiple-value-bind (nb m) (floor row cout)
        (declare (type dim nb m))
        (let ((xbase (* nb xplane))
              (orow (+ (* nb oplane) (* m out-len)))
              (wbase (* m wplane))
              (gr (floor m per-group-out)))
          (declare (type dim xbase orow wbase gr))
          (dotimes (c cgroup)
            (declare (type dim c))
            (let* ((chan (+ (* gr cgroup) c))
                   (xrow (+ xbase (* chan len)))
                   (wrow (+ wbase (* c kw))))
              (declare (type dim chan xrow wrow))
              (dotimes (kk kw)
                (declare (type dim kk))
                (let ((weight (aref dw (+ wrow kk)))
                      ;; the taps this kernel position contributes to are the
                      ;; ones whose source sample is inside the signal; solve
                      ;; for that range once instead of testing every tap
                      (shift (- (* kk dilation) pad-begin)))
                  (declare (type fixnum shift))
                  (unless (zerop weight)
                    ;; both ends are clamped into DIM: LO because a tap index is
                    ;; never negative, HI because an empty range has to stay
                    ;; representable when SHIFT runs off the end of the signal
                    (let ((lo (max 0 (ceiling (- shift) stride)))
                          (hi (max 0 (min out-len (ceiling (- len shift) stride)))))
                      (declare (type dim lo hi))
                      (loop for o of-type dim from lo below hi
                            do (incf (aref dr (+ orow o))
                                     (* weight
                                        (aref dx (+ xrow shift (* o stride))))))))))))))))
  (values))

(defop "Conv" (node ins)
  (destructuring-bind (x w &optional bias) ins
    (let ((kernel (node-attr node "kernel_shape")))
      (unless (= 1 (length kernel))
        (error "Conv over ~d spatial dimensions is not implemented (this graph is all 1-D)"
               (length kernel))))
    (multiple-value-bind (stride dilation pads group) (conv1d-attrs node)
      (let* ((xs (tensor-shape x)) (ws (tensor-shape w))
             (batch (aref xs 0)) (cin (aref xs 1)) (len (aref xs 2))
             (cout (aref ws 0)) (cgroup (aref ws 1)) (kw (aref ws 2))
             (pad-begin (first pads)) (pad-end (second pads))
             (out-len (1+ (floor (- (+ len pad-begin pad-end) (* dilation (1- kw)) 1) stride)))
             (per-group-out (floor cout group))
             (res (make-tensor :f32 (vector batch cout out-len)))
             (dx (the (simple-array single-float (*)) (tensor-data x)))
             (dw (the (simple-array single-float (*)) (tensor-data w)))
             (dr (the (simple-array single-float (*)) (tensor-data res)))
             (db (and bias (the (simple-array single-float (*)) (tensor-data bias)))))
        (declare (type index len out-len kw))
        (unless (= cin (* cgroup group))
          (error "Conv: input has ~d channels but the weights want ~d x ~d"
                 cin group cgroup))
        ;; the bias is the initial value of every output tap, so it goes in
        ;; before the accumulation rather than inside it
        (when db
          (dotimes (nb batch)
            (dotimes (m cout)
              (let ((orow (+ (* nb cout out-len) (* m out-len)))
                    (b (aref db m)))
                (dotimes (o out-len) (setf (aref dr (+ orow o)) b))))))
        ;; a row costs CGROUP * KW multiply-adds per output tap; hand a thread
        ;; work worth having, not a row that finishes before it wakes up
        (parallel-range (* batch cout)
                        (lambda (lo hi)
                          (conv1d-core dx dw dr lo hi cin len cout cgroup kw out-len
                                       stride dilation pad-begin per-group-out))
                        :min-chunk (max 1 (ceiling +parallel-grain+
                                                   (max 1 (* cgroup kw out-len)))))
        res))))

(defun conv-transpose1d-core (dx dw dr row-lo row-hi cin len cout cgroup-in
                              cgroup-out kw out-len stride dilation pad-begin)
  "Scatter DX through DW into rows [ROW-LO, ROW-HI) of DR, which the caller has
already filled with the bias (or with zeros).  Note the weight layout:
(in-channels, out/group, kernel) — the transpose of Conv's."
  (declare (type (simple-array single-float (*)) dx dw dr)
           (type dim row-lo row-hi cin len cout cgroup-in cgroup-out kw out-len
                 stride dilation)
           (type (signed-byte 30) pad-begin)
           (optimize (speed 3) (safety 0)))
  (let ((xplane (* cin len))
        (oplane (* cout out-len))
        (wplane (* cgroup-out kw)))     ; elements per INPUT channel here
    (declare (type dim xplane oplane wplane))
    (loop for row of-type dim from row-lo below row-hi do
      (multiple-value-bind (nb ochan) (floor row cout)
        (declare (type dim nb ochan))
        (multiple-value-bind (g m) (floor ochan cgroup-out)
          (declare (type dim g m))
          (let ((xbase (* nb xplane))
                (orow (+ (* nb oplane) (* ochan out-len)))
                (wcol (* m kw)))
            (declare (type dim xbase orow wcol))
            (dotimes (ci cgroup-in)
              (declare (type dim ci))
              (let* ((c (+ (* g cgroup-in) ci))
                     (xrow (+ xbase (* c len)))
                     (wrow (+ (* c wplane) wcol)))
                (declare (type dim c xrow wrow))
                (dotimes (kk kw)
                  (declare (type dim kk))
                  (let ((weight (aref dw (+ wrow kk)))
                        ;; output tap for input sample l is l*stride + shift
                        (shift (- (* kk dilation) pad-begin)))
                    (declare (type fixnum shift))
                    (unless (zerop weight)
                      (let ((lo (max 0 (ceiling (- shift) stride)))
                            (hi (max 0 (min len (ceiling (- out-len shift) stride)))))
                        (declare (type dim lo hi))
                        (loop for l of-type dim from lo below hi
                              do (incf (aref dr (+ orow shift (* l stride)))
                                       (* weight (aref dx (+ xrow l))))))))))))))))
  (values))

(defop "ConvTranspose" (node ins)
  (destructuring-bind (x w &optional bias) ins
    (let ((kernel (node-attr node "kernel_shape")))
      (unless (= 1 (length kernel))
        (error "ConvTranspose over ~d spatial dimensions is not implemented"
               (length kernel))))
    (multiple-value-bind (stride dilation pads group) (conv1d-attrs node)
      (let* ((xs (tensor-shape x)) (ws (tensor-shape w))
             (batch (aref xs 0)) (cin (aref xs 1)) (len (aref xs 2))
             ;; note the weight layout: (in-channels, out-channels/group, kernel)
             (cgroup-in (floor cin group)) (cgroup-out (aref ws 1)) (kw (aref ws 2))
             (cout (* cgroup-out group))
             (pad-begin (first pads)) (pad-end (second pads))
             (extra (first (node-attr node "output_padding" '(0))))
             (out-len (+ (* (1- len) stride) (- pad-begin) (- pad-end)
                         (* dilation (1- kw)) 1 extra))
             (res (make-tensor :f32 (vector batch cout out-len)))
             (dx (the (simple-array single-float (*)) (tensor-data x)))
             (dw (the (simple-array single-float (*)) (tensor-data w)))
             (dr (the (simple-array single-float (*)) (tensor-data res)))
             (db (and bias (the (simple-array single-float (*)) (tensor-data bias)))))
        (declare (type index len out-len kw))
        (when db
          (dotimes (nb batch)
            (dotimes (m cout)
              (let ((orow (+ (* nb cout out-len) (* m out-len)))
                    (b (aref db m)))
                (dotimes (o out-len) (setf (aref dr (+ orow o)) b))))))
        (parallel-range (* batch cout)
                        (lambda (lo hi)
                          (conv-transpose1d-core dx dw dr lo hi cin len cout
                                                 cgroup-in cgroup-out kw out-len
                                                 stride dilation pad-begin))
                        :min-chunk (max 1 (ceiling +parallel-grain+
                                                   (max 1 (* cgroup-in kw len)))))
        res))))

;;; ---- noise -----------------------------------------------------------------
;;;
;;; VITS draws Gaussian noise in two places: the stochastic duration predictor
;;; (scaled by noise_w) and the flow (scaled by noise_scale).  Both scales come
;;; in as inputs, so setting them to zero makes the whole graph a pure function
;;; — which is exactly what the fixture dump does, and why a golden value exists
;;; for all but three of its nodes.  With the scales at their voice defaults the
;;; noise is real, and this is where it comes from.

(defvar *noise-seed* 1
  "Seed for the graph's Gaussian noise.  Fixed by default: two runs of the same
text should give the same audio unless something asks for otherwise.")

(defvar *noise-state* nil)

(defun reset-noise (&optional (seed *noise-seed*))
  (setf *noise-state* (sb-ext:seed-random-state seed)))

(defun random-normal ()
  "One standard normal, Box-Muller.  Not the same stream as onnxruntime's — the
per-node gate skips the three values downstream of this, and everything past the
noise-scale multiply agrees again."
  (unless *noise-state* (reset-noise))
  (let ((u (max 1d-300 (random 1d0 *noise-state*)))
        (v (random 1d0 *noise-state*)))
    (* (sqrt (* -2d0 (log u))) (cos (* 2d0 pi v)))))

(defop "RandomNormalLike" (node ins)
  (let* ((a (first ins))
         (dtype (if (node-attr node "dtype") (onnx-dtype (node-attr node "dtype"))
                    (tensor-dtype a)))
         (mean (float (node-attr node "mean" 0.0d0) 1d0))
         (scale (float (node-attr node "scale" 1.0d0) 1d0))
         (res (make-tensor dtype (tensor-shape a))))
    (with-typed-data (dr res :dtypes (:f32 :f64))
      (dotimes (i (length dr))
        (setf (aref dr i) (coerce (+ mean (* scale (random-normal)))
                                  (array-element-type dr)))))
    res))
