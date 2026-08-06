;;;; node-gate.lisp — compare every node against onnxruntime, in graph order.
;;;;
;;;;     sbcl --dynamic-space-size 8192 --load inspect/node-gate.lisp
;;;;
;;;; A TTS engine gives one bit of feedback at the end: the audio sounds wrong.
;;;; That is not enough to find an arithmetic mistake in node 900 of 2755.  So
;;;; the gate runs chord's own graph and, as each node finishes, compares its
;;;; output against the value onnxruntime produced for the same node on the same
;;;; input.  A wrong answer names the node that FIRST diverged, which is nearly
;;;; always the one with the bug in it — everything after is downstream damage.
;;;;
;;;; Integer and boolean tensors must match exactly; there is no such thing as a
;;;; nearly-right index.  Float tensors are compared with a tolerance, because a
;;;; different summation order is not a bug, and reported with their actual
;;;; error so that "passes" never hides a value that is drifting.
;;;;
;;;; Three values cannot be compared, and the gate says so rather than skipping
;;;; them quietly: the two RandomNormalLike draws and the one multiply between
;;;; the flow's draw and its noise-scale gate.  Every other node downstream of
;;;; the noise is multiplied by a scale of zero and agrees again.

(asdf:load-system :chord)
(asdf:load-system :mill/npy)      ; the .npy reader, so onnxruntime's dumps
(in-package #:chord)              ; can be read without a Python in the loop

(defparameter *fixtures*
  (pathname (or (sb-posix:getenv "CHORD_FIXTURES") "/mnt/lisp/chord/fixtures/")))
(defparameter *export*
  (pathname (or (sb-posix:getenv "CHORD_GRAPH")
                "/mnt/lisp/chord/export/en_US-lessac-medium.graph")))

(defparameter *noise-tainted*
  '("/dp/RandomNormalLike_output_0" "/RandomNormalLike_output_0" "/Mul_5_output_0")
  "Values that depend on a random draw.  onnxruntime's stream is not ours, so
these cannot be compared; the nodes that consume them multiply by a zeroed noise
scale, and the comparison resumes there.")

(defparameter *atol* 1d-4)
(defparameter *rtol* 1d-4)

(defun load-manifest ()
  (with-open-file (s (merge-pathnames "manifest.sexp" *fixtures*) :external-format :utf-8)
    (let ((*read-eval* nil) (*package* (find-package '#:chord)))
      (read s))))

(defun fixture-index (manifest)
  "Value name -> the file number holding onnxruntime's answer for it."
  (let ((h (make-hash-table :test #'equal)))
    (loop for (n name dtype shape) in (getf manifest :values)
          do (progn dtype shape (setf (gethash name h) n)))
    h))

(defun read-fixture (n)
  (read-npy (merge-pathnames (format nil "values/~d.npy" n) *fixtures*)))

(defun feeds-from-fixtures (model)
  (loop for (name) in (model-inputs model)
        collect (cons name (read-npy (merge-pathnames (format nil "inputs/~a.npy" name)
                                                      *fixtures*)))))

(defun compare (ours theirs)
  "Values: pass-p, the error, and how it was judged."
  (cond ((not (equalp (tensor-shape ours) (tensor-shape theirs)))
         (values nil nil :shape))
        ((not (eq (tensor-dtype ours) (tensor-dtype theirs)))
         (values nil nil :dtype))
        ((member (tensor-dtype ours) '(:i64 :i32 :bool))
         (let ((d (tensor-max-abs-diff ours theirs)))
           (values (zerop d) d :exact)))
        (t (let ((d (tensor-max-abs-diff ours theirs))
                 (scale (let ((m 0d0))
                          (with-typed-data (dt theirs :dtypes (:f32 :f64))
                            (dotimes (i (length dt))
                              (let ((a (abs (float (aref dt i) 1d0))))
                                (when (> a m) (setf m a)))))
                          m)))
             (values (<= d (+ *atol* (* *rtol* scale))) d :tolerance)))))

(defun run-gate (&key (stop-after 1) verbose)
  (let* ((manifest (load-manifest))
         (index (fixture-index manifest))
         (model (load-model *export*))
         (compared 0) (skipped 0) (missing 0)
         (worst 0d0) (worst-name nil)
         (failures '()))
    (format t "~&graph:    ~a~%" (file-namestring *export*))
    (format t "outputs:  ~{~a~^, ~}~%" (getf manifest :outputs))
    (format t "nodes:    ~d, fixtures: ~d~%~%"
            (length (model-nodes model)) (length (getf manifest :values)))
    (handler-case
        (run-model
         model (feeds-from-fixtures model)
         :on-node
         (lambda (i node outs)
           (loop for name across (node-outputs node)
                 for ours in outs
                 do (cond
                      ((member name *noise-tainted* :test #'string=)
                       (incf skipped))
                      ((not (gethash name index)) (incf missing))
                      (t
                       (let ((theirs (read-fixture (gethash name index))))
                         (multiple-value-bind (ok err how) (compare ours theirs)
                           (incf compared)
                           (when (and err (> err worst))
                             (setf worst err worst-name name))
                           (when verbose
                             (format t "~5d ~18a ~a  err ~,3e~%" i (node-op node) name
                                     (float (or err 0) 1d0)))
                           (unless ok
                             (push (list i (node-op node) (node-name node) name err how
                                         (coerce (tensor-shape ours) 'list)
                                         (coerce (tensor-shape theirs) 'list))
                                   failures)
                             (when (>= (length failures) stop-after)
                               (return-from run-gate
                                 (report compared skipped missing worst worst-name
                                         (reverse failures) ours theirs)))))))))))
      (unimplemented-op (c)
        (format t "~&STOPPED: ~a~%~%" c)
        (format t "compared ~d nodes cleanly before that.~%" compared)
        (format t "~&still unimplemented, by node count:~%")
        (loop for (op . n) in (model-missing-ops model)
              do (format t "  ~5d  ~a~%" n op))
        (return-from run-gate nil)))
    (report compared skipped missing worst worst-name (reverse failures) nil nil)))

(defun report (compared skipped missing worst worst-name failures ours theirs)
  (format t "~&~%compared ~d values against onnxruntime~%" compared)
  (format t "~d skipped as noise-dependent, ~d had no fixture~%" skipped missing)
  (format t "worst error among those that passed: ~,3e (~a)~%" worst worst-name)
  (cond ((null failures)
         (format t "~%GATE GREEN — every comparable node matches within ~,1e + ~,1e*|ref|~%"
                 *atol* *rtol*)
         t)
        (t
         (format t "~%FIRST DIVERGENCE:~%")
         (loop for (i op name value err how oshape tshape) in failures
               do (format t "  node ~d: ~a ~a~%    value ~a~%    ~a~%"
                          i op name value
                          (case how
                            (:shape (format nil "shape ~a, onnxruntime says ~a" oshape tshape))
                            (:dtype "dtype disagrees")
                            (t (format nil "error ~,3e, allowed ~,1e + ~,1e*|ref| (shape ~a)"
                                       err *atol* *rtol* oshape)))))
         (when (and ours theirs (< (tensor-size ours) 200))
           (format t "~&  ours:  ") (print-tensor ours :limit 12)
           (format t "~&  theirs:") (print-tensor theirs :limit 12))
         nil)))

(defun main ()
  (let ((ok (run-gate)))
    (finish-output)
    (sb-ext:exit :code (if ok 0 1))))

(main)
