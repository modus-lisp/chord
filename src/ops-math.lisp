;;;; ops-math.lisp — elementwise arithmetic, comparison, and activations.
;;;;
;;;; Every binary op here broadcasts, and broadcasting is done with strides of
;;;; zero rather than by materializing the stretched operand: a (1,1,30) mask
;;;; against a (1,192,30) tensor should not cost a 192x copy.
;;;;
;;;; The dtype dispatch is a macro rather than a generic function because the
;;;; whole point is that the inner loop sees a SIMPLE-ARRAY of a known element
;;;; type.  Without that SBCL boxes every float it touches, and this graph
;;;; touches a few hundred million of them per second of speech.

(in-package #:chord)

;;; ---- binary ----------------------------------------------------------------

(defmacro def-binop (name (xa xb) &key form int-form (out :same)
                                       (dtypes '(:f32 :f64 :i64 :i32)))
  "Define an elementwise binary op.  FORM computes the result from XA and XB;
INT-FORM replaces it for integer dtypes when the two differ (Div is the reason:
ONNX integer division truncates).  OUT is :SAME or :BOOL."
  `(defop ,name (node ins)
     (let* ((a (first ins)) (b (second ins))
            (shape (broadcast-shapes (tensor-shape a) (tensor-shape b)))
            (sa (broadcast-strides (tensor-shape a) shape))
            (sb (broadcast-strides (tensor-shape b) shape)))
       (unless (eq (tensor-dtype a) (tensor-dtype b))
         (error "~a: operands differ in dtype (~a vs ~a)" ,name
                (tensor-dtype a) (tensor-dtype b)))
       (ecase (tensor-dtype a)
         ,@(loop for dt in dtypes
                 for et = (dtype-element-type dt)
                 for ot = (if (eq out :bool) '(unsigned-byte 8) et)
                 for body = (if (and int-form (member dt '(:i64 :i32 :bool)))
                                int-form form)
                 collect
                 `(,dt (let ((res (make-tensor ,(if (eq out :bool) :bool dt) shape))
                             (da (the (simple-array ,et (*)) (tensor-data a)))
                             (db (the (simple-array ,et (*)) (tensor-data b))))
                         (let ((dr (the (simple-array ,ot (*)) (tensor-data res))))
                           (if (and (equalp (tensor-shape a) shape)
                                    (equalp (tensor-shape b) shape))
                               ;; Nothing is being stretched, so all three
                               ;; tensors are walked by the same index and the
                               ;; odometer is pure overhead — which it was: an
                               ;; Add cost 20 ns an element with it, and this
                               ;; graph's Adds alone were 27% of a synthesis.
                               (locally (declare (optimize (speed 3) (safety 0)))
                                 (dotimes (i (length dr))
                                   (let ((,xa (aref da i)) (,xb (aref db i)))
                                     (declare (ignorable ,xa ,xb))
                                     (setf (aref dr i) ,body))))
                               (do-broadcast ((ia sa) (ib sb) shape)
                                 (let ((,xa (aref da ia)) (,xb (aref db ib)))
                                   (declare (ignorable ,xa ,xb))
                                   (setf (aref dr i) ,body)))))
                         res)))))))

(defun pow-f (base exponent)
  "ONNX Pow on floats.  Computed in double and rounded back, which is at least as
close to the true value as a float32 pow; a negative base with a fractional
exponent is NaN, not a complex number."
  (let ((b (float base 1d0)) (e (float exponent 1d0)))
    (cond ((and (minusp b) (/= e (ffloor e)))
           (sb-kernel:make-double-float #x7FF80000 0))
          (t (expt b e)))))

(def-binop "Add" (x y) :form (+ x y) :dtypes (:f32 :f64 :i64 :i32))
(def-binop "Sub" (x y) :form (- x y) :dtypes (:f32 :f64 :i64 :i32))
(def-binop "Mul" (x y) :form (* x y) :dtypes (:f32 :f64 :i64 :i32))
(def-binop "Div" (x y) :form (/ x y) :int-form (values (truncate x y))
                       :dtypes (:f32 :f64 :i64 :i32))
(def-binop "Pow" (x y) :form (float (pow-f x y) x) :int-form (expt x y)
                       :dtypes (:f32 :f64 :i64 :i32))

(def-binop "Equal" (x y) :form (if (= x y) 1 0) :out :bool
                         :dtypes (:f32 :f64 :i64 :i32 :bool))
(def-binop "Less" (x y) :form (if (< x y) 1 0) :out :bool)
(def-binop "LessOrEqual" (x y) :form (if (<= x y) 1 0) :out :bool)
(def-binop "Greater" (x y) :form (if (> x y) 1 0) :out :bool)
(def-binop "GreaterOrEqual" (x y) :form (if (>= x y) 1 0) :out :bool)
(def-binop "And" (x y) :form (if (and (/= x 0) (/= y 0)) 1 0) :dtypes (:bool))
(def-binop "Or" (x y) :form (if (or (/= x 0) (/= y 0)) 1 0) :dtypes (:bool))

;;; ---- selection -------------------------------------------------------------

(defop "Where" (node ins)
  ;; All three operands broadcast against each other, which this graph relies on:
  ;; the condition is usually a (1,1,T) mask and the branches are full tensors.
  (destructuring-bind (cond-t a b) ins
    (let* ((shape (broadcast-shapes (broadcast-shapes (tensor-shape cond-t)
                                                      (tensor-shape a))
                                    (tensor-shape b)))
           (sc (broadcast-strides (tensor-shape cond-t) shape))
           (sa (broadcast-strides (tensor-shape a) shape))
           (sb (broadcast-strides (tensor-shape b) shape))
           (dc (the (simple-array (unsigned-byte 8) (*)) (tensor-data cond-t))))
      (unless (eq (tensor-dtype a) (tensor-dtype b))
        (error "Where: branches differ in dtype (~a vs ~a)"
               (tensor-dtype a) (tensor-dtype b)))
      (macrolet ((dispatch ()
                   `(ecase (tensor-dtype a)
                      ,@(loop for dt in '(:f32 :f64 :i64 :i32 :bool)
                              for et = (dtype-element-type dt)
                              collect
                              `(,dt (let ((res (make-tensor ,dt shape))
                                          (da (the (simple-array ,et (*)) (tensor-data a)))
                                          (db (the (simple-array ,et (*)) (tensor-data b))))
                                      (let ((dr (the (simple-array ,et (*)) (tensor-data res))))
                                        (do-broadcast ((ic sc) (ia sa) (ib sb) shape)
                                          (setf (aref dr i)
                                                (if (zerop (aref dc ic))
                                                    (aref db ib)
                                                    (aref da ia)))))
                                      res))))))
        (dispatch)))))

;;; ---- unary -----------------------------------------------------------------

(defmacro def-unop (name (x) &key form int-form (dtypes '(:f32 :f64)))
  "Define an elementwise unary op.  Same shape as DEF-BINOP; the output always
has the input's dtype."
  `(defop ,name (node ins)
     (let ((a (first ins)))
       (ecase (tensor-dtype a)
         ,@(loop for dt in dtypes
                 for et = (dtype-element-type dt)
                 for body = (if (and int-form (member dt '(:i64 :i32 :bool))) int-form form)
                 collect
                 `(,dt (let ((res (make-tensor ,dt (tensor-shape a)))
                             (da (the (simple-array ,et (*)) (tensor-data a))))
                         (let ((dr (the (simple-array ,et (*)) (tensor-data res))))
                           (locally (declare (optimize (speed 3) (safety 0)))
                             (dotimes (i (length da))
                               (let ((,x (aref da i)))
                                 (declare (ignorable ,x))
                                 (setf (aref dr i) ,body)))))
                         res)))))))

;;; erf has no CL equivalent and GELU is 24 nodes of this graph, so it is worth
;;; getting right rather than approximating.  This is the confluent
;;; hypergeometric series
;;;
;;;     erf(x) = 2x/sqrt(pi) * e^(-x^2) * SUM (2x^2)^n / (1*3*5*...*(2n+1))
;;;
;;; whose terms are all positive — nothing cancels, so the relative error stays
;;; near machine epsilon instead of degrading the way the alternating Taylor
;;; series does.  Past |x|=6 the answer is +-1 to well under a double's
;;; resolution, let alone a float's.
(defconstant +2/sqrt-pi+ 1.1283791670955126d0)

(defun erf-d (x)
  (declare (type double-float x))
  (let ((ax (abs x)))
    (cond ((> ax 6d0) (float-sign x 1d0))
          ((zerop ax) x)
          (t (let* ((x2 (* ax ax))
                    (term 1d0)
                    (sum 1d0))
               (declare (type double-float term sum))
               (loop for n of-type fixnum from 1 below 300
                     do (setf term (/ (* term 2d0 x2) (+ (* 2d0 n) 1d0)))
                        (incf sum term)
                     while (> term (* 1d-18 sum)))
               (float-sign x (* +2/sqrt-pi+ ax (exp (- x2)) sum)))))))

(declaim (inline sigmoid-d softplus-d))
(defun sigmoid-d (x) (/ 1d0 (+ 1d0 (exp (- (float x 1d0))))))
(defun softplus-d (x)
  ;; ln(1+e^x) overflows for large x and is just x there; the branch keeps the
  ;; big-input case exact instead of infinite.
  (let ((x (float x 1d0)))
    (if (> x 30d0) x (log (+ 1d0 (exp x))))))

(def-unop "Neg" (x) :form (- x) :dtypes (:f32 :f64 :i64 :i32))
(def-unop "Abs" (x) :form (abs x) :dtypes (:f32 :f64 :i64 :i32))
(def-unop "Sqrt" (x) :form (if (minusp x) (/ (- x x) (- x x)) (sqrt x)))
(def-unop "Exp" (x) :form (exp x))
(def-unop "Log" (x) :form (log x))
(def-unop "Ceil" (x) :form (fceiling x))
(def-unop "Floor" (x) :form (ffloor x))
(def-unop "Tanh" (x) :form (tanh x))
(def-unop "Relu" (x) :form (max x (- x x)) :int-form (max x 0) :dtypes (:f32 :f64 :i64 :i32))
(def-unop "Erf" (x) :form (float (erf-d (float x 1d0)) x))
(def-unop "Sigmoid" (x) :form (float (sigmoid-d x) x))
(def-unop "Softplus" (x) :form (float (softplus-d x) x))
(def-unop "Not" (x) :form (if (zerop x) 1 0) :int-form (if (zerop x) 1 0) :dtypes (:bool))

(defop "LeakyRelu" (node ins)
  (let* ((a (first ins))
         (alpha (float (node-attr node "alpha" 0.01d0) 1.0f0))
         (res (make-tensor (tensor-dtype a) (tensor-shape a)))
         (da (the (simple-array single-float (*)) (tensor-data a)))
         (dr (the (simple-array single-float (*)) (tensor-data res))))
    (dotimes (i (length da))
      (let ((x (aref da i)))
        (setf (aref dr i) (if (minusp x) (* alpha x) x))))
    res))

(defop "Clip" (node ins)
  (destructuring-bind (a &optional lo hi) ins
    (let* ((res (make-tensor (tensor-dtype a) (tensor-shape a))))
      (with-two-typed-data (da a) (dr res)
        (let ((lo (and lo (plusp (tensor-size lo)) (tensor-scalar lo)))
              (hi (and hi (plusp (tensor-size hi)) (tensor-scalar hi))))
          (dotimes (i (length da))
            (let ((x (aref da i)))
              (when (and lo (< x lo)) (setf x lo))
              (when (and hi (> x hi)) (setf x hi))
              (setf (aref dr i) x)))))
      res)))
