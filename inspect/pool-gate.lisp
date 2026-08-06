;;;; pool-gate.lisp — does recycling dead data vectors change any answer?
;;;;
;;;;     sbcl --dynamic-space-size 8192 --load inspect/pool-gate.lisp
;;;;
;;;; RUN-MODEL hands a value's data vector back to *TENSOR-POOL* once the last
;;;; node that reads it has run, and MAKE-TENSOR takes from there rather than
;;;; asking for fresh pages.  That is only sound if nothing still refers to the
;;;; vector, and the thing that makes it easy to get wrong is that RESHAPE-VIEW
;;;; and the identity-shaped ops give the same vector a second name — so a value
;;;; can be dead under one name and live under another.
;;;;
;;;; inspect/node-gate.lisp cannot see any of this: it runs with KEEP-ALL, which
;;;; is exactly the mode where nothing is ever dropped and the pool stays empty.
;;;; So this gate runs the same graph twice in one process, once with the pool
;;;; budget set to zero (every allocation fresh, which is the old behaviour) and
;;;; once with it on, and compares the two waveforms bit for bit.  A reused
;;;; vector that was still live shows up as a changed sample; there is no
;;;; tolerance here, because there is nothing about reuse that should perturb
;;;; arithmetic at all.
;;;;
;;;; The two Gaussian draws are scaled by values passed in, so setting those
;;;; scales to zero makes the graph a pure function of the phonemes — otherwise
;;;; the two runs would differ for a reason that has nothing to do with the pool.

(require :asdf)
(asdf:load-system :chord)

(in-package #:chord)

(defparameter *graph* "/mnt/lisp/chord/export/en_US-lessac-medium.graph")

(defparameter *texts*
  '("Hello world, this is a test."
    "The quick brown dog jumps over five lazy foxes, on March 3rd, 2026."
    "Numbers: 1, 22, 333. Dr. Smith paid $4.50."
    "She read the record aloud, then read it again."))

(defun samples-of (voice ipa budget)
  "One synthesis with the pool allowed BUDGET bytes.  Zero disables it: a dead
vector is dropped on the floor and every MAKE-TENSOR allocates."
  (let ((*tensor-pool-budget* budget))
    (synthesize voice ipa)))

(defun first-difference (a b)
  "The index of the first sample that differs, or NIL."
  (cond ((/= (length a) (length b)) :length)
        (t (dotimes (i (length a) nil)
             (unless (= (aref a i) (aref b i)) (return i))))))

(defun main ()
  (let ((voice (load-voice *graph*))
        (failures '()))
    (setf (voice-noise-scale voice) 0.0d0
          (voice-noise-w voice) 0.0d0)
    (dolist (text *texts*)
      (let* ((ipa (first (text-to-ipa text)))
             ;; pooled first, so the pool is warm before the run it is compared
             ;; against — a bug that only bites on a reused vector needs there to
             ;; have been a reuse
             (pooled (samples-of voice ipa (* 48 1024 1024)))
             (fresh (samples-of voice ipa 0))
             (again (samples-of voice ipa (* 48 1024 1024)))
             (d1 (first-difference fresh pooled))
             (d2 (first-difference fresh again)))
        (format t "~&~:[ok  ~;BAD ~]~6d samples  ~s~%" (or d1 d2) (length fresh) text)
        (when d1 (push (format nil "~s — pooled run differs at ~a" text d1) failures))
        (when d2 (push (format nil "~s — second pooled run differs at ~a" text d2)
                       failures))))
    (finish-output)
    (cond ((null failures)
           (format t "~%GATE GREEN — the pool changes no sample of any run~%"))
          (t
           (format t "~%GATE RED~%")
           (dolist (f (reverse failures)) (format t "  ~a~%" f))))
    (sb-ext:exit :code (if failures 1 0))))

(main)
