;;;; profile.lisp — where does a synthesis run actually spend its time?
;;;;
;;;;     sbcl --dynamic-space-size 8192 --load inspect/profile.lisp
;;;;     CHORD_PROFILE_TEXT="some sentence" sbcl ... --load inspect/profile.lisp
;;;;
;;;; Two views, because they answer different questions:
;;;;
;;;;   by op type   — which ONNX ops to rewrite.  Measured through RUN-MODEL's
;;;;                  ON-NODE hook, so the runner needs no profiling support and
;;;;                  the numbers are of the same code the gate validates.
;;;;   by function  — whether the time inside an op is arithmetic or overhead
;;;;                  (boxing, GETHASH, consing).  sb-sprof, statistical.
;;;;
;;;; The per-node deltas are wall clock and include whatever the runner does
;;;; between nodes (input lookup, output binding, dropping dead values); that is
;;;; deliberate, since it is real time spent and someone has to own it.  The
;;;; residual line at the bottom is the part no node claimed.

(require :asdf)
(require :sb-sprof)
(asdf:load-system :chord)

(in-package #:chord)

(defparameter *graph* "/mnt/lisp/chord/export/en_US-lessac-medium.graph")

(defparameter *text* (or (sb-posix:getenv "CHORD_PROFILE_TEXT")
                         "Hello world, this is a test."))

(defstruct (opstat (:conc-name os-))
  (op "" :type string)
  (count 0 :type fixnum)
  (ticks 0 :type integer)
  (elems 0 :type integer))       ; output elements produced, a crude size proxy

(defun profile-by-op (voice ipa)
  "Total wall time per ONNX op type for one synthesis."
  (let* ((stats (make-hash-table :test #'equal))
         (ids (phoneme-ids voice ipa))
         (lengths (tensor-from-list :i64 (vector 1) (list (tensor-dim ids 1))))
         (scales (tensor-from-list :f32 (vector 3)
                                   (list (voice-noise-scale voice)
                                         (voice-length-scale voice)
                                         (voice-noise-w voice))))
         (last (get-internal-real-time))
         (t0 (get-internal-real-time)))
    (run-model (voice-model voice)
               (list (cons "input" ids)
                     (cons "input_lengths" lengths)
                     (cons "scales" scales))
               :on-node
               (lambda (i node outs)
                 (declare (ignore i))
                 (let* ((now (get-internal-real-time))
                        (s (or (gethash (node-op node) stats)
                               (setf (gethash (node-op node) stats)
                                     (make-opstat :op (node-op node))))))
                   (incf (os-count s))
                   (incf (os-ticks s) (- now last))
                   (dolist (o outs)
                     (when (tensor-p o) (incf (os-elems s) (tensor-size o))))
                   (setf last now))))
    (values stats (- (get-internal-real-time) t0))))

(defun report-by-op (stats total-ticks)
  (let* ((k internal-time-units-per-second)
         (ranked (sort (loop for s being the hash-values of stats collect s)
                       #'> :key #'os-ticks))
         (claimed (reduce #'+ ranked :key #'os-ticks :initial-value 0)))
    (format t "~&~%time by ONNX op (total ~,2f s over ~d nodes)~%"
            (/ total-ticks k) (reduce #'+ ranked :key #'os-count :initial-value 0))
    (format t "~&~16a ~7a ~9a ~7a ~9a ~a~%"
            "op" "nodes" "seconds" "share" "us/node" "Melem out")
    (format t "~a~%" (make-string 66 :initial-element #\-))
    (loop for s in ranked
          while (> (os-ticks s) 0)
          do (format t "~16a ~7d ~9,3f ~6,1f% ~7,0f ~9,1f~%"
                     (os-op s) (os-count s) (/ (os-ticks s) k)
                     (* 100 (/ (os-ticks s) (max 1 total-ticks)))
                     (* 1d6 (/ (os-ticks s) k (max 1 (os-count s))))
                     (/ (os-elems s) 1d6)))
    (format t "~a~%" (make-string 66 :initial-element #\-))
    (let ((zero (remove-if (lambda (s) (> (os-ticks s) 0)) ranked)))
      (when zero
        (format t "~16a ~7d ~9,3f  (below timer resolution)~%" "everything else"
                (reduce #'+ zero :key #'os-count :initial-value 0) 0.0)))
    (format t "~16a ~7a ~9,3f ~6,1f%~%" "unclaimed" ""
            (/ (- total-ticks claimed) k)
            (* 100 (/ (- total-ticks claimed) (max 1 total-ticks))))))

(defmacro timing (label elapsed &body body)
  "Run BODY, print how long it took, add that to ELAPSED, and return its value."
  (let ((s (gensym)) (v (gensym)) (d (gensym)))
    `(let* ((,s (get-internal-real-time))
            (,v (progn ,@body))
            (,d (/ (- (get-internal-real-time) ,s)
                   (float internal-time-units-per-second 1d0))))
       (incf ,elapsed ,d)
       (format t "~&  ~14a ~7,3f s~%" ,label ,d)
       ,v)))

(defun end-to-end ()
  "The headline number: seconds of speech produced per second of work.  Under 1.0
means the voice cannot keep up with itself, which is what rules out live use.
LOAD-VOICE is left out of the factor — it is paid once per process, not per
utterance — but it is still printed, because a Pi has to pay it at startup."
  (format t "~&~%end to end~%")
  (let ((work 0d0) (load 0d0))
    (let* ((voice (timing "load-voice" load (load-voice *graph*)))
           (ipa (timing "g2p" work (first (text-to-ipa *text*))))
           (samples (timing "synthesize" work (synthesize voice ipa)))
           (rate (voice-sample-rate voice))
           (audio (/ (length samples) (float rate 1d0))))
      (timing "wav" work (write-wav "/tmp/chord-profile.wav" samples rate))
      (format t "~&  ~14a ~7,3f s~%" "audio produced" audio)
      (format t "~&  ~14a ~7,3fx realtime~%" "==>" (/ audio work))
      (values voice ipa))))

(defun profile-run ()
  (multiple-value-bind (voice ipa) (end-to-end)
    (format t "~&text ~s~&ipa  ~s~%" *text* ipa)
    ;; -- view 1: by op type
    (multiple-value-bind (stats ticks) (profile-by-op voice ipa)
      (report-by-op stats ticks))
    ;; -- view 2: by function
    (format t "~&~%~%time by function (sb-sprof, one synthesis):~%")
    (sb-sprof:with-profiling (:max-samples 40000 :sample-interval 0.001
                              :report :flat :mode :cpu)
      (synthesize voice ipa))))

(profile-run)
