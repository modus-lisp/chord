;;;; speech.lisp — the part that is about speech rather than about tensors.
;;;;
;;;; Everything above this file is a general ONNX interpreter.  This is the thin
;;;; layer that knows a VITS voice: how Piper turns phonemes into ids, what the
;;;; three scale knobs mean, and how to get a float waveform onto disk.
;;;;
;;;; Grapheme-to-phoneme is NOT here.  Turning English text into IPA is a
;;;; separate problem with a separate correctness standard, and bolting it on
;;;; before the numeric engine is proven would mean debugging both at once.  The
;;;; entry point takes phonemes.

(in-package #:chord)

(defstruct (voice (:constructor %make-voice))
  (model nil)
  (config nil)
  (sample-rate 22050 :type fixnum)
  (id-map (make-hash-table :test #'equal) :type hash-table)
  (noise-scale 0.667d0)
  (length-scale 1.0d0)
  (noise-w 0.8d0))

(defun load-voice (graph-path)
  "Load the voice exported to GRAPH-PATH.  The .bin and .config.json beside it
come along automatically."
  (let* ((model (load-model graph-path))
         (config (model-config model)))
    (unless config
      (error "no config next to ~a — chord needs the phoneme id map that ships ~
              with the voice" graph-path))
    (let ((audio (gethash "audio" config))
          (inference (gethash "inference" config)))
      (%make-voice
       :model model
       :config config
       :sample-rate (if audio (gethash "sample_rate" audio) 22050)
       :id-map (gethash "phoneme_id_map" config)
       :noise-scale (if inference (gethash "noise_scale" inference) 0.667d0)
       :length-scale (if inference (gethash "length_scale" inference) 1.0d0)
       :noise-w (if inference (gethash "noise_w" inference) 0.8d0)))))

;;; ---- phonemes to ids -------------------------------------------------------
;;;
;;; Piper's sequence is BOS, then every phoneme followed by a pad, then EOS.  The
;;; interleaved pads are not decoration: the model was trained with them, and the
;;; alignment it predicts is over this doubled sequence.

(defun phoneme-ids (voice phonemes)
  "A (1, N) i64 tensor of ids for PHONEMES, which may be a string or a list of
phoneme strings.  Signals an error naming any phoneme this voice does not know,
rather than dropping it — a silently missing phoneme is a mispronunciation with
no trace."
  (let* ((map (voice-id-map voice))
         (units (etypecase phonemes
                  (string (map 'list #'string phonemes))
                  (list (mapcar (lambda (p) (if (characterp p) (string p) p)) phonemes))))
         (missing (remove-if (lambda (p) (nth-value 1 (gethash p map))) units)))
    (when missing
      (error "this voice has no id for ~{~s~^, ~}" (remove-duplicates missing :test #'equal)))
    (flet ((ids-for (p) (coerce (gethash p map) 'list)))
      (let ((ids (append (ids-for "^")
                         (loop for p in units append (append (ids-for p) (ids-for "_")))
                         (ids-for "$"))))
        (tensor-from-list :i64 (vector 1 (length ids)) ids)))))

;;; ---- running it ------------------------------------------------------------

(defun synthesize (voice phonemes &key noise-scale length-scale noise-w)
  "Audio for PHONEMES as a single-float vector in [-1, 1], plus the sample rate.
The three scales default to the voice's own; passing 0 for the two noise scales
makes the run deterministic, which is what the fixtures were dumped with."
  (let* ((ids (phoneme-ids voice phonemes))
         (lengths (tensor-from-list :i64 (vector 1) (list (tensor-dim ids 1))))
         (scales (tensor-from-list
                  :f32 (vector 3)
                  (list (or noise-scale (voice-noise-scale voice))
                        (or length-scale (voice-length-scale voice))
                        (or noise-w (voice-noise-w voice)))))
         (vals (run-model (voice-model voice)
                          (list (cons "input" ids)
                                (cons "input_lengths" lengths)
                                (cons "scales" scales))))
         (out (model-output-tensor (voice-model voice) vals)))
    ;; the graph's output is (batch, time, 1, samples); this only ever runs one
    ;; utterance at a time, so the waveform is the whole thing flattened
    (values (tensor-data out) (voice-sample-rate voice))))

;;; ---- WAV -------------------------------------------------------------------

(defun write-le (stream value bytes)
  (dotimes (i bytes) (write-byte (ldb (byte 8 (* 8 i)) value) stream)))

(defun write-wav (path samples sample-rate)
  "Write SAMPLES (floats in [-1, 1]) to PATH as 16-bit mono PCM."
  (let* ((n (length samples))
         (data-bytes (* 2 n)))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (flet ((tag (string) (loop for c across string do (write-byte (char-code c) s))))
        (tag "RIFF") (write-le s (+ 36 data-bytes) 4) (tag "WAVE")
        (tag "fmt ") (write-le s 16 4)
        (write-le s 1 2)                ; PCM
        (write-le s 1 2)                ; mono
        (write-le s sample-rate 4)
        (write-le s (* sample-rate 2) 4) ; byte rate
        (write-le s 2 2)                ; block align
        (write-le s 16 2)               ; bits per sample
        (tag "data") (write-le s data-bytes 4)
        (loop for i below n
              do (let* ((x (max -1.0 (min 1.0 (float (aref samples i) 1.0f0))))
                        (q (round (* x 32767))))
                   (write-le s (logand q #xffff) 2)))))
    path))

(defun say (voice phonemes path &rest args)
  "Synthesize PHONEMES and write them to PATH.  Returns the path and the duration
in seconds."
  (multiple-value-bind (samples rate) (apply #'synthesize voice phonemes args)
    (write-wav path samples rate)
    (values path (/ (length samples) (float rate 1d0)))))
