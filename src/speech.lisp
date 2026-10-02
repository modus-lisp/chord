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
  (noise-w 0.8d0)
  ;; Kokoro only: the directory of per-voice style tables, and the one to use by default.
  (styles nil)
  (style-name nil))

(defparameter *voice-dir*
  (or (uiop:getenv "CHORD_VOICE_DIR")
      (namestring (merge-pathnames ".chord/voices/" (user-homedir-pathname))))
  "Where FETCH-VOICE puts voices, and where LOAD-VOICE looks when given no path.")

(defparameter *default-voice*
  (or (uiop:getenv "CHORD_VOICE") "en_US-lessac-medium")
  "The voice LOAD-VOICE uses when called with no argument.  $CHORD_VOICE overrides.

lessac by default because it is the best-sounding voice Piper publishes and this is a speech
engine, not a licence.  Its TRAINING DATA is research-only and not redistributable, which is a
constraint on shipping it, not on running it — see the README.  Set this to en_US-ljspeech-medium
for a public-domain voice if what you make needs to travel.  kokoro-v1.0 loads too, when asked
for by name.")

(defun voice-path (&optional (name *default-voice*))
  "Where NAME lives.  A path is taken as given; a bare name prefers the exporter's .graph if one is
there and falls back to the .onnx, since either loads."
  (if (or (find #\/ name) (string-equal "onnx" (pathname-type name))
          (string-equal "graph" (pathname-type name)))
      name
      (let ((graph (merge-pathnames (concatenate 'string name ".graph") *voice-dir*))
            (onnx (merge-pathnames (concatenate 'string name ".onnx") *voice-dir*)))
        (namestring (if (probe-file graph) graph onnx)))))

(defun load-voice (&optional (graph-path (voice-path)))
  "Load the voice exported to GRAPH-PATH.  The .bin and .config.json beside it
come along automatically.  With no argument, loads *DEFAULT-VOICE* from *VOICE-DIR*.

Weights are not part of this repository — the good Piper voices are trained on corpora that forbid
redistribution — so a missing voice is a normal first-run state, and the error says how to fix it
rather than complaining about a file."
  (unless (probe-file graph-path)
    (error "no voice at ~a.~%~%  Fetch one:  (chord:fetch-voice ~s)~%  Or list them: (chord:available-voices)~%"
           graph-path *default-voice*))
  ;; A .onnx is read directly, in Lisp; a .graph is the exporter's pre-chewed form.  Same MODEL
  ;; either way — the exporter is now an optimization, not a dependency, which is what lets this
  ;; run somewhere with no Python.
  (let* ((model (if (string-equal "onnx" (pathname-type graph-path))
                    (load-onnx graph-path)
                    (load-model graph-path)))
         (config (model-config model)))
    (unless config
      (error "no config next to ~a — chord needs the phoneme id map that ships ~
              with the voice" graph-path))
    (let ((audio (gethash "audio" config))
          (inference (gethash "inference" config))
          (kokoro (gethash "kokoro" config)))
      (%make-voice
       :styles (when kokoro
                 (merge-pathnames (concatenate 'string (pathname-name graph-path) ".voices/")
                                  (directory-namestring (truename graph-path))))
       :style-name (when kokoro
                     (or (sb-ext:posix-getenv "CHORD_KOKORO_VOICE") (gethash "default_voice" kokoro)))
       :model model
       :config config
       :sample-rate (if audio (gethash "sample_rate" audio) 22050)
       :id-map (or (gethash "phoneme_id_map" config) (gethash "vocab" config))
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

(defun voice-speaker-count (voice)
  "How many speakers this voice holds.  1 for an ordinary single-speaker voice."
  (or (ignore-errors (gethash "num_speakers" (voice-config voice))) 1))

(defun voice-multi-speaker-p (voice)
  "True when the graph wants a SID input — i.e. the weights hold more than one voice."
  (and (find "sid" (model-inputs (voice-model voice)) :key #'first :test #'string=) t))

;;; ---- Kokoro ----------------------------------------------------------------
;;;
;;; A different graph with the same job: (tokens, style, speed) -> samples.  Tokens are the
;;; espeak-style IPA characters looked up in the model's own vocab, each one an id with no pads
;;; between them, a 0 at both ends; characters the vocab lacks are dropped, as kokoro-onnx does.
;;; STYLE is a 256-vector chosen by how many tokens there are, from a table of 510 per voice,
;;; exported once from the voices file as raw little-endian f32 (<name>.voices/<voice>.f32).

(defun kokoro-style (voice name count)
  (let ((path (merge-pathnames (concatenate 'string name ".f32") (voice-styles voice))))
    (unless (probe-file path)
      (error "kokoro: no voice ~s (looked for ~a)" name path))
    (with-open-file (s path :element-type '(unsigned-byte 8))
      (file-position s (* 1024 (min count 509)))
      (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
        (read-sequence buf s)
        (loop for i below 256
              collect (let ((u (logior (aref buf (* 4 i)) (ash (aref buf (+ 1 (* 4 i))) 8)
                                       (ash (aref buf (+ 2 (* 4 i))) 16)
                                       (ash (aref buf (+ 3 (* 4 i))) 24))))
                        (sb-kernel:make-single-float (if (logbitp 31 u) (- u (ash 1 32)) u))))))))

(defun kokoro-synthesize (voice phonemes speaker length-scale)
  (let* ((text (etypecase phonemes
                 (string phonemes)
                 (list (format nil "~{~a~}" phonemes))))
         (vocab (voice-id-map voice))
         (ids (loop for c across text
                    for id = (gethash (string c) vocab)
                    when id collect (if (vectorp id) (aref id 0) id)))
         (ids (subseq ids 0 (min (length ids) 510)))
         (n (length ids))
         (name (if (stringp speaker) speaker (voice-style-name voice)))
         (all (append (list 0) ids (list 0)))
         (vals (run-model (voice-model voice)
                          (list (cons "tokens" (tensor-from-list :i64 (vector 1 (length all)) all))
                                (cons "style" (tensor-from-list :f32 (vector 1 256)
                                                                (kokoro-style voice name n)))
                                (cons "speed" (tensor-from-list
                                               :f32 (vector 1)
                                               (list (/ 1.0 (or length-scale 1.0))))))))
         (out (model-output-tensor (voice-model voice) vals)))
    (values (tensor-data out) (voice-sample-rate voice))))

(defun synthesize (voice phonemes &key noise-scale length-scale noise-w (speaker 0))
  "Audio for PHONEMES as a single-float vector in [-1, 1], plus the sample rate.
The three scales default to the voice's own; passing 0 for the two noise scales
makes the run deterministic, which is what the fixtures were dumped with.

SPEAKER selects among the voices in a multi-speaker model — LibriTTS carries 904 of
them in one graph, which is the difference between a corpus you may redistribute and
one you may not.  A single-speaker graph declares no SID input and ignores it."
  (when (voice-styles voice)
    (return-from synthesize (kokoro-synthesize voice phonemes speaker length-scale)))
  (let* ((ids (phoneme-ids voice phonemes))
         (lengths (tensor-from-list :i64 (vector 1) (list (tensor-dim ids 1))))
         (scales (tensor-from-list
                  :f32 (vector 3)
                  (list (or noise-scale (voice-noise-scale voice))
                        (or length-scale (voice-length-scale voice))
                        (or noise-w (voice-noise-w voice)))))
         (n (voice-speaker-count voice))
         (vals (run-model (voice-model voice)
                          (append (list (cons "input" ids)
                                        (cons "input_lengths" lengths)
                                        (cons "scales" scales))
                                  ;; keyed off what the GRAPH declares, not off the config:
                                  ;; the config is metadata, the input list is the contract
                                  (when (voice-multi-speaker-p voice)
                                    (unless (< -1 speaker n)
                                      (error "speaker ~a out of range: this voice has ~a" speaker n))
                                    (list (cons "sid" (tensor-from-list :i64 (vector 1)
                                                                        (list speaker))))))))
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
