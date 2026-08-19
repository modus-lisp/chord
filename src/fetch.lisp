;;;; fetch.lisp — get a voice, in Lisp.
;;;;
;;;; The weights are not in this repository and cannot be: the best-sounding Piper voices are
;;;; trained on corpora that forbid redistribution.  So a voice is fetched on first use — over
;;;; weft's pure-CL TLS, and read by mill's own ONNX reader, because the machine that has to do
;;;; this may be modus, where there is no curl to shell out to and no Python to convert with.
;;;;
;;;; What ships here is the CATALOGUE: which voices exist and, for each, the licence of the data it
;;;; was trained on.  That is the fact that is hard to find and easy to get wrong — a Piper model
;;;; card says "License: mit" while the dataset underneath it says research-only, and it is the
;;;; dataset that binds.

(in-package #:chord)

(defparameter *voice-catalogue*
  ;; name                          huggingface path                  data licence
  '(("en_US-lessac-medium"    "en/en_US/lessac/medium"    :research  "Blizzard 2013 Lessac — research only, NOT redistributable")
    ("en_US-lessac-high"      "en/en_US/lessac/high"      :research  "Blizzard 2013 Lessac — research only, NOT redistributable")
    ("en_US-ljspeech-medium"  "en/en_US/ljspeech/medium"  :free      "LJSpeech — public domain")
    ("en_US-ljspeech-high"    "en/en_US/ljspeech/high"    :free      "LJSpeech — public domain")
    ("en_US-libritts-high"    "en/en_US/libritts/high"    :attribute "LibriTTS — CC BY 4.0; 904 speakers, pass :SPEAKER")
    ("en_US-kristin-medium"   "en/en_US/kristin/medium"   :free      "LibriVox — public domain")
    ("en_US-norman-medium"    "en/en_US/norman/medium"    :free      "LibriVox — public domain")
    ("en_US-joe-medium"       "en/en_US/joe/medium"       :free      "CC0")
    ("en_US-mike-medium"      "en/en_US/mike/medium"      :free      "CC0")
    ("en_GB-cori-high"        "en/en_GB/cori/high"        :free      "LibriVox — public domain")
    ("en_GB-alba-medium"      "en/en_GB/alba/medium"      :attribute "CC BY 4.0"))
  "Voice -> where it lives, and what its TRAINING DATA permits.  :FREE is public domain or CC0,
:ATTRIBUTE needs credit, :RESEARCH may be used but not shipped.")

(defparameter *voice-base-url* "https://huggingface.co/rhasspy/piper-voices/resolve/main/")

(defun available-voices (&key (stream *standard-output*))
  "Print the catalogue, licence first, because that is what decides which of these you can use."
  (dolist (v *voice-catalogue*)
    (format stream "~&  ~:[ ~;*~]~a~30t~a~%"
            (string= (first v) *default-voice*) (first v) (fourth v)))
  (format stream "~&~%  * = current default.  (chord:fetch-voice \"name\") to install one.~%")
  (values))

(defun voice-licence (name)
  (let ((v (assoc name *voice-catalogue* :test #'string=)))
    (and v (values (third v) (fourth v)))))

(defun %fetch-to-file (url path)
  (let ((r (weft.fetch:fetch url)))
    (unless (= 200 (weft.fetch:response-status r))
      (error "chord: fetching ~a returned HTTP ~a" url (weft.fetch:response-status r)))
    (let ((body (weft.fetch:decompress-body (weft.fetch:response-headers r)
                                            (weft.fetch:response-body r))))
      (with-open-file (o path :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede :if-does-not-exist :create)
        (write-sequence body o))
      (length body))))

(defun fetch-voice (&optional (name *default-voice*) &key (dir *voice-dir*) force keep-onnx)
  "Download NAME into DIR and return the path chord will load it from.

Nothing is converted: mill reads the .onnx directly, so what lands on disk is exactly what upstream
published, and the file can be checked against upstream's hash.  Already-present voices are left
alone unless FORCE."
  (let* ((entry (assoc name *voice-catalogue* :test #'string=))
         (onnx (merge-pathnames (concatenate 'string name ".onnx") dir)))
    (unless entry
      (error "chord: unknown voice ~s.  (chord:available-voices) lists them." name))
    (let ((graph (merge-pathnames (concatenate 'string name ".graph") dir)))
      (when (and (probe-file graph) (not force))
        (return-from fetch-voice (namestring graph))))
    (ensure-directories-exist dir)
    (format *error-output* "~&chord: fetching ~a~%      licence: ~a~%" name (fourth entry))
    (when (eq (third entry) :research)
      (format *error-output*
              "~&      NOTE: this voice's data is licensed for research use and may not be~
               ~%      redistributed.  Running it is between you and that licence; do not ship~
               ~%      the weights or sell the audio.  A voice you CAN ship: en_US-ljspeech-medium.~%"))
    (finish-output *error-output*)
    (let* ((base (concatenate 'string *voice-base-url* (second entry) "/" name))
           (n1 (%fetch-to-file (concatenate 'string base ".onnx") onnx))
           (n2 (%fetch-to-file (concatenate 'string base ".onnx.json")
                               (merge-pathnames (concatenate 'string name ".onnx.json") dir))))
      (format *error-output* "~&chord: fetched ~:d bytes + ~:d config; converting~%" n1 n2)
      (finish-output *error-output*)
      ;; Convert ONCE, here.  Protobuf is how ONNX is published, not how a voice should be read on
      ;; every load: the .graph form is an s-expression plus little-endian floats and loads about
      ;; three times faster.  Keeping the ONNX reader is what lets this step happen anywhere,
      ;; including on a machine with no Python; it just does not belong on the hot path.
      (let ((graph (onnx->graph onnx dir)))
        ;; the config travels under the name the .graph loader looks for
        (let ((cfg (merge-pathnames (concatenate 'string name ".config.json") dir)))
          (with-open-file (in (merge-pathnames (concatenate 'string name ".onnx.json") dir)
                              :element-type '(unsigned-byte 8))
            (with-open-file (out cfg :direction :output :element-type '(unsigned-byte 8)
                                     :if-exists :supersede :if-does-not-exist :create)
              (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
                (read-sequence buf in) (write-sequence buf out)))))
        (when (and keep-onnx (not keep-onnx)) nil)
        (unless keep-onnx (ignore-errors (delete-file onnx)))
        (format *error-output* "~&chord: ~a ready -> ~a~%" name graph)
        (finish-output *error-output*)
        graph))))

(defun ensure-voice (&optional (name *default-voice*))
  "The voice NAME, fetching it first if it is not already here.  This is what makes a fresh
checkout work without a separate install step."
  (let ((path (voice-path name)))
    (if (probe-file path) (load-voice path) (load-voice (fetch-voice name)))))
