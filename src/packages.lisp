;;;; packages.lisp — chord, a text-to-speech engine in pure Common Lisp.
;;;;
;;;; The tensors, the ONNX graph interpreter and the ops it dispatches to are not
;;;; here — they are mill, which chord is one consumer of.  What is left in this
;;;; package is the part that is about speech: a VITS voice, and English text.
;;;;
;;;; mill's engine names are re-exported rather than hidden, because a caller
;;;; holding a voice sooner or later wants the tensor that came out of it.

(defpackage #:chord
  (:use #:cl #:mill)
  (:export
   ;; the engine, by way of mill
   #:tensor #:tensor-p #:make-tensor #:tensor-data #:tensor-shape #:tensor-dtype
   #:tensor-size #:tensor-rank #:tensor-dim #:tensor-ref #:tensor-from-list
   #:tensor-scalar #:tensor-integers
   #:tensor-equal-p #:tensor-max-abs-diff #:print-tensor
   #:*worker-count* #:shutdown-workers
   #:model #:load-model #:load-onnx #:onnx->graph #:model-nodes #:model-initializers #:model-inputs #:model-outputs
   #:model-config #:model-opset
   #:node #:node-op #:node-name #:node-inputs #:node-outputs #:node-attrs #:node-attr
   #:run-model #:model-output-tensor #:*op-table* #:defop #:op-implemented-p
   #:unimplemented-op #:model-missing-ops
   #:*noise-seed* #:reset-noise #:random-normal
   ;; speech
   #:voice #:load-voice #:fetch-voice #:ensure-voice #:available-voices #:voice-licence #:*voice-catalogue* #:voice-path #:*voice-dir* #:*default-voice* #:voice-sample-rate #:voice-model #:voice-id-map
   #:voice-noise-scale #:voice-length-scale #:voice-noise-w
   #:voice-config #:voice-speaker-count #:voice-multi-speaker-p
   #:phoneme-ids #:synthesize #:say #:write-wav
   ;; grapheme-to-phoneme
   #:text-to-ipa #:word-to-ipa #:word-phones #:arpabet-to-ipa
   #:normalize-text #:lts-phones #:dictionary-coverage
   #:arpabet-covers-inventory-p))
