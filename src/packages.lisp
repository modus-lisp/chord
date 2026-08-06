;;;; packages.lisp — chord, a text-to-speech engine in pure Common Lisp.
(defpackage #:chord
  (:use #:cl)
  (:export
   ;; tensors
   #:tensor #:tensor-p #:make-tensor #:tensor-data #:tensor-shape #:tensor-dtype
   #:tensor-size #:tensor-rank #:tensor-dim #:tensor-ref #:tensor-from-list
   #:tensor-scalar #:tensor-integers
   #:tensor-equal-p #:tensor-max-abs-diff #:print-tensor
   ;; how many cores the kernels use, and how to give them back
   #:*worker-count* #:shutdown-workers
   ;; models
   #:model #:load-model #:model-nodes #:model-initializers #:model-inputs #:model-outputs
   #:model-config #:model-opset
   #:node #:node-op #:node-name #:node-inputs #:node-outputs #:node-attrs #:node-attr
   ;; execution
   #:run-model #:model-output-tensor #:*op-table* #:defop #:op-implemented-p
   #:unimplemented-op #:model-missing-ops
   #:*noise-seed* #:reset-noise #:random-normal
   ;; speech
   #:voice #:load-voice #:voice-sample-rate #:voice-model #:voice-id-map
   #:voice-noise-scale #:voice-length-scale #:voice-noise-w
   #:phoneme-ids #:synthesize #:say #:write-wav
   ;; grapheme-to-phoneme
   #:text-to-ipa #:word-to-ipa #:word-phones #:arpabet-to-ipa
   #:normalize-text #:lts-phones #:dictionary-coverage
   #:arpabet-covers-inventory-p))
