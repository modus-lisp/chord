;;;; chord — a text-to-speech engine in pure Common Lisp.
;;;;
;;;; No FFI, no foreign libraries, no C.  The model is a VITS graph exported from
;;;; ONNX by tools/export-onnx.py; chord executes it node for node so that every
;;;; one of its 2755 nodes can be compared against onnxruntime's answer.

(asdf:defsystem #:chord
  :description "Neural text-to-speech in pure Common Lisp."
  :author "ynniv"
  :license "MIT"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "tensor")
                             (:file "parallel")
                             (:file "model")
                             (:file "graph")
                             (:file "ops-shape")
                             (:file "ops-math")
                             (:file "ops-reduce")
                             (:file "ops-index")
                             (:file "ops-nn")
                             (:file "speech")
                             ;; grapheme-to-phoneme: CMUdict (BSD) for the words
                             ;; English has written down, learned rules for the
                             ;; rest, and one table to the voice's own IPA
                             (:file "strings")
                             (:file "lts-data")
                             (:file "lts")
                             (:file "lexicon")
                             (:file "arpabet")
                             (:file "text")
                             (:file "g2p")))))
