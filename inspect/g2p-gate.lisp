;;;; g2p-gate.lisp — what the front end is measured by, end to end.
;;;;
;;;;   sbcl --non-interactive --load inspect/g2p-gate.lisp
;;;;
;;;; Four different kinds of check, and it is worth being clear which is which:
;;;;
;;;;   MECHANICAL   every IPA symbol we can emit exists in the voice — a fact,
;;;;                checkable, and fatal when false (synthesis errors out).
;;;;   MEASURED     how much of real prose the dictionary answers directly, and
;;;;                how well the learned rules do on words held out of them.
;;;;   ASSERTED     a short list of words whose US pronunciation is not in
;;;;                dispute.  This is the only check on the ARPAbet->IPA table's
;;;;                CHOICES, and it is weaker than it looks: it can only catch a
;;;;                symbol that is wrong, never one that is merely unidiomatic.
;;;;   SHAPE        text arrives as sentences, nothing empty, nothing dropped.
;;;;
;;;; There is deliberately no reference synthesiser here.  The dictionary is the
;;;; authority for words it lists; for the rest the authority is held-out entries
;;;; from that same dictionary; and for the sound of it, a person listening.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :chord)))

(defpackage #:chord-g2p-gate (:use #:cl))
(in-package #:chord-g2p-gate)

(defvar *pass* 0)
(defvar *fail* 0)

(defun check (ok label &optional detail)
  (if ok (incf *pass*) (incf *fail*))
  (format t "  ~:[FAIL~;ok  ~] ~a~@[ — ~a~]~%" ok label detail))

(defun section (title) (format t "~&-- ~a ~a~%" title (make-string (max 0 (- 60 (length title))) :initial-element #\-)))

;;; ---- mechanical ------------------------------------------------------------

(section "the voice knows every symbol we can say")

(defvar *voice-path*
  (or (sb-posix:getenv "GLASS_VOICE")
      "/mnt/lisp/chord/export/en_US-lessac-medium.graph"))

(defvar *voice* (and (probe-file *voice-path*) (chord:load-voice *voice-path*)))

(if *voice*
    (let ((result (chord:arpabet-covers-inventory-p *voice*)))
      (check (eq result t) "every IPA symbol the table emits is in the voice"
             (if (eq result t)
                 (format nil "~d symbols" (length (chord::arpabet-ipa-symbols)))
                 (format nil "missing ~s" result))))
    (format t "  --   no voice at ~a — skipping the inventory check~%" *voice-path*))

;;; ---- asserted --------------------------------------------------------------

(section "words whose pronunciation is not in dispute")

;; `hello' is hə- and not hɛ-: CMUdict's first listing is HH AH0 L OW1, the
;; reduced first syllable of ordinary speech.  It lists HH EH0 L OW1 second, and
;; taking the dictionary's own order over a tidier-looking answer is the whole
;; policy — the alternative is a thousand small opinions with nothing behind them.
(dolist (case '(("hello"  "həlˈoʊ")
                ("father" "fˈɑːðɚ")
                ("bird"   "bˈɜːd")
                ("butter" "bˈʌtɚ")
                ("happy"  "hˈapi")
                ("thought" "θˈɔːt")
                ("measure" "mˈɛʒɚ")
                ("church" "tʃˈɜːtʃ")
                ("boy"    "bˈɔɪ")
                ("cow"    "kˈaʊ")))
  (destructuring-bind (word want) case
    (let ((got (chord:word-to-ipa word)))
      (check (string= got want) (format nil "~a" word)
             (if (string= got want) got (format nil "~s, wanted ~s" got want))))))

;;; ---- measured: what the dictionary covers ----------------------------------

(section "how much of ordinary prose the dictionary answers")

(defparameter *prose*
  "The quick brown fox jumps over the lazy dog. She sells sea shells by the sea
shore, and the shells she sells are surely seashells. Whether the weather is
cold or whether the weather is hot, we weather the weather whatever the weather,
whether we like it or not. A desktop that can speak is a desktop that can tell
you what it is doing while you are looking somewhere else, which is most of the
time, and that is the whole argument for having a voice at all.")

(let* ((words (remove "" (chord::split-on-any (string-downcase *prose*)
                                              " .,;:!?" )
                      :test #'string=))
       (coverage (chord:dictionary-coverage words)))
  (check (>= (/ (car coverage) (cdr coverage)) 9/10)
         "the dictionary answers at least 90% of prose words directly"
         (format nil "~d of ~d" (car coverage) (cdr coverage))))

;;; ---- measured: morphology --------------------------------------------------

(section "endings the dictionary does not list are still regular")

;; These four are genuinely absent from CMUdict (checked, not assumed) while their
;; stems are present, so each one exercises the morphology and not the lookup —
;; testing `dogs' would prove nothing, since the dictionary lists it outright.
;; The ending is chosen by the SOUND the stem ends in: podcast ends voiceless, so
;; -s is S; hyperlink ends voiced, so -ed is D.
(dolist (case '(("podcasts"       ("P" "AO1" "D" "K" "AE2" "S" "T" "S"))
                ("hyperlinked"    ("HH" "AY1" "P" "ER0" "L" "IH0" "NG" "K" "T"))
                ("onboarding"     ("AA1" "N" "B" "AO2" "R" "D" "IH0" "NG"))
                ;; one L, not two: `instrumental' already ends in L, and the
                ;; dictionary's own -ly words (totally, finally, normally) do
                ;; not double it.
                ("instrumentally" ("IH2" "N" "S" "T" "R" "AH0" "M" "EH1" "N" "T"
                                   "AH0" "L" "IY0"))))
  (destructuring-bind (word want) case
    (let ((got (chord:word-phones word)))
      (check (equal got want) (format nil "~a" word)
             (format nil "~{~a~^ ~}~@[ (wanted ~{~a~^ ~})~]"
                     got (unless (equal got want) want))))))

;;; ---- shape -----------------------------------------------------------------

(section "text arrives as sentences")

(let ((out (chord:text-to-ipa "Hello there. The desktop can speak now!")))
  (check (= 2 (length out)) "two sentences in, two strings out"
         (format nil "~s" out))
  (check (notany (lambda (s) (zerop (length s))) out) "no empty sentence"))

(let ((out (chord:text-to-ipa "")))
  (check (null out) "nothing in, nothing out"))

;;; ---- unknown words ---------------------------------------------------------

(section "a word the dictionary has never heard of still gets said")

(dolist (word '("zorblatt" "frimbulous" "kwyjibo"))
  (let ((ipa (chord:word-to-ipa word)))
    (check (plusp (length ipa)) (format nil "~a" word) ipa)))

;;; ---- robustness ------------------------------------------------------------

(section "text nobody meant to be said out loud")

;; A desktop's voice is handed whatever is on the screen — a URL, a stack trace,
;; a line of Lisp, an emoji.  None of it has to sound good.  All of it has to
;; come back without signalling, because the caller is a speech thread and a
;; condition there is a sentence that never gets spoken.
(dolist (case '(("a URL"          "See https://example.com/a?b=1&c=2#frag for more.")
                ("code"           "(let ((x 3)) (+ x 4)) ; => 7")
                ("emoji"          "shipped it 🚀🚀 — 100% done ✅")
                ("accents"        "naïve café résumé Schrödinger")
                ("a long word"    "pneumonoultramicroscopicsilicovolcanoconiosis")
                ("only symbols"   "!!! ??? ***")
                ("only spaces"    "     ")
                ("one letter"     "a")
                ("a stack trace"  "at foo.bar.Baz.qux(Baz.java:42)")))
  (destructuring-bind (label text) case
    (multiple-value-bind (out err)
        (handler-case (values (chord:text-to-ipa text) nil)
          (serious-condition (e) (values nil e)))
      (check (null err) (format nil "~a comes back without signalling" label)
             (if err (princ-to-string err)
                 (format nil "~d sentence~:p" (length out)))))))

(format t "~&~%chord g2p gate: ~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
