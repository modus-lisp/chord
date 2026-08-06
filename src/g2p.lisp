;;;; g2p.lisp — text in, IPA out: the entry point everything else here serves.
;;;;
;;;; The pipeline is four steps, each its own file and each measurable on its own:
;;;;
;;;;   text.lisp     text -> clauses of sayable words (numbers spelled out,
;;;;                 abbreviations expanded, punctuation turned into breaks)
;;;;   lexicon.lisp  word -> ARPAbet, by dictionary then morphology
;;;;   lts.lisp      word -> ARPAbet when the dictionary has never heard of it
;;;;   arpabet.lisp  ARPAbet -> the IPA the voice was trained on
;;;;
;;;; Words are rendered one at a time and joined with spaces.  That means no
;;;; cross-word context — the linking r of `far away', the way `the' changes
;;;; before a vowel — which is a real limit and a visible one: it shows up as
;;;; slightly careful, word-by-word diction rather than as a wrong word.
;;;;
;;;; The other known limit is homographs.  CMUdict lists `read', `live', `lead'
;;;; and `record' twice, with no way to tell which is meant; we take the first
;;;; listing every time, so half of those sentences are wrong and nothing here
;;;; can notice.  Choosing between them needs a part-of-speech tag — a different
;;;; problem, and one worth solving only after the rest is right.

(in-package #:chord)

(defun word-to-ipa (word)
  "One word as IPA, or \"\" if there is nothing in it to say."
  (let ((phones (word-phones word)))
    (if phones (arpabet-to-ipa phones) "")))

(defun clause-to-ipa (clause)
  "One clause — already normalised to sayable words — as IPA."
  (let ((parts (remove "" (mapcar #'word-to-ipa
                                  (remove "" (split-on #\Space clause)
                                          :test #'string=))
                       :test #'string=)))
    (format nil "~{~a~^ ~}" parts)))

(defun text-to-ipa (text)
  "TEXT as IPA, one string per sentence.

The list is the unit of synthesis: a caller says the first sentence while the
second is still being turned into audio, which is why this returns sentences
rather than one long string."
  (remove "" (mapcar #'clause-to-ipa (normalize-text text)) :test #'string=))
