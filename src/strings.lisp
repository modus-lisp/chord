;;;; strings.lisp — the two string splits the front end needs, and nothing else.
;;;;
;;;; Kept apart from the code that uses them so that the lexicon, the letter-to-
;;;; sound rules and the text normaliser can all have them without one depending
;;;; on another.

(in-package #:chord)

(defun split-on (char string)
  "STRING split at every CHAR.  Empty fields are kept — callers that don't want
them remove them, which is rarer than it sounds."
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start pos)
        while pos
        do (setf start (1+ pos))))

(defun split-on-any (string chars)
  "STRING split at every character in CHARS."
  (loop with start = 0
        for pos = (position-if (lambda (c) (find c chars)) string :start start)
        collect (subseq string start pos)
        while pos
        do (setf start (1+ pos))))
