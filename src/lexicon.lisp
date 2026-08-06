;;;; lexicon.lisp — the pronunciation dictionary: CMUdict, and the morphology that
;;;; stretches it to cover words it does not literally list.
;;;;
;;;; data/cmudict.dict is the CMU Pronouncing Dictionary (BSD, see
;;;; data/LICENSE.cmudict): 135,166 entries of ARPAbet with stress digits, one per
;;;; line, variants marked `word(2)'.  It is shipped rather than generated because
;;;; a dictionary IS the data — there is nothing to compile, and a build step that
;;;; downloads 4 MB is a worse dependency than 4 MB.
;;;;
;;;; The dictionary is the exception list AND the common case: it holds nearly
;;;; every word ordinary prose uses.  What it does not hold is inflections it
;;;; happens to omit, hyphenations, and names — which is why LOOKUP tries a short
;;;; ladder of morphological guesses before giving up and handing the word to the
;;;; letter-to-sound rules (src/lts.lisp).
;;;;
;;;; Each rung of that ladder is a claim about English that can be wrong, so each
;;;; one is spelled out below rather than folded into a regex.

(in-package #:chord)

(defvar *cmudict* nil
  "WORD (downcased string) -> list of pronunciations, each a list of ARPAbet
strings with the stress digit still attached (\"AH0\").  Loaded on first use.")

(defun cmudict-path ()
  (merge-pathnames "data/cmudict.dict" (asdf:system-source-directory '#:chord)))

(defun parse-cmudict-line (line)
  "One dictionary line as (WORD . PHONES), or NIL for a comment or a blank.

Variant entries are `word(2) ...'; the parenthesised index is dropped so variants
land on the same key, in file order — the first listed is the one we say."
  (let ((line (string-right-trim '(#\Space #\Tab #\Return) line)))
    (when (and (plusp (length line))
               (not (eql (char line 0) #\;)))
      ;; a trailing `# ...' comment is CMUdict's own annotation, not a phoneme
      (let* ((hash (search " #" line))
             (line (if hash (subseq line 0 hash) line))
             (space (position #\Space line)))
        (when space
          (let ((word (subseq line 0 space))
                (phones (remove "" (split-on #\Space (subseq line (1+ space)))
                                :test #'string=)))
            (let ((paren (position #\( word)))
              (when paren (setf word (subseq word 0 paren))))
            (when (and (plusp (length word)) phones)
              (cons word phones))))))))

(defun load-cmudict (&optional (path (cmudict-path)))
  "Read the dictionary into a hash table.  About 135k entries; a second or so,
paid once, on the first word anything asks to pronounce."
  (let ((table (make-hash-table :test #'equal :size 150000)))
    (with-open-file (in path :external-format :latin-1)
      (loop for line = (read-line in nil nil)
            while line
            do (let ((entry (parse-cmudict-line line)))
                 (when entry
                   (push (cdr entry) (gethash (car entry) table))))))
    ;; pushed, so each word's variants are reversed out of file order
    (maphash (lambda (k v) (setf (gethash k table) (nreverse v))) table)
    table))

(defun cmudict ()
  (or *cmudict* (setf *cmudict* (load-cmudict))))

(defun dict-entry (word)
  "WORD's first listed pronunciation, or NIL.  WORD is matched as CMUdict spells
it: lowercase, apostrophes kept (`don't'), periods kept (`u.s.')."
  (first (gethash (string-downcase word) (cmudict))))

;;; ---- morphology ------------------------------------------------------------
;;;
;;; CMUdict lists most inflections outright, so these rungs fire rarely — but when
;;; they fire they are the difference between a correct ending and a spelled-out
;;; guess.  Each returns phones or NIL, and each is a rule of English pronunciation
;;; rather than of spelling: the ending you add depends on the sound the stem ends
;;; in, not on the letter.

(defparameter +voiceless+ '("P" "T" "K" "F" "TH" "HH")
  "Consonants after which -s is said S and -ed is said T.")

(defparameter +sibilant+ '("S" "Z" "SH" "ZH" "CH" "JH")
  "Consonants after which -s becomes a syllable of its own, IH0 Z.")

(defun final-phone (phones)
  (car (last phones)))

(defun plural-of (stem-phones)
  "STEM + s: IH0 Z after a sibilant, S after a voiceless consonant, else Z."
  (let ((last (final-phone stem-phones)))
    (append stem-phones
            (cond ((member last +sibilant+ :test #'string=) '("IH0" "Z"))
                  ((member last +voiceless+ :test #'string=) '("S"))
                  (t '("Z"))))))

(defun past-of (stem-phones)
  "STEM + ed: IH0 D after T or D, T after a voiceless consonant, else D."
  (let ((last (final-phone stem-phones)))
    (append stem-phones
            (cond ((member last '("T" "D") :test #'string=) '("IH0" "D"))
                  ((member last +voiceless+ :test #'string=) '("T"))
                  (t '("D"))))))

(defun undouble (stem)
  "STEM with a doubled final consonant collapsed (`stopp' -> `stop'), or NIL.

English doubles the consonant before -ed/-ing to keep the vowel short, so the
stem in the dictionary is the undoubled one."
  (let ((n (length stem)))
    (when (and (> n 2)
               (eql (char stem (1- n)) (char stem (- n 2)))
               (not (find (char stem (1- n)) "aeiou")))
      (subseq stem 0 (1- n)))))

(defun stem-lookup (stem)
  "STEM's phones, trying the stem as spelled, then undoubled, then with a dropped
`e' restored (`stor' -> `store', for `storing')."
  (or (dict-entry stem)
      (let ((u (undouble stem))) (and u (dict-entry u)))
      (dict-entry (concatenate 'string stem "e"))))

(defun suffix-pronunciation (word)
  "WORD as a known stem plus a regular ending, or NIL.  Tried only after a direct
lookup fails, so a word the dictionary lists its own way (`houses', `learned')
is never reached by these rules."
  (let ((n (length word)))
    (flet ((ends (suffix)
             (let ((k (length suffix)))
               (and (> n k) (string= suffix word :start2 (- n k))
                    (subseq word 0 (- n k))))))
      (let (stem)
        (cond
          ;; -'s and -s' are possessives, said exactly like the plural
          ((setf stem (ends "'s"))
           (let ((p (dict-entry stem))) (and p (plural-of p))))
          ((setf stem (ends "s'"))
           (let ((p (dict-entry stem))) (and p (plural-of p))))
          ((setf stem (ends "es"))
           (let ((p (or (dict-entry stem) (stem-lookup stem))))
             (and p (plural-of p))))
          ((setf stem (ends "s"))
           (let ((p (dict-entry stem))) (and p (plural-of p))))
          ((setf stem (ends "ed"))
           (let ((p (stem-lookup stem))) (and p (past-of p))))
          ((setf stem (ends "ing"))
           (let ((p (stem-lookup stem))) (and p (append p '("IH0" "NG")))))
          ;; -ly on a stem that already ends in L does not double it: the
          ;; dictionary says `total' + `ly' is T OW1 T AH0 L IY0, not L L IY0,
          ;; and the same for finally, fully, really, usually, normally.
          ;; English has no geminate consonants across this boundary.
          ((setf stem (ends "ly"))
           (let ((p (dict-entry stem)))
             (and p (append p (if (string= (car (last p)) "L")
                                  '("IY0")
                                  '("L" "IY0"))))))
          (t nil))))))

(defun hyphenated-pronunciation (word)
  "A hyphenated or slashed compound said as its parts, or NIL if any part is
unknown — a compound is only as sayable as its pieces."
  (let ((parts (remove "" (split-on-any word "-/") :test #'string=)))
    (when (rest parts)
      (let ((each (mapcar #'word-phones parts)))
        (unless (some #'null each)
          (reduce #'append each))))))

(defun word-phones (word)
  "WORD as ARPAbet phones with stress digits, by whatever means: the dictionary,
then morphology, then the compound rule, then the letter-to-sound rules.

Returns NIL only for something with no letters in it at all — LTS-PHONES always
has an answer, which is the point of having it."
  (let ((word (string-downcase (string-trim "-'/" word))))
    (cond ((zerop (length word)) nil)
          ((dict-entry word))
          ((suffix-pronunciation word))
          ((hyphenated-pronunciation word))
          (t (lts-phones word)))))

(defun dictionary-coverage (words)
  "How many of WORDS the dictionary answers directly, as (HITS . TOTAL) — the
measurement that says how much the letter-to-sound rules actually matter."
  (let ((hits 0))
    (dolist (w words (cons hits (length words)))
      (when (dict-entry w) (incf hits)))))
