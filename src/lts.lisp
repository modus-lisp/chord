;;;; lts.lisp — letter to sound: what to say when the dictionary has never heard
;;;; of the word.
;;;;
;;;;   (lts-phones WORD) -> list of ARPAbet phones with stress digits
;;;;
;;;; WORD is lowercase letters (and possibly an apostrophe).  The answer is never
;;;; NIL for anything with a letter in it: an unknown word still has to be said
;;;; out loud, and a wrong guess is better than silence in the middle of a
;;;; sentence.  Whatever comes back has a vowel, and one of its vowels carries
;;;; primary stress.
;;;;
;;;; The rules were LEARNED FROM CMUdict — the same BSD dictionary the lexicon
;;;; reads — by tools/learn-lts.lisp, which wrote src/lts-data.lisp.  Nothing here
;;;; derives from another synthesiser's rule set.  This file never opens the
;;;; dictionary: it is the matcher, and everything it knows arrived as a table.
;;;;
;;;; ---------------------------------------------------------------------------
;;;; How it answers
;;;;
;;;; Each letter says a CHUNK: nothing, one phone, or two.  Which chunk depends on
;;;; the letter, its neighbours out to four either side, and the last two phones
;;;; already said — a chain of nested context windows, narrow to wide.  The
;;;; probability of a chunk is read off the widest window that was worth storing
;;;; and interpolated back down the chain, so an unseen context degrades into its
;;;; parent instead of falling off a cliff.
;;;;
;;;; That runs as a small beam search, because a letter's chunk changes what the
;;;; NEXT letter's context is and a locally-good choice can strand the rest of the
;;;; word.  The survivors are then re-ranked by a whole-word stress model: which
;;;; syllable is loud is a property of the entire word, mostly of its ending, and
;;;; no nine-letter window can see that.  Re-ranking, not rewriting — painting a
;;;; `1' onto a schwa gives a vowel nobody says, and measurably loses accuracy.
;;;;
;;;; ---------------------------------------------------------------------------
;;;; The tables, and how they are read
;;;;
;;;; src/lts-data.lisp holds them as text inside string literals, which the reader
;;;; swallows whole; a quarter of a million rules as list structure would be a
;;;; minute of compiling and a fasl to match.  They are unpacked into hash tables
;;;; the first time a word misses the dictionary, and not at all for text the
;;;; dictionary covers.
;;;;
;;;; Every value is one character of +LTS-SYMBOLS+ — base 70, chosen to contain
;;;; no quote and no backslash.  A level is a run of lines ended by a line holding
;;;; just `='.  Within a level the lines are sorted by key, and each begins with
;;;; one character saying how many leading key symbols it shares with the line
;;;; before, so only the tail of a key is spelled out; sorted deep contexts share
;;;; most of their length, which is worth about a third of the file.  After the
;;;; key come the alternatives: for a context rule, two characters of chunk id and
;;;; one of count; for a stress rule, the pattern itself (as many digits as the
;;;; key's syllable count, which is the key's first symbol) and one of count.
;;;;
;;;; Counts are rounded on a log scale — exact to twelve, then a sixth apart —
;;;; because only their ratios matter and they have to fit in one character.  The
;;;; learner scored itself on the rounded values, so its numbers and the gate's
;;;; are numbers about the same model.
;;;;
;;;; +LTS-CHAIN+ and +LTS-STRESS-SHAPE+ travel with the tables, so the matcher
;;;; derives its key layout from the data rather than from a constant that could
;;;; silently drift out of step with whatever last generated it.

(in-package #:chord)

;;; --- how the search is run --------------------------------------------------
;;; These are the settings the learner measured itself with, so changing them
;;; changes the answers.  Beam 8 with 4 branches is within a quarter of a point of
;;; a beam five times as wide.

(defparameter *lts-alpha* 2d0
  "Back-off weight: how much a context level trusts its parent.")
(defparameter *lts-beam* 8 "How many partial pronunciations stay alive.")
(defparameter *lts-branch* 4 "How many chunks one letter is allowed to try.")
(defparameter *lts-stress-weight* 1d0
  "How loudly the whole-word stress model may argue with the letter rules.")

;;; --- symbols ----------------------------------------------------------------
;;; Letters are 0..25, the apostrophe 26, and 27 is the word boundary padding both
;;; ends, so that "the first letter" and "the letter after a t" are the same kind
;;; of fact.  Phone-history symbols are offset by one so zero can mean "nothing
;;; said yet".  These are conventions shared with tools/learn-lts.lisp.

(defconstant +lts-boundary+ 27)

(declaim (inline lts-letter-code))
(defun lts-letter-code (ch)
  (cond ((char<= #\a ch #\z) (- (char-code ch) (char-code #\a)))
        ((char= ch #\') 26)
        (t nil)))

(defparameter +lts-symbols+
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&*+-"
  "One character per value, for keys, counts and chunk ids.")

(defparameter +lts-radix+ 70)

(defvar *lts-symbol-values*
  (let ((v (make-array 128 :element-type 'fixnum :initial-element -1)))
    (loop for i below (length +lts-symbols+)
          do (setf (aref v (char-code (char +lts-symbols+ i))) i))
    v)
  "Character code -> value: the inverse of +LTS-SYMBOLS+.")

(declaim (inline lts-symbol-value))
(defun lts-symbol-value (ch)
  (aref (the (simple-array fixnum (*)) *lts-symbol-values*) (char-code ch)))

(defconstant +lts-count-exact+ 12
  "Counts to here ship exactly; above it they are a sixth apart on a log scale.")

(defun lts-dequantise-count (q)
  (if (<= q +lts-count-exact+)
      q
      (round (* +lts-count-exact+ (expt 1.6d0 (- q +lts-count-exact+))))))

;;; --- the shape of the keys --------------------------------------------------
;;; A key is one integer: five bits per letter symbol, seven per phone-history
;;; symbol, outermost symbol first.  So a context's parent is the same number
;;; shifted right, which is what makes the back-off walk free.  The deepest
;;; context is fifty-nine bits, a fixnum everywhere it matters.

(defun lts-n-levels () (1+ (length +lts-chain+)))

(defun lts-level-bits (k)
  "How many bits the symbol added at level K needs.  Level 0 is the bare letter."
  (if (zerop k) 5 (if (eq (first (nth (1- k) +lts-chain+)) :prev) 7 5)))

(defun lts-context-pad ()
  "How far past either end of a word the letter window can reach."
  (loop for spec in +lts-chain+
        maximize (if (eq (first spec) :prev) 1 (second spec))))

(defun lts-stress-levels ()
  (destructuring-bind (suffix prefix max-syllables) +lts-stress-shape+
    (declare (ignore max-syllables))
    (+ 1 suffix prefix)))

;;; --- unpacking --------------------------------------------------------------

(defvar *lts-phone-strings* nil "Phone id -> ARPAbet string.")
(defvar *lts-chunk-phones* nil "Chunk id -> list of phone ids, possibly empty.")
(defvar *lts-context* nil "Level -> hash of key -> ((chunk id . count) ...).")
(defvar *lts-stress-model* nil "Level -> hash of key -> ((pattern . count) ...).")
(defvar *lts-loaded* nil)

(defun lts-read-level (text start width bits entries)
  "Read delta-coded lines from TEXT at START until a line holding just `='.  WIDTH
is how many symbols a key has at this level and BITS gives each of their widths.
ENTRIES is called with the key's symbols, the text, and the bounds of what follows
the key; whatever it returns is the value.  Answers the table and where the next
level starts."
  (let ((table (make-hash-table :test #'eql))
        (syms (make-array width :element-type 'fixnum :initial-element 0))
        (i start))
    (loop
      (let ((eol (or (position #\Newline text :start i) (length text))))
        (when (or (= eol i) (char= (char text i) #\=))
          (return (values table (1+ eol))))
        (let ((share (lts-symbol-value (char text i)))
              (j (1+ i)))
          (loop for k from share below width
                do (setf (aref syms k) (lts-symbol-value (char text j)))
                   (incf j))
          (let ((key 0))
            (dotimes (k width)
              (setf key (+ (ash key (funcall bits k)) (aref syms k))))
            (setf (gethash key table) (funcall entries syms text j eol))))
        (setf i (1+ eol))))))

(defun lts-read-chunk-entries (syms text start end)
  "Groups of three: two characters of chunk id, one of count."
  (declare (ignore syms))
  (let ((out '()))
    (loop for j from start below end by 3
          do (push (cons (+ (* +lts-radix+ (lts-symbol-value (char text j)))
                            (lts-symbol-value (char text (+ j 1))))
                         (lts-dequantise-count (lts-symbol-value (char text (+ j 2)))))
                   out))
    (nreverse out)))

(defun lts-read-stress-entries (syms text start end)
  "The key's first symbol is the syllable count, so the pattern's length is known
before it is read: that many digits, then one character of count."
  (let ((len (aref syms 0))
        (out '())
        (j start))
    (loop while (< j end)
          do (push (cons (subseq text j (+ j len))
                         (lts-dequantise-count (lts-symbol-value (char text (+ j len)))))
                   out)
             (incf j (1+ len)))
    (nreverse out)))

(defun lts-read-ids (line)
  "Two characters per id, space-separated or packed; blanks ignored."
  (loop for j from 0 below (length line) by 3
        collect (+ (* +lts-radix+ (lts-symbol-value (char line j)))
                   (lts-symbol-value (char line (1+ j))))))

(defun lts-unpack ()
  "Turn the shipped text into hash tables.  Everything is built into locals and
published at the end, so two threads racing here waste work rather than seeing
each other's half-built tables."
  (let* ((phones (coerce (split-on #\Space +lts-phones+) 'vector))
         ;; one line per chunk id, in order, and the empty line IS a chunk — the
         ;; letter that says nothing.  BUTLAST drops only the tail left by the
         ;; closing newline.
         (chunks (map 'vector #'lts-read-ids
                      (butlast (split-on #\Newline +lts-chunks+))))
         (context (make-array (lts-n-levels)))
         (stress (make-array (lts-stress-levels))))
    (let ((at 0))
      (dotimes (k (length context))
        (multiple-value-bind (table next)
            (lts-read-level +lts-rules+ at (1+ k) #'lts-level-bits
                            #'lts-read-chunk-entries)
          (setf (aref context k) table
                at next))))
    (let ((at 0))
      (dotimes (k (length stress))
        (multiple-value-bind (table next)
            (lts-read-level +lts-stress+ at (1+ k) (constantly 5)
                            #'lts-read-stress-entries)
          (setf (aref stress k) table
                at next))))
    (setf *lts-phone-strings* phones
          *lts-chunk-phones* chunks
          *lts-context* context
          *lts-stress-model* stress
          *lts-loaded* t)))

(defun ensure-lts-tables ()
  (unless *lts-loaded* (lts-unpack))
  t)

;;; --- context keys -----------------------------------------------------------

(defun lts-word-letters (word)
  "WORD as a vector of letter codes, dropping anything unmodelled."
  (let ((v (make-array (length word) :element-type 'fixnum :fill-pointer 0)))
    (loop for ch across word
          for code = (lts-letter-code ch)
          when code do (vector-push code v))
    v))

(defun lts-padded (letters pad)
  (let* ((n (length letters))
         (v (make-array (+ n (* 2 pad)) :element-type 'fixnum
                                        :initial-element +lts-boundary+)))
    (dotimes (i n v) (setf (aref v (+ i pad)) (aref letters i)))))

(defun lts-keys-for (padded pad i emitted keys)
  "Fill KEYS with the context key at every level for letter I.  EMITTED holds the
phone ids said so far, most recent last."
  (declare (type (simple-array fixnum (*)) padded) (type fixnum i pad)
           (optimize (speed 3) (safety 1)))
  (let ((key (aref padded (+ i pad)))
        (n (fill-pointer emitted)))
    (declare (type fixnum key n))
    (setf (aref keys 0) key)
    (loop for spec in +lts-chain+ for k of-type fixnum from 1
          do (let ((sym (ecase (first spec)
                          (:left  (aref padded (- (+ i pad) (the fixnum (second spec)))))
                          (:right (aref padded (+ i pad (the fixnum (second spec)))))
                          (:prev  (let ((j (- n (the fixnum (second spec)))))
                                    (if (minusp j) 0 (1+ (aref emitted j))))))))
               (declare (type fixnum sym))
               (setf key (+ (ash key (if (eq (first spec) :prev) 7 5)) sym))
               (setf (aref keys k) key)))
    keys))

(defun lts-interpolated (keys)
  "P(chunk | context) as an alist, backing off down the chain.  A level that was
pruned away is simply skipped, which is what interpolation does with an unseen
context anyway."
  (let ((dist '()))
    (dotimes (k (lts-n-levels) dist)
      (let ((alist (gethash (aref keys k) (aref *lts-context* k))))
        (when alist
          (let* ((total (reduce #'+ alist :key #'cdr :initial-value 0))
                 (a (* *lts-alpha* (length alist)))
                 (scale (/ (+ total a))))
            (setf dist (mapcar (lambda (cell) (cons (car cell) (* (cdr cell) a scale))) dist))
            (dolist (cell alist)
              (let ((have (assoc (car cell) dist)))
                (if have
                    (incf (cdr have) (* (cdr cell) scale))
                    (push (cons (car cell) (* (cdr cell) scale)) dist))))))))))

;;; --- the beam ---------------------------------------------------------------

(defstruct (lts-hyp (:constructor make-lts-hyp (score phones)))
  (score 0d0 :type double-float)
  (phones nil))                         ; phone ids, most recent FIRST

(defun lts-decode (letters)
  "The beam's best few readings of LETTERS, each as (score . phone ids)."
  (let* ((pad (lts-context-pad))
         (padded (lts-padded letters pad))
         (keys (make-array (lts-n-levels)))
         (scratch (make-array (+ 8 (* 2 (length letters)))
                              :element-type 'fixnum :fill-pointer 0))
         (beam (list (make-lts-hyp 0d0 '()))))
    (dotimes (i (length letters))
      (let ((next '()))
        (dolist (h beam)
          (setf (fill-pointer scratch) 0)
          (dolist (p (reverse (lts-hyp-phones h))) (vector-push p scratch))
          (let ((dist (sort (lts-interpolated (lts-keys-for padded pad i scratch keys))
                            #'> :key #'cdr)))
            (loop for cell in dist repeat *lts-branch*
                  do (push (make-lts-hyp
                            (+ (lts-hyp-score h) (log (max 1d-12 (cdr cell))))
                            (append (reverse (aref *lts-chunk-phones* (car cell)))
                                    (lts-hyp-phones h)))
                           next))))
        (setf beam (subseq (sort next #'> :key #'lts-hyp-score)
                           0 (min *lts-beam* (length next))))))
    (mapcar (lambda (h) (cons (lts-hyp-score h) (reverse (lts-hyp-phones h)))) beam)))

;;; --- stress ------------------------------------------------------------------

(declaim (inline lts-vowel-p))
(defun lts-vowel-p (phone)
  "ARPAbet marks a vowel by carrying a stress digit."
  (and (plusp (length phone))
       (char<= #\0 (char phone (1- (length phone))) #\9)))

(defun lts-stress-pattern (phones)
  (coerce (loop for p in phones when (lts-vowel-p p) collect (char p (1- (length p))))
          'string))

(defun lts-stress-key (nsyll word level)
  "Syllable count, then the last letters from the end inwards, then the first —
packed five bits apiece, outermost first, the same way the table was written."
  (destructuring-bind (suffix prefix max-syllables) +lts-stress-shape+
    (declare (ignore prefix))
    (let ((n (length word))
          (key (min nsyll max-syllables)))
      (loop for k from 1 to level
            do (setf key
                     (+ (ash key 5)
                        (if (<= k suffix)
                            (let ((i (- n k)))
                              (if (minusp i) +lts-boundary+ (lts-letter-code (char word i))))
                            (let ((i (- k suffix 1)))
                              (if (>= i n) +lts-boundary+ (lts-letter-code (char word i))))))))
      key)))

(defun lts-stress-logp (word pattern)
  "Interpolated P(PATTERN | syllable count, spelling): the re-ranker's whole-word
view of which syllable ought to be loud."
  (let ((nsyll (length pattern))
        (max-syllables (third +lts-stress-shape+)))
    (if (or (zerop nsyll) (> nsyll max-syllables))
        0d0
        (let ((p 1d-3))
          (dotimes (k (lts-stress-levels) (log (max 1d-9 p)))
            (let ((alist (gethash (lts-stress-key nsyll word k) (aref *lts-stress-model* k))))
              (when alist
                (let* ((total (reduce #'+ alist :key #'cdr :initial-value 0))
                       (a (* *lts-alpha* (length alist)))
                       (hit (or (cdr (assoc pattern alist :test #'string=)) 0)))
                  (setf p (/ (+ hit (* a p)) (+ total a)))))))))))

(defun lts-promote (phones pick)
  "Make the first vowel PICK likes the primary one, or answer NIL."
  (let ((done nil))
    (let ((out (mapcar (lambda (p)
                         (if (and (not done) (lts-vowel-p p) (funcall pick p))
                             (progn (setf done t)
                                    (concatenate 'string (subseq p 0 (1- (length p))) "1"))
                             p))
                       phones)))
      (and done out))))

(defun lts-ensure-stress (phones)
  "The contract: a non-empty answer has a vowel, and one of its vowels is primary."
  (cond ((null phones) nil)
        ((notany #'lts-vowel-p phones)
         ;; something we heard as nothing but consonants still has to be sayable
         (lts-ensure-stress (if (rest phones)
                                (list* (first phones) "AH1" (rest phones))
                                (append phones (list "AH1")))))
        ((find-if (lambda (p) (eql (char p (1- (length p))) #\1)) phones) phones)
        ;; the loudest thing available: a full vowel if there is one, else a schwa
        (t (or (lts-promote phones (lambda (p) (not (string= p "AH0"))))
               (lts-promote phones (constantly t))))))

;;; --- the contract ------------------------------------------------------------

(defun lts-phones (word)
  "WORD's phones, guessed from the learned rules.  Never NIL for anything with a
letter in it; NIL only for a string with no letters at all."
  (let* ((down (string-downcase word))
         (letters (lts-word-letters down)))
    (when (loop for c across letters thereis (< c 26))
      (ensure-lts-tables)
      (let ((clean (map 'string (lambda (c) (if (= c 26) #\' (code-char (+ c (char-code #\a)))))
                        letters))
            (best nil)
            (best-score -1d30))
        (dolist (h (lts-decode letters))
          (let* ((phones (mapcar (lambda (i) (aref *lts-phone-strings* i)) (cdr h)))
                 (score (+ (car h)
                           (* *lts-stress-weight*
                              (lts-stress-logp clean (lts-stress-pattern phones))))))
            (when (> score best-score)
              (setf best-score score best phones))))
        ;; a word every one of whose letters went silent still has to be said
        (lts-ensure-stress (or best (list "AH1")))))))
