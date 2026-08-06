;;;; learn-lts.lisp — learn chord's letter-to-sound rules from CMUdict.
;;;;
;;;;   sbcl --non-interactive --load tools/learn-lts.lisp
;;;;
;;;; Writes src/lts-data.lisp.  Nothing in here runs at speech time: this is the
;;;; compiler, src/lts.lisp is the interpreter, and src/lts-data.lisp is the
;;;; object code between them.
;;;;
;;;; The only input is data/cmudict.dict.  There is no other pronunciation source
;;;; anywhere in this file, by design — chord has no GPL lineage and the whole
;;;; reason this program exists is to keep it that way.  Where an oracle would be
;;;; convenient the answer is held-out entries from the same dictionary.
;;;;
;;;; ---------------------------------------------------------------------------
;;;; What is learned, in three stages
;;;;
;;;; 1. ALIGNMENT.  A dictionary line pairs a spelling with a pronunciation but
;;;;    says nothing about which letter made which sound, and the correspondence
;;;;    is not one to one: `x' is two phones, the `e' of `make' is none, `ph' is
;;;;    one phone spread over two letters.  So each letter is allowed to emit a
;;;;    CHUNK of zero, one or two phones, and the letter->chunk probabilities are
;;;;    fitted by expectation-maximisation over every entry at once — forward-
;;;;    backward across all the ways a word could be carved up, which needs no
;;;;    hand-made seed alignment and no list of digraphs.  Viterbi then commits
;;;;    each word to its single best carving.  (Re-aligning afterwards against the
;;;;    context model of stage 2 was tried and moved nothing: EM's carving is
;;;;    already the one the context model would have chosen.)
;;;;
;;;; 2. CONTEXT.  With letters and chunks lined up, the question becomes: given
;;;;    this letter, its neighbours, and the sounds already said, what does it
;;;;    say?  Counts are taken over a chain of NESTED windows, each one symbol
;;;;    wider than the last, and the answer at speech time is an interpolated
;;;;    back-off down that chain.  The table is stored as a DIFFERENCE LIST: a
;;;;    wider window is recorded only where it disagrees with what the narrower
;;;;    ones already say, or where it has been seen often enough to sharpen them.
;;;;    Most of English is regular, so most wide contexts have nothing to add and
;;;;    cost nothing to store; 3.9 million windows shrink to about a quarter of a
;;;;    million for a point and a half of accuracy.
;;;;
;;;; 3. STRESS.  Which syllable is loud is a different problem from which sounds
;;;;    are in it — it is a property of the whole word, mostly of its ending, and
;;;;    a nine-letter window cannot see the whole word.  It also cannot be fixed
;;;;    up afterwards: writing a `1' onto a schwa gives a vowel no English speaker
;;;;    would say, and doing that measurably LOSES accuracy.  So stress is a
;;;;    RE-RANKER instead: the search keeps its best few candidates, each of them
;;;;    internally consistent, and the stress model chooses among them.  That is
;;;;    worth four and a half points of whole-word accuracy where overwriting the
;;;;    digits was worth minus one.
;;;;
;;;; Every tenth entry is held back — one twentieth to be the gate's test slice,
;;;; one twentieth a dev slice for the tuning done here.  Neither is trained on.

(defpackage #:chord-learn-lts
  (:use #:cl)
  (:export #:learn))

(in-package #:chord-learn-lts)

(defparameter *root*
  (merge-pathnames "../" (make-pathname :name nil :type nil
                                        :defaults (or *load-truename*
                                                      *default-pathname-defaults*)))
  "The chord checkout this tool belongs to.")

(defun repo-file (relative) (merge-pathnames relative *root*))

(defmacro note (fmt &rest args)
  `(progn (format t ,(concatenate 'string "~&" fmt "~%") ,@args)
          (finish-output)))

;;; ---------------------------------------------------------------------------
;;; The dictionary, and the split
;;;
;;; What is trained on has to be reproducible by inspect/lts-gate.lisp without
;;; running this tool, so the rule is stated in terms of nothing but the file:
;;; read the lines in order; keep entries whose word is letters and apostrophes;
;;; keep only the first spelling of any word (CMUdict's `word(2)' entries are
;;; alternative pronunciations, and the lexicon says the first); number what
;;; survives from zero.  Index mod 20 = 0 is test, mod 20 = 1 is dev, rest trains.

(defun letters-only-p (word)
  (and (plusp (length word))
       (every (lambda (c) (or (char<= #\a c #\z) (eql c #\'))) word)))

(defun split-spaces (string)
  (let ((out '()) (start 0))
    (loop for pos = (position #\Space string :start start)
          do (let ((piece (subseq string start (or pos (length string)))))
               (when (plusp (length piece)) (push piece out)))
             (if pos (setf start (1+ pos)) (return)))
    (nreverse out)))

(defun parse-line (line)
  "One CMUdict line as (WORD . PHONES), or NIL."
  (let* ((line (string-right-trim '(#\Space #\Tab #\Return) line))
         (hash (search " #" line))
         (line (if hash (subseq line 0 hash) line))
         (space (position #\Space line)))
    (when (and space (plusp (length line)) (not (eql (char line 0) #\;)))
      (let ((word (subseq line 0 space))
            (phones (split-spaces (subseq line (1+ space)))))
        (let ((paren (position #\( word)))
          (when paren (setf word (subseq word 0 paren))))
        (when (and (letters-only-p word) phones)
          (cons word phones))))))

(defun read-entries (&optional (path (repo-file "data/cmudict.dict")))
  "The dictionary as a vector of (WORD . PHONES), first spelling of each word, in
file order.  This vector's indices ARE the split."
  (let ((seen (make-hash-table :test #'equal))
        (out (make-array 0 :adjustable t :fill-pointer t)))
    (with-open-file (in path :external-format :latin-1)
      (loop for line = (read-line in nil nil)
            while line
            do (let ((e (parse-line line)))
                 (when (and e (not (gethash (car e) seen)))
                   (setf (gethash (car e) seen) t)
                   (vector-push-extend e out)))))
    out))

(defun split-role (index)
  (case (mod index 20) (0 :test) (1 :dev) (t :train)))

;;; ---------------------------------------------------------------------------
;;; Symbols
;;;
;;; Letters are 0..25, the apostrophe 26, and 27 is the word boundary that pads
;;; both ends, so that `the first letter' and `the letter after a t' are the same
;;; kind of fact.  Phone-history symbols share the same numbering space, offset by
;;; one so that 0 can mean `nothing said yet'.

(defconstant +boundary+ 27)
(defconstant +n-letter-symbols+ 28)

(declaim (inline letter-code))
(defun letter-code (ch)
  (cond ((char<= #\a ch #\z) (- (char-code ch) (char-code #\a)))
        ((eql ch #\') 26)
        (t nil)))

(defvar *phones* nil "id -> phone string, e.g. \"AH0\".")
(defvar *phone-ids* nil "phone string -> id.")
(defvar *n-phones* 0)
(defvar *n-chunks* 0)

(defun intern-phones (entries)
  (setf *phone-ids* (make-hash-table :test #'equal)
        *phones* (make-array 0 :adjustable t :fill-pointer t))
  (loop for e across entries
        do (dolist (p (cdr e))
             (unless (gethash p *phone-ids*)
               (setf (gethash p *phone-ids*) (fill-pointer *phones*))
               (vector-push-extend p *phones*))))
  (length *phones*))

;;; A chunk is what one letter says: nothing, one phone, or two.  Numbered so the
;;; id is arithmetic rather than a lookup — 0 is silence, 1..NP one phone, then
;;; the pairs.

(declaim (inline chunk-single chunk-pair))
(defun chunk-single (p) (the fixnum (1+ p)))
(defun chunk-pair (p q)
  (the fixnum (+ 1 *n-phones* (the fixnum (* p *n-phones*)) q)))

(defun chunk-phones (c)
  "Chunk id -> list of phone ids."
  (cond ((zerop c) '())
        ((<= c *n-phones*) (list (1- c)))
        (t (let ((k (- c 1 *n-phones*)))
             (list (floor k *n-phones*) (mod k *n-phones*))))))

(defun chunk-strings (c)
  (mapcar (lambda (p) (aref *phones* p)) (chunk-phones c)))

;;; ---------------------------------------------------------------------------
;;; Stage 1 — expectation-maximisation alignment
;;;
;;; For a word of N letters and M phones, the alignments are the monotone paths
;;; through an (N+1) x (M+1) grid where each step advances one letter and zero,
;;; one or two phones.  Forward-backward sums the probability of every such path
;;; and gives each step its share of the credit; the M step renormalises
;;; P(chunk | letter).  Twelve passes is past where the alignments stop moving.

(deftype dvec () '(simple-array double-float (*)))
(deftype fvec () '(simple-array fixnum (*)))

(defvar *prob* nil "letter*n-chunks + chunk -> probability.")

(defstruct (sample (:constructor make-sample (word letters phones)))
  word
  (letters nil :type fvec)
  (phones nil :type fvec)
  (chunks nil))

(defun word-letters (word)
  "WORD as a vector of letter codes, or NIL if it has a character we do not model."
  (let ((v (make-array (length word) :element-type 'fixnum)))
    (loop for i below (length word)
          for c = (letter-code (char word i))
          do (if c (setf (aref v i) c) (return-from word-letters nil)))
    v))

(defun word-phone-ids (phones)
  (let ((v (make-array (length phones) :element-type 'fixnum)))
    (loop for i from 0 for p in phones do (setf (aref v i) (gethash p *phone-ids*)))
    v))

(defun initialise-probabilities (samples)
  "A co-occurrence prior: a letter starts out most likely to say the phones that
turn up in the words it turns up in.  EM gets there from uniform too, just more
slowly and with more chances to settle somewhere silly."
  (let ((co (make-array (* +n-letter-symbols+ *n-phones*)
                        :element-type 'double-float :initial-element 0d0)))
    (dolist (s samples)
      (let* ((ls (sample-letters s)) (ps (sample-phones s))
             (w (/ 1d0 (* (length ls) (length ps)))))
        (loop for i below (length ls)
              do (loop for j below (length ps)
                       do (incf (aref co (+ (* (aref ls i) *n-phones*) (aref ps j))) w)))))
    ;; normalise per letter, then spread the mass: 0.70 to single phones, 0.20 to
    ;; pairs (as the product of the singles), 0.10 to saying nothing at all.
    (let ((prob (make-array (* +n-letter-symbols+ *n-chunks*)
                            :element-type 'double-float :initial-element 0d0)))
      (dotimes (l +n-letter-symbols+)
        (let ((total (loop for p below *n-phones* sum (aref co (+ (* l *n-phones*) p))))
              (base (* l *n-chunks*)))
          (when (zerop total) (setf total 1d0))
          (setf (aref prob base) 0.10d0)
          (dotimes (p *n-phones*)
            (let ((q (/ (aref co (+ (* l *n-phones*) p)) total)))
              (setf (aref prob (+ base (chunk-single p))) (max 1d-9 (* 0.70d0 q)))
              (dotimes (r *n-phones*)
                (setf (aref prob (+ base (chunk-pair p r)))
                      (max 1d-12 (* 0.20d0 q (/ (aref co (+ (* l *n-phones*) r)) total)))))))))
      (setf *prob* prob))))

(defun alignable-p (s)
  (and (<= (length (sample-phones s)) (* 2 (length (sample-letters s))))
       (<= (length (sample-letters s)) 63)
       (<= (length (sample-phones s)) 63)))

(defun em-pass (samples counts)
  "One forward-backward pass over SAMPLES, adding expected chunk counts to COUNTS.
Returns the total log likelihood."
  (declare (type dvec counts) (optimize (speed 3) (safety 0)))
  (let ((ll 0d0)
        (prob *prob*)
        (nc *n-chunks*)
        (np *n-phones*)
        (alpha (make-array 4096 :element-type 'double-float))
        (beta (make-array 4096 :element-type 'double-float)))
    (declare (type double-float ll) (type fixnum nc np)
             (type dvec prob alpha beta))
    (macrolet ((p1 (l c) `(aref prob (+ (* ,l nc) ,c)))
               (cs (j) `(the fixnum (1+ (aref ps ,j))))
               (cp (j) `(the fixnum (+ 1 np (the fixnum (* (aref ps ,j) np))
                                       (aref ps (1+ ,j))))))
      (dolist (s samples ll)
        (let* ((ls (sample-letters s)) (ps (sample-phones s))
               (n (length ls)) (m (length ps)) (w (1+ m)))
          (declare (type fvec ls ps) (type fixnum n m w))
          (fill alpha 0d0 :end (* (1+ n) w))
          (setf (aref alpha 0) 1d0)
          (dotimes (i n)
            (declare (type fixnum i))
            (let ((l (aref ls i)) (base (* i w)) (next (* (1+ i) w)))
              (declare (type fixnum l base next))
              (dotimes (j w)
                (declare (type fixnum j))
                (let ((a (aref alpha (+ base j))))
                  (declare (type double-float a))
                  (unless (zerop a)
                    (incf (aref alpha (+ next j)) (* a (p1 l 0)))
                    (when (< j m)
                      (incf (aref alpha (+ next j 1)) (* a (p1 l (cs j)))))
                    (when (< (1+ j) m)
                      (incf (aref alpha (+ next j 2)) (* a (p1 l (cp j))))))))))
          (let ((total (aref alpha (+ (* n w) m))))
            (declare (type double-float total))
            (when (> total 0d0)
              (incf ll (log total))
              (fill beta 0d0 :end (* (1+ n) w))
              (setf (aref beta (+ (* n w) m)) 1d0)
              (loop for i of-type fixnum from (1- n) downto 0
                    do (let ((l (aref ls i)) (base (* i w)) (next (* (1+ i) w)))
                         (declare (type fixnum l base next))
                         (dotimes (j w)
                           (declare (type fixnum j))
                           (let ((b (* (p1 l 0) (aref beta (+ next j)))))
                             (declare (type double-float b))
                             (when (< j m)
                               (incf b (* (p1 l (cs j)) (aref beta (+ next j 1)))))
                             (when (< (1+ j) m)
                               (incf b (* (p1 l (cp j)) (aref beta (+ next j 2)))))
                             (setf (aref beta (+ base j)) b)))))
              (dotimes (i n)
                (declare (type fixnum i))
                (let ((l (aref ls i)) (base (* i w)) (next (* (1+ i) w)))
                  (declare (type fixnum l base next))
                  (dotimes (j w)
                    (declare (type fixnum j))
                    (let ((a (aref alpha (+ base j))))
                      (declare (type double-float a))
                      (unless (zerop a)
                        (let ((share (/ a total)))
                          (declare (type double-float share))
                          (incf (aref counts (* l nc))
                                (* share (p1 l 0) (aref beta (+ next j))))
                          (when (< j m)
                            (let ((c (cs j)))
                              (incf (aref counts (+ (* l nc) c))
                                    (* share (p1 l c) (aref beta (+ next j 1))))))
                          (when (< (1+ j) m)
                            (let ((c (cp j)))
                              (incf (aref counts (+ (* l nc) c))
                                    (* share (p1 l c) (aref beta (+ next j 2)))))))))))))))))))

(defun run-em (samples &key (iterations 12))
  (let ((usable (remove-if-not #'alignable-p samples)))
    (note "  aligning ~:d of ~:d entries (~:d have more phones than two per letter)"
          (length usable) (length samples) (- (length samples) (length usable)))
    (initialise-probabilities usable)
    (dotimes (it iterations)
      (let ((counts (make-array (* +n-letter-symbols+ *n-chunks*)
                                :element-type 'double-float :initial-element 0d0))
            (start (get-internal-real-time)))
        (let ((ll (em-pass usable counts)))
          (dotimes (l +n-letter-symbols+)
            (let ((total (loop for c below *n-chunks*
                               sum (aref counts (+ (* l *n-chunks*) c))))
                  (base (* l *n-chunks*)))
              (if (zerop total)
                  (dotimes (c *n-chunks*) (setf (aref *prob* (+ base c)) (/ 1d0 *n-chunks*)))
                  (dotimes (c *n-chunks*)
                    ;; a floor, so a chunk that vanished this pass can come back
                    (setf (aref *prob* (+ base c))
                          (max 1d-14 (/ (aref counts (+ base c)) total)))))))
          (note "  em ~2d  log-likelihood/word ~,4f  (~,1fs)"
                (1+ it) (/ ll (length usable))
                (/ (- (get-internal-real-time) start)
                   internal-time-units-per-second)))))
    usable))

(defun viterbi-align (s)
  "The single best carving of S's phones among its letters, as a list of chunk ids
one per letter, or NIL if there is no path at all."
  (let* ((ls (sample-letters s)) (ps (sample-phones s))
         (n (length ls)) (m (length ps)) (w (1+ m))
         (prob *prob*) (nc *n-chunks*)
         (best (make-array (* (1+ n) w) :element-type 'double-float :initial-element -1d30))
         (back (make-array (* (1+ n) w) :element-type 'fixnum :initial-element -1)))
    (setf (aref best 0) 0d0)
    (dotimes (i n)
      (let ((l (aref ls i)) (base (* i w)) (next (* (1+ i) w)))
        (dotimes (j w)
          (let ((a (aref best (+ base j))))
            (when (> a -1d29)
              (flet ((try (dj chunk)
                       (let ((score (+ a (log (max 1d-300 (aref prob (+ (* l nc) chunk)))))))
                         (when (> score (aref best (+ next j dj)))
                           (setf (aref best (+ next j dj)) score
                                 (aref back (+ next j dj)) chunk)))))
                (try 0 0)
                (when (< j m) (try 1 (chunk-single (aref ps j))))
                (when (< (1+ j) m) (try 2 (chunk-pair (aref ps j) (aref ps (1+ j)))))))))))
    (when (> (aref best (+ (* n w) m)) -1d29)
      (let ((chunks '()) (j m))
        (loop for i from n downto 1
              do (let ((c (aref back (+ (* i w) j))))
                   (push c chunks)
                   (decf j (length (chunk-phones c)))))
        chunks))))

;;; ---------------------------------------------------------------------------
;;; Stage 2 — the context chain
;;;
;;; The chain starts at the bare letter and adds one symbol per level, alternating
;;; sides and folding in the last two phones already said.  It has to be NESTED —
;;; every level a sub-context of the next — because that is what lets a level's own
;;; key answer `and what would the level below have said?'.  The order was chosen
;;; by measurement on the dev slice: the first phone of history is worth more than
;;; a third letter of context, a fifth letter either side is worth about a third of
;;; a point, and nothing past that pays for its size.
;;;
;;; Keys are packed so that the parent key is always the child shifted right, which
;;; makes both the back-off walk and the difference list cheap.

(defparameter +chain+
  '((:prev 1) (:right 1) (:left 1) (:right 2) (:left 2)
    (:prev 2) (:right 3) (:left 3) (:right 4) (:left 4))
  "One entry per level above the bare letter.  (:LEFT n) and (:RIGHT n) add the
letter n places away; (:PREV n) adds the n-th most recent phone already said.")

(defconstant +context-pad+ 4 "how far the letter window can reach past either end")

(defun n-levels () (1+ (length +chain+)))
(defun level-bits (k) (if (eq (car (nth (1- k) +chain+)) :prev) 7 5))

(defun padded-letters (letters)
  (let* ((n (length letters))
         (v (make-array (+ n (* 2 +context-pad+))
                        :element-type 'fixnum :initial-element +boundary+)))
    (dotimes (i n) (setf (aref v (+ i +context-pad+)) (aref letters i)))
    v))

(defun keys-for (padded i emitted keys)
  "Fill KEYS with the context key at every level for letter I.  EMITTED is the
phone ids said so far, most recent last."
  (declare (type fvec padded) (type fixnum i) (optimize (speed 3)))
  (let ((key (aref padded (+ i +context-pad+)))
        (n (fill-pointer emitted)))
    (declare (type fixnum key n))
    (setf (aref keys 0) key)
    (loop for spec in +chain+ for k of-type fixnum from 1
          do (let ((sym (ecase (car spec)
                          (:left  (aref padded (- (+ i +context-pad+) (the fixnum (second spec)))))
                          (:right (aref padded (+ i +context-pad+ (the fixnum (second spec)))))
                          (:prev  (let ((j (- n (the fixnum (second spec)))))
                                    (if (minusp j) 0 (1+ (aref emitted j))))))))
               (declare (type fixnum sym))
               (setf key (+ (ash key (if (eq (car spec) :prev) 7 5)) sym))
               (setf (aref keys k) key)))
    keys))

(defun tally-contexts (samples)
  "TABLES[level] : key -> ((chunk . count) ...), over every aligned letter."
  (let ((tables (make-array (n-levels)))
        (keys (make-array (n-levels))))
    (dotimes (k (n-levels)) (setf (aref tables k) (make-hash-table :test #'eql)))
    (dolist (s samples tables)
      (let ((padded (padded-letters (sample-letters s)))
            (emitted (make-array 128 :element-type 'fixnum :fill-pointer 0)))
        (loop for i from 0 for c in (sample-chunks s)
              do (keys-for padded i emitted keys)
                 (dotimes (k (n-levels))
                   (let* ((key (aref keys k))
                          (cell (assoc c (gethash key (aref tables k)))))
                     (if cell
                         (incf (the fixnum (cdr cell)))
                         (push (cons c 1) (gethash key (aref tables k))))))
                 (dolist (p (chunk-phones c)) (vector-push p emitted)))))))

;;; --- counts, quantised ------------------------------------------------------
;;;
;;; Counts are shipped one character each, so they are rounded on a log scale:
;;; exact up to twelve, then a sixth apart.  Only their ratios matter, and the
;;; learner measures itself on the rounded values so that the dev numbers here and
;;; the test numbers in the gate are numbers about the same model.

(defconstant +count-exact+ 12)
(defconstant +count-max-code+ 35)

(defun quantise-count (n)
  (if (<= n +count-exact+)
      n
      (min +count-max-code+
           (+ +count-exact+ (floor (log (/ n (float +count-exact+ 1d0))) (log 1.6d0))))))

(defun dequantise-count (q)
  (if (<= q +count-exact+) q (round (* +count-exact+ (expt 1.6d0 (- q +count-exact+))))))

(defun round-trip-count (n) (dequantise-count (quantise-count n)))

(defun count-code (n)
  "The character value that ships a count already put through ROUND-TRIP-COUNT.
Not QUANTISE-COUNT again: rounding a value back onto its own scale can land a
notch low, which would ship a model half a step away from the one measured here."
  (if (<= n +count-exact+)
      n
      (loop for q from (1+ +count-exact+) to +count-max-code+
            when (= (dequantise-count q) n) return q
            finally (return +count-max-code+))))

(defun top-chunks (alist n)
  (subseq (sort (copy-list alist) #'> :key #'cdr) 0 (min n (length alist))))

(defun difference-list (tables &key (keep-count 5) (top 2))
  "MODEL[level] : key -> ((chunk . count) ...), keeping a level only where it says
something its ancestors do not, or where it has been seen KEEP-COUNT times and so
is worth its weight in sharpening them.  Counts come back quantised, which is how
they will ship."
  (let ((model (make-array (n-levels))) (kept 0))
    (dotimes (k (n-levels)) (setf (aref model k) (make-hash-table :test #'eql)))
    (flet ((store (k key alist)
             (setf (gethash key (aref model k))
                   (mapcar (lambda (cell) (cons (car cell) (round-trip-count (cdr cell))))
                           (top-chunks alist top)))
             (incf kept)))
      (maphash (lambda (key alist) (store 0 key alist)) (aref tables 0))
      (labels ((predict (key level)
                 (loop for k of-type fixnum from level downto 0
                       do (let ((a (gethash key (aref model k))))
                            (when a (return-from predict (car (first a))))
                            (when (plusp k) (setf key (ash key (- (level-bits k))))))))
               (best (alist)
                 (let ((b nil) (bn -1))
                   (dolist (cell alist b)
                     (when (> (cdr cell) bn) (setf bn (cdr cell) b (car cell)))))))
        (loop for k from 1 below (n-levels)
              do (maphash (lambda (key alist)
                            (let ((total (reduce #'+ alist :key #'cdr :initial-value 0)))
                              (when (or (>= total keep-count)
                                        (not (eql (best alist)
                                                  (predict (ash key (- (level-bits k))) (1- k)))))
                                (store k key alist))))
                          (aref tables k)))))
    (values model kept)))

;;; ---------------------------------------------------------------------------
;;; Stage 3 — the stress model
;;;
;;; The pattern is the stress digits of a word's vowels in order: `AH0 K AE1 D' is
;;; "01".  What predicts it is how many syllables there are and — mostly — how the
;;; word ends, with a little from how it begins.  Keys are nested the same way as
;;; the context chain: syllable count first, then the last letter, the one before
;;; it, and so on out to seven, then the first letter and the second.
;;;
;;; Words are padded with the boundary symbol so every key at a level is the same
;;; length; that is what makes `cat' and `scat' different five-letter endings
;;; rather than the same short one.

(defconstant +stress-suffix+ 7)
(defconstant +stress-prefix+ 2)
(defconstant +max-syllables+ 15)

(defun n-stress-levels () (+ 1 +stress-suffix+ +stress-prefix+))

(defun stress-key (nsyll word level)
  "LEVEL's key as a vector of symbols: syllable count, then ending letters from
the last inwards, then the first letters."
  (let* ((n (length word))
         (key (make-array (1+ level) :element-type 'fixnum)))
    (setf (aref key 0) (min nsyll +max-syllables+))
    (loop for k from 1 to level
          do (setf (aref key k)
                   (if (<= k +stress-suffix+)
                       (let ((i (- n k))) (if (minusp i) +boundary+ (letter-code (char word i))))
                       (let ((i (- k +stress-suffix+ 1)))
                         (if (>= i n) +boundary+ (letter-code (char word i)))))))
    key))

(declaim (inline vowel-phone-p))
(defun vowel-phone-p (phone)
  (char<= #\0 (char phone (1- (length phone))) #\9))

(defun stress-pattern (phones)
  (coerce (loop for p in phones
                when (vowel-phone-p p) collect (char p (1- (length p))))
          'string))

(defun tally-stress (samples)
  (let ((tables (make-array (n-stress-levels))))
    (dotimes (k (n-stress-levels)) (setf (aref tables k) (make-hash-table :test #'equalp)))
    (dolist (s samples tables)
      (let* ((phones (map 'list (lambda (i) (aref *phones* i)) (sample-phones s)))
             (pattern (stress-pattern phones))
             (nsyll (length pattern)))
        (when (and (plusp nsyll) (<= nsyll +max-syllables+))
          (dotimes (k (n-stress-levels))
            (let* ((key (stress-key nsyll (sample-word s) k))
                   (cell (assoc pattern (gethash key (aref tables k)) :test #'string=)))
              (if cell (incf (cdr cell)) (push (cons pattern 1) (gethash key (aref tables k)))))))))))

(defun stress-difference-list (tables &key (keep-count 2) (top 3))
  (let ((model (make-array (n-stress-levels))) (kept 0))
    (dotimes (k (n-stress-levels)) (setf (aref model k) (make-hash-table :test #'equalp)))
    (flet ((store (k key alist)
             (setf (gethash key (aref model k))
                   (mapcar (lambda (cell) (cons (car cell) (round-trip-count (cdr cell))))
                           (top-chunks alist top)))
             (incf kept)))
      (maphash (lambda (key alist) (store 0 key alist)) (aref tables 0))
      (labels ((predict (key level)
                 (loop for k of-type fixnum from level downto 0
                       do (let ((a (gethash (subseq key 0 (1+ k)) (aref model k))))
                            (when a (return-from predict (car (first a)))))))
               (best (alist)
                 (let ((b nil) (bn -1))
                   (dolist (cell alist b)
                     (when (> (cdr cell) bn) (setf bn (cdr cell) b (car cell)))))))
        (loop for k from 1 below (n-stress-levels)
              do (maphash (lambda (key alist)
                            (let ((total (reduce #'+ alist :key #'cdr :initial-value 0)))
                              (when (or (>= total keep-count)
                                        (not (equal (best alist) (predict key (1- k)))))
                                (store k key alist))))
                          (aref tables k)))))
    (values model kept)))

;;; ---------------------------------------------------------------------------
;;; The decoder, exactly as src/lts.lisp will run it
;;;
;;; Kept here so that the dev numbers below are numbers about the thing that
;;; ships.  src/lts.lisp is a re-implementation of this against the unpacked
;;; tables and inspect/lts-gate.lisp measures that one; if the two ever disagree,
;;; the gate is right.

(defparameter +alpha+ 2d0 "back-off weight: how much a level trusts its parent.")
(defparameter +beam+ 8)
(defparameter +branch+ 4)
(defparameter +stress-weight+ 1d0)

(defun interpolated (model keys)
  "P(chunk | context) as an alist, backing off down the chain.  A level that was
pruned away is simply skipped, which is what interpolation does with an unseen
context anyway."
  (let ((dist '()))
    (dotimes (k (n-levels) dist)
      (let ((alist (gethash (aref keys k) (aref model k))))
        (when alist
          (let* ((total (reduce #'+ alist :key #'cdr :initial-value 0))
                 (a (* +alpha+ (length alist)))
                 (scale (/ (+ total a))))
            (setf dist (mapcar (lambda (cell) (cons (car cell) (* (cdr cell) a scale))) dist))
            (dolist (cell alist)
              (let ((have (assoc (car cell) dist)))
                (if have
                    (incf (cdr have) (* (cdr cell) scale))
                    (push (cons (car cell) (* (cdr cell) scale)) dist))))))))))

(defstruct (hyp (:constructor make-hyp (score phones)))
  (score 0d0 :type double-float)
  (phones nil))                         ; phone ids, most recent first

(defun decode (model word)
  "The beam's best few chunk sequences for WORD, each as (score . phone ids)."
  (let ((letters (word-letters word)))
    (when letters
      (let* ((padded (padded-letters letters))
             (keys (make-array (n-levels)))
             (scratch (make-array 128 :element-type 'fixnum :fill-pointer 0))
             (beam (list (make-hyp 0d0 '()))))
        (dotimes (i (length letters))
          (let ((next '()))
            (dolist (h beam)
              (setf (fill-pointer scratch) 0)
              (dolist (p (reverse (hyp-phones h))) (vector-push p scratch))
              (let ((dist (sort (interpolated model (keys-for padded i scratch keys))
                                #'> :key #'cdr)))
                (loop for cell in dist repeat +branch+
                      do (push (make-hyp (+ (hyp-score h) (log (max 1d-12 (cdr cell))))
                                         (append (reverse (chunk-phones (car cell)))
                                                 (hyp-phones h)))
                               next))))
            (setf beam (subseq (sort next #'> :key #'hyp-score)
                               0 (min +beam+ (length next))))))
        (mapcar (lambda (h) (cons (hyp-score h) (reverse (hyp-phones h)))) beam)))))

(defun stress-logp (stress-model word pattern)
  "Interpolated P(PATTERN | syllables, spelling), the re-ranker's whole-word view."
  (let ((nsyll (length pattern)))
    (if (or (zerop nsyll) (> nsyll +max-syllables+))
        0d0
        (let ((p 1d-3))
          (dotimes (k (n-stress-levels) (log (max 1d-9 p)))
            (let ((alist (gethash (stress-key nsyll word k) (aref stress-model k))))
              (when alist
                (let* ((total (reduce #'+ alist :key #'cdr :initial-value 0))
                       (a (* +alpha+ (length alist)))
                       (hit (or (cdr (assoc pattern alist :test #'string=)) 0)))
                  (setf p (/ (+ hit (* a p)) (+ total a)))))))))))

(defun promote (phones pick)
  (let ((done nil))
    (let ((out (mapcar (lambda (p)
                         (if (and (not done) (vowel-phone-p p) (funcall pick p))
                             (progn (setf done t)
                                    (concatenate 'string (subseq p 0 (1- (length p))) "1"))
                             p))
                       phones)))
      (and done out))))

(defun ensure-stress (phones)
  "The contract: a non-empty answer has a vowel, and one of its vowels is primary."
  (cond ((null phones) nil)
        ((notany #'vowel-phone-p phones)
         ;; a word we heard as nothing but consonants still has to be sayable
         (ensure-stress (if (rest phones)
                            (list* (first phones) "AH1" (rest phones))
                            (append phones (list "AH1")))))
        ((find-if (lambda (p) (eql (char p (1- (length p))) #\1)) phones) phones)
        ;; the loudest thing available: a full vowel if there is one, else a schwa
        (t (or (promote phones (lambda (p) (not (string= p "AH0"))))
               (promote phones (constantly t))))))

(defun predict-phones (model stress-model word)
  (let ((best nil) (best-score -1d30))
    (dolist (h (decode model word))
      (let* ((phones (mapcar (lambda (i) (aref *phones* i)) (cdr h)))
             (score (+ (car h) (* +stress-weight+
                                  (stress-logp stress-model word (stress-pattern phones))))))
        (when (> score best-score) (setf best-score score best phones))))
    (ensure-stress best)))

;;; ---------------------------------------------------------------------------
;;; Writing src/lts-data.lisp
;;;
;;; The table is compiled into the image, so it has to be a Lisp file — but a
;;; quarter of a million entries as list structure is a file the compiler chews on
;;; for a minute and a fasl to match.  So it is written as plain text inside string
;;; literals, which the reader takes in one gulp, and src/lts.lisp unpacks them on
;;; first use.  The format is described there, beside the reader.
;;;
;;; Keys are sorted and stored as the difference from the line before: one
;;; character saying how many leading symbols are shared, then the rest.  Sorted
;;; deep contexts share most of their length, so this is worth about a third of
;;; the file.

(defparameter +symbols+
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&*+-"
  "One character per symbol value, for keys, counts and chunk ids.  70 of them:
enough for a phone-history symbol, and none of them a quote or a backslash.")

(defun sym-char (v)
  (assert (< -1 v (length +symbols+)) () "symbol ~d out of range" v)
  (char +symbols+ v))

(defun encode-number (n width)
  (let ((out (make-string width)) (base (length +symbols+)))
    (loop for i from (1- width) downto 0
          do (setf (char out i) (char +symbols+ (mod n base)) n (floor n base)))
    (assert (zerop n) () "number too wide")
    out))

(defun key-symbols (key level)
  "The packed KEY back as its symbols, outermost first."
  (let ((out (make-array (1+ level))))
    (loop for k from level downto 0
          do (let ((bits (if (zerop k) 5 (level-bits k))))
               (setf (aref out k) (logand key (1- (ash 1 bits))))
               (setf key (ash key (- bits)))))
    out))

(defun shared-prefix (a b)
  (let ((n (min (length a) (length b))))
    (or (loop for i below n unless (eql (aref a i) (aref b i)) return i) n)))

(defun write-level (out entries renumber)
  "ENTRIES is a list of (symbol-vector . ((chunk . count) ...)), sorted."
  (let ((previous #()))
    (dolist (e entries)
      (let* ((syms (car e))
             (share (shared-prefix previous syms)))
        (write-char (sym-char share) out)
        (loop for i from share below (length syms) do (write-char (sym-char (aref syms i)) out))
        (dolist (cell (cdr e))
          (write-string (encode-number (gethash (car cell) renumber) 2) out)
          (write-char (sym-char (count-code (cdr cell))) out))
        (write-char #\Newline out)
        (setf previous syms)))))

(defun used-chunks (model)
  (let ((used (make-hash-table :test #'eql)))
    (dotimes (k (n-levels))
      (maphash (lambda (key alist)
                 (declare (ignore key))
                 (dolist (cell alist) (setf (gethash (car cell) used) t)))
               (aref model k)))
    (sort (loop for c being the hash-keys of used collect c) #'<)))

(defun write-data-file (path model stress-model &key notes)
  (let* ((ids (used-chunks model))
         (renumber (make-hash-table :test #'eql)))
    (loop for id in ids for new from 0 do (setf (gethash id renumber) new))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (format out ";;;; lts-data.lisp — GENERATED by tools/learn-lts.lisp.  Do not edit.~%")
      (format out ";;;;~%")
      (format out ";;;; Letter-to-sound tables learned from data/cmudict.dict (BSD; see~%")
      (format out ";;;; data/LICENSE.cmudict).  Nothing here derives from any other source.~%")
      (format out ";;;;~%")
      (format out ";;;; They are text inside string literals rather than list structure: the~%")
      (format out ";;;; reader swallows a string whole, where a quarter of a million conses~%")
      (format out ";;;; would be a minute of compiling and a fasl to match.  src/lts.lisp~%")
      (format out ";;;; unpacks them into hash tables the first time a word misses the~%")
      (format out ";;;; dictionary; the format is documented there, beside the reader.~%")
      (format out ";;;;~%")
      (dolist (line notes) (format out ";;;; ~a~%" line))
      (format out "~%(in-package #:chord)~%~%")
      ;; --- the shape of the tables, so the reader needs no shared constants ---
      (format out "(defparameter +lts-chain+~%  '~s~%  ~s)~%~%"
              +chain+
              "One entry per context level above the bare letter, in order.  The
runtime builds its keys from this, so the tables below and the matcher cannot
drift apart: (:LEFT n) / (:RIGHT n) add the letter n places away, (:PREV n) adds
the n-th most recent phone already said.")
      (format out "(defparameter +lts-stress-shape+ '(~d ~d ~d)~%  ~s)~%~%"
              +stress-suffix+ +stress-prefix+ +max-syllables+
              "Stress keys: how many final letters, how many initial letters, and
the largest syllable count modelled.")
      ;; --- phones, in the order everything else refers to them ---------------
      (format out "(defparameter +lts-phones+ \"~{~a~^ ~}\"~%  ~s)~%~%"
              (coerce *phones* 'list)
              "Phone ids, in the order the tables refer to them.")
      ;; --- chunks: what one letter can say ----------------------------------
      (format out "(defparameter +lts-chunks+ \"")
      (dolist (id ids)
        (format out "~{~a~^ ~}~%" (mapcar (lambda (p) (encode-number p 2)) (chunk-phones id))))
      (format out "\"~%  ~s)~%~%" "Chunk id (by line, from 0) -> the phone ids one letter says.")
      ;; --- the context model -------------------------------------------------
      (format out "(defparameter +lts-rules+ \"")
      (dotimes (k (n-levels))
        (let ((entries '()))
          (maphash (lambda (key alist) (push (cons (key-symbols key k) alist) entries))
                   (aref model k))
          (setf entries (sort entries (lambda (a b)
                                        (let ((x (car a)) (y (car b)))
                                          (loop for i below (length x)
                                                do (cond ((< (aref x i) (aref y i)) (return t))
                                                         ((> (aref x i) (aref y i)) (return nil)))
                                                finally (return nil))))))
          (write-level out entries renumber))
        (format out "=~%"))
      (format out "\"~%  ~s)~%~%"
              "Context windows, narrowest level first; see UNPACK-LTS-RULES.")
      ;; --- the stress model --------------------------------------------------
      (format out "(defparameter +lts-stress+ \"")
      (let ((pat-ids (make-hash-table :test #'equal)))
        (declare (ignore pat-ids))
        (dotimes (k (n-stress-levels))
          (let ((entries '()))
            (maphash (lambda (key alist) (push (cons key alist) entries)) (aref stress-model k))
            (setf entries (sort entries (lambda (a b)
                                          (let ((x (car a)) (y (car b)))
                                            (loop for i below (length x)
                                                  do (cond ((< (aref x i) (aref y i)) (return t))
                                                           ((> (aref x i) (aref y i)) (return nil)))
                                                  finally (return nil))))))
            (let ((previous #()))
              (dolist (e entries)
                (let* ((syms (car e)) (share (shared-prefix previous syms)))
                  (write-char (sym-char share) out)
                  (loop for i from share below (length syms)
                        do (write-char (sym-char (aref syms i)) out))
                  (dolist (cell (cdr e))
                    (write-string (car cell) out)
                    (write-char (sym-char (count-code (cdr cell))) out))
                  (write-char #\Newline out)
                  (setf previous syms)))))
          (format out "=~%")))
      (format out "\"~%  ~s)~%" "Syllable count and spelling -> stress pattern."))
    (with-open-file (in path) (file-length in))))

;;; ---------------------------------------------------------------------------
;;; Measuring, here, on the DEV slice.  inspect/lts-gate.lisp measures the test
;;; slice; nothing in this file is allowed to look at that.

(defun edit-distance (a b)
  (let* ((a (coerce a 'vector)) (b (coerce b 'vector))
         (n (length a)) (m (length b))
         (prev (make-array (1+ m) :element-type 'fixnum))
         (cur (make-array (1+ m) :element-type 'fixnum)))
    (dotimes (j (1+ m)) (setf (aref prev j) j))
    (loop for i from 1 to n
          do (setf (aref cur 0) i)
             (loop for j from 1 to m
                   do (setf (aref cur j)
                            (min (1+ (aref prev j)) (1+ (aref cur (1- j)))
                                 (+ (aref prev (1- j))
                                    (if (string= (aref a (1- i)) (aref b (1- j))) 0 1)))))
             (rotatef prev cur))
    (aref prev m)))

(defun strip-stress (phones)
  (mapcar (lambda (p) (if (vowel-phone-p p) (subseq p 0 (1- (length p))) p)) phones))

(defun evaluate (model stress-model entries roles role)
  (let ((words 0) (exact 0) (bare 0) (total 0) (errors 0) (bare-errors 0))
    (dotimes (i (length entries))
      (when (eq (aref roles i) role)
        (let* ((e (aref entries i))
               (truth (cdr e))
               (guess (predict-phones model stress-model (car e))))
          (incf words)
          (incf total (length truth))
          (incf errors (edit-distance truth guess))
          (incf bare-errors (edit-distance (strip-stress truth) (strip-stress guess)))
          (when (equal truth guess) (incf exact))
          (when (equal (strip-stress truth) (strip-stress guess)) (incf bare)))))
    (list :words words
          :exact (/ exact (max 1 words))
          :exact-bare (/ bare (max 1 words))
          :phone-acc (- 1d0 (/ errors (max 1 total)))
          :phone-acc-bare (- 1d0 (/ bare-errors (max 1 total))))))

(defun report (label r)
  (note "~a words ~:d | exact ~,2f% | exact-no-stress ~,2f% | phone ~,2f% | phone-no-stress ~,2f%"
        label (getf r :words) (* 100 (getf r :exact)) (* 100 (getf r :exact-bare))
        (* 100 (getf r :phone-acc)) (* 100 (getf r :phone-acc-bare))))

;;; ---------------------------------------------------------------------------

(defvar *entries* nil)
(defvar *roles* nil)
(defvar *aligned* nil)
(defvar *context-tables* nil)
(defvar *stress-tables* nil)

(defun prepare (&key (iterations 12))
  "Read, split, align, tally.  The expensive half, kept apart so the cheap half
can be re-run while tuning."
  (note "reading data/cmudict.dict")
  (setf *entries* (read-entries))
  (setf *roles* (make-array (length *entries*)))
  (dotimes (i (length *entries*)) (setf (aref *roles* i) (split-role i)))
  (note "  ~:d entries, ~:d train / ~:d dev / ~:d test"
        (length *entries*) (count :train *roles*) (count :dev *roles*) (count :test *roles*))
  (setf *n-phones* (intern-phones *entries*))
  (setf *n-chunks* (+ 1 *n-phones* (* *n-phones* *n-phones*)))
  (note "  ~d phones" *n-phones*)
  (let ((train '()))
    (dotimes (i (length *entries*))
      (when (eq (aref *roles* i) :train)
        (let* ((e (aref *entries* i)) (letters (word-letters (car e))))
          (when letters
            (push (make-sample (car e) letters (word-phone-ids (cdr e))) train)))))
    (setf train (nreverse train))
    (note "expectation-maximisation alignment")
    (let ((usable (run-em train :iterations iterations)))
      (note "committing each word to its best alignment")
      (dolist (s usable) (setf (sample-chunks s) (viterbi-align s)))
      (setf *aligned* (remove-if-not #'sample-chunks usable))
      (note "  ~:d aligned" (length *aligned*))))
  (note "tallying")
  (setf *context-tables* (tally-contexts *aligned*))
  (setf *stress-tables* (tally-stress *aligned*))
  (note "  context windows seen: ~:d over ~d levels"
        (loop for k below (n-levels) sum (hash-table-count (aref *context-tables* k)))
        (n-levels))
  (note "  stress contexts seen: ~:d over ~d levels"
        (loop for k below (n-stress-levels) sum (hash-table-count (aref *stress-tables* k)))
        (n-stress-levels))
  *aligned*)

(defun learn (&key (iterations 12) (keep-count 5) (top 2)
                   (stress-keep-count 2) (stress-top 3) (write t))
  (unless *aligned* (prepare :iterations iterations))
  (multiple-value-bind (model kept) (difference-list *context-tables* :keep-count keep-count
                                                                     :top top)
    (multiple-value-bind (stress-model stress-kept)
        (stress-difference-list *stress-tables* :keep-count stress-keep-count :top stress-top)
      (note "  context model: ~:d entries (~{~:d~^ ~})" kept
            (loop for k below (n-levels) collect (hash-table-count (aref model k))))
      (note "  stress model:  ~:d entries" stress-kept)
      (let ((start (get-internal-real-time)))
        (report "dev  " (evaluate model stress-model *entries* *roles* :dev))
        (note "  (~,1f ms per word)"
              (/ (* 1000 (- (get-internal-real-time) start))
                 internal-time-units-per-second (count :dev *roles*))))
      (when write
        (let* ((path (repo-file "src/lts-data.lisp"))
               (bytes (write-data-file path model stress-model
                                       :notes (list (format nil "learned from ~:d aligned training entries; ~:d context rules, ~:d stress rules"
                                                            (length *aligned*) kept stress-kept)))))
          (note "wrote ~a (~:d bytes)" (namestring path) bytes)))
      (values model stress-model))))

(unless (find-symbol "*LTS-NO-RUN*" '#:cl-user)
  (learn))
