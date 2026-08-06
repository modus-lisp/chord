;;;; lts-gate.lisp — what the letter-to-sound rules are measured by.
;;;;
;;;;   sbcl --non-interactive --load inspect/lts-gate.lisp
;;;;
;;;; The rules in src/lts-data.lisp were learned from data/cmudict.dict, so
;;;; measuring them on that dictionary only means anything on the part of it the
;;;; learner never saw.  One entry in twenty is held back for exactly this: read
;;;; the file in order, keep the entries whose word is letters and apostrophes,
;;;; keep the first spelling of each word, number what survives from zero, and
;;;; index mod 20 = 0 is the TEST slice.  (Index mod 20 = 1 is a dev slice the
;;;; learner tunes on; everything else it trains on.)  That rule is restated from
;;;; the file here rather than imported, so that the gate and the learner agreeing
;;;; is evidence rather than assumption.
;;;;
;;;; Three numbers, and they answer different questions:
;;;;
;;;;   PHONE ACCURACY   1 - (edit distance / phones in the truth), summed over
;;;;                    the slice.  Levenshtein over phone strings, so an inserted
;;;;                    or dropped phone costs one, not a whole word.  This is the
;;;;                    number that says how close a wrong answer is to right.
;;;;   WORD EXACT       the whole pronunciation, stress digits and all, identical
;;;;                    to the dictionary's.  The honest headline, and the harsh
;;;;                    one: one schwa in a nine-phone word fails it.
;;;;   WORD EXACT, NO STRESS
;;;;                    the same with the digits stripped, which separates `we
;;;;                    heard the wrong sounds' from `we heard the right sounds
;;;;                    and put the emphasis in the wrong place'.
;;;;
;;;; There is no reference synthesiser anywhere in this file.  Held-out dictionary
;;;; entries are the only oracle chord has, on purpose.
;;;;
;;;; A caveat about the ceiling: the truth here is CMUdict's FIRST listing, and
;;;; for a word with real variants (`either', `route') the rules can produce a
;;;; pronunciation an American would call correct and still be scored wrong.
;;;; Nothing is done about that — it is a reason the exact-match number is a floor,
;;;; not a reason to move it.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :chord)))

(defpackage #:chord-lts-gate (:use #:cl))
(in-package #:chord-lts-gate)

(defvar *pass* 0)
(defvar *fail* 0)

(defun check (ok label &optional detail)
  (if ok (incf *pass*) (incf *fail*))
  (format t "  ~:[FAIL~;ok  ~] ~a~@[ — ~a~]~%" ok label detail))

(defun section (title)
  (format t "~&~%-- ~a ~a~%" title
          (make-string (max 0 (- 60 (length title))) :initial-element #\-)))

;;; ---------------------------------------------------------------------------
;;; The dictionary and the split, stated from the file

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

(defun read-entries (path)
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

(defun test-slice-p (index) (zerop (mod index 20)))

;;; ---------------------------------------------------------------------------
;;; Scoring

(defun edit-distance (a b)
  "Levenshtein over two lists of phone strings."
  (let* ((a (coerce a 'vector)) (b (coerce b 'vector))
         (n (length a)) (m (length b))
         (prev (make-array (1+ m) :element-type 'fixnum))
         (cur (make-array (1+ m) :element-type 'fixnum)))
    (dotimes (j (1+ m)) (setf (aref prev j) j))
    (loop for i from 1 to n
          do (setf (aref cur 0) i)
             (loop for j from 1 to m
                   do (setf (aref cur j)
                            (min (1+ (aref prev j))
                                 (1+ (aref cur (1- j)))
                                 (+ (aref prev (1- j))
                                    (if (string= (aref a (1- i)) (aref b (1- j))) 0 1)))))
             (rotatef prev cur))
    (aref prev m)))

(defun vowel-p (phone)
  (and (plusp (length phone))
       (char<= #\0 (char phone (1- (length phone))) #\9)))

(defun strip-stress (phones)
  (mapcar (lambda (p) (if (vowel-p p) (subseq p 0 (1- (length p))) p)) phones))

(defun show (phones) (format nil "~{~a~^ ~}" phones))

;;; ---------------------------------------------------------------------------

(defvar *dict* (merge-pathnames "../data/cmudict.dict"
                                (make-pathname :name nil :type nil
                                               :defaults *load-truename*)))

(section "the dictionary and the split")

(defvar *entries* (read-entries *dict*))
(defvar *test* (loop for i below (length *entries*)
                     when (test-slice-p i) collect (aref *entries* i)))

(format t "  ~:d usable entries, ~:d of them held out as the test slice~%"
        (length *entries*) (length *test*))
(check (> (length *test*) 5000) "the test slice is big enough to mean something"
       (format nil "~:d words" (length *test*)))

;;; ---------------------------------------------------------------------------

(section "the contract")

;; The lexicon promises its caller a pronunciation for anything with a letter in
;; it, so these are not accuracy questions — they are questions about whether the
;; synthesiser can be handed the answer at all.
(dolist (word '("qwrtplk" "zzzzz" "'" "a" "i" "schlernpfeffer" "aaaaaaaaaaaaaaaaaaaa"
                "o'shaughnessy" "x" "mmm"))
  (let ((p (chord:lts-phones word)))
    (if (some (lambda (c) (char<= #\a c #\z)) word)
        (check (and p (some #'vowel-p p)
                    (find-if (lambda (s) (eql (char s (1- (length s))) #\1)) p))
               (format nil "~s is sayable" word) (show p))
        (check (null p) (format nil "~s has no letters, so no phones" word)))))

;; And the same over the whole slice, which is where a rare shape would show up.
(let ((empty 0) (voiceless 0) (unstressed 0))
  (dolist (e *test*)
    (let ((p (chord:lts-phones (car e))))
      (cond ((null p) (incf empty))
            ((notany #'vowel-p p) (incf voiceless))
            ((not (find-if (lambda (s) (eql (char s (1- (length s))) #\1)) p))
             (incf unstressed)))))
  (check (zerop empty) "no held-out word comes back with nothing to say"
         (format nil "~d empty" empty))
  (check (zerop voiceless) "every answer has a vowel in it"
         (format nil "~d without" voiceless))
  (check (zerop unstressed) "every answer has a primary stress"
         (format nil "~d without" unstressed)))

;;; ---------------------------------------------------------------------------

(section "held-out accuracy")

(defvar *results* '())

(let ((words 0) (exact 0) (bare 0) (phones 0) (errors 0) (bare-errors 0)
      (start (get-internal-real-time)))
  (dolist (e *test*)
    (let* ((truth (cdr e))
           (guess (chord:lts-phones (car e)))
           (d (edit-distance truth guess))
           (db (edit-distance (strip-stress truth) (strip-stress guess))))
      (push (list (car e) truth guess d) *results*)
      (incf words)
      (incf phones (length truth))
      (incf errors d)
      (incf bare-errors db)
      (when (equal truth guess) (incf exact))
      (when (equal (strip-stress truth) (strip-stress guess)) (incf bare))))
  (setf *results* (nreverse *results*))
  (let ((ms (/ (* 1000d0 (- (get-internal-real-time) start))
               internal-time-units-per-second words)))
    (format t "~%  test slice           ~:d words, ~:d phones~%" words phones)
    (format t "  phone accuracy       ~,2f%   (1 - edit distance / phones)~%"
            (* 100 (- 1d0 (/ errors phones))))
    (format t "  phone accuracy       ~,2f%   ignoring stress digits~%"
            (* 100 (- 1d0 (/ bare-errors phones))))
    (format t "  word exact           ~,2f%   (~:d of ~:d)~%"
            (* 100 (/ exact words)) exact words)
    (format t "  word exact           ~,2f%   ignoring stress digits (~:d of ~:d)~%"
            (* 100 (/ bare words)) bare words)
    (format t "  cost                 ~,2f ms per word~%~%" ms)
    ;; Published English letter-to-sound systems land around 60-75% word-exact.
    ;; The bar here is set below where this measures, not at it: it is a guard
    ;; against the tables being regenerated into something broken, not a target.
    (check (> (/ exact words) 0.50) "word-exact is in the range this was built to"
           (format nil "~,2f%" (* 100 (/ exact words))))
    (check (> (- 1d0 (/ errors phones)) 0.84) "phone accuracy is where it was left"
           (format nil "~,2f%" (* 100 (- 1d0 (/ errors phones)))))))

;;; ---------------------------------------------------------------------------

(section "where the errors are")

;; A word can be wrong in two quite different ways and the fix is different for
;; each, so it is worth knowing the mix.
(let ((right 0) (stress-only 0) (segments 0))
  (dolist (r *results*)
    (destructuring-bind (word truth guess d) r
      (declare (ignore word d))
      (cond ((equal truth guess) (incf right))
            ((equal (strip-stress truth) (strip-stress guess)) (incf stress-only))
            (t (incf segments)))))
  (let ((n (length *results*)))
    (format t "  right                       ~5d  ~,1f%~%" right (* 100 (/ right n)))
    (format t "  right sounds, wrong stress  ~5d  ~,1f%~%" stress-only (* 100 (/ stress-only n)))
    (format t "  wrong sounds                ~5d  ~,1f%~%" segments (* 100 (/ segments n)))))

;; How far off the wrong ones are.  Most of the damage is one phone.
(let ((by-distance (make-hash-table)))
  (dolist (r *results*) (incf (gethash (fourth r) by-distance 0)))
  (format t "~%  phones wrong per word:")
  (loop for d from 0 to 4
        do (format t "  ~d:~d" d (gethash d by-distance 0)))
  (format t "  5+:~d~%"
          (loop for k being the hash-keys of by-distance using (hash-value v)
                when (> k 4) sum v)))

;;; ---------------------------------------------------------------------------

(section "worked examples")

;; Fixed picks, not the prettiest ones: every 700th word of the test slice, so
;; this table changes only when the model does.
(format t "~%  ~14a  ~a~%" "WORD" "GUESS, then the dictionary where they differ")
(let ((n (length *results*)))
  (loop for i from 0 below n by (max 1 (floor n 14))
        do (destructuring-bind (word truth guess d) (nth i *results*)
             (format t "~%  ~14a  ~a~%" word (show guess))
             (if (equal truth guess)
                 (format t "  ~14a  right~%" "")
                 (format t "  ~14a  ~a   (~d phone~:p out)~%" "" (show truth) d)))))

;; And some deliberate hard cases: words a letter-to-sound rule has no business
;; getting right, kept as a reminder of what the failures actually look like.
(format t "~%  a few by hand:~%")
(dolist (word '("colonel" "yacht" "chthonic" "gnocchi" "phlegm" "quixotic"
                "onomatopoeia" "worcestershire" "isle" "subtle"))
  (let* ((truth (cdr (find word (coerce *entries* 'list) :key #'car :test #'string=)))
         (guess (chord:lts-phones word)))
    (format t "~%  ~14a  ~a~%" word (show guess))
    (cond ((null truth) (format t "  ~14a  (not in the dictionary)~%" ""))
          ((equal truth guess) (format t "  ~14a  right~%" ""))
          (t (format t "  ~14a  ~a   (~d phone~:p out)~%" "" (show truth)
                     (edit-distance truth guess))))))

;;; ---------------------------------------------------------------------------

(section "cost")

(let ((bytes (with-open-file (in (merge-pathnames "../src/lts-data.lisp"
                                                  (make-pathname :name nil :type nil
                                                                 :defaults *load-truename*)))
               (file-length in))))
  (format t "  src/lts-data.lisp    ~:d bytes~%" bytes)
  (check (< bytes 6000000) "the table is small enough to compile into the image"
         (format nil "~,1f MB" (/ bytes 1024d0 1024))))

(format t "~%~a~%~d passed, ~d failed.~%"
        (make-string 64 :initial-element #\=) *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
