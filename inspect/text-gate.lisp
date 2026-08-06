;;;; text-gate.lisp — what text normalisation is measured by.
;;;;
;;;;   sbcl --non-interactive --load inspect/text-gate.lisp
;;;;
;;;; The contract under test:
;;;;
;;;;   (normalize-text TEXT) -> a list of sentence-sized clauses, each of them
;;;;   nothing but words the lexicon can be asked about — letters, apostrophes
;;;;   and internal hyphens, one space between them.
;;;;
;;;; Three kinds of check, and it matters which is which:
;;;;
;;;;   TABLE      an input and the exact clauses it must produce.  Most of the
;;;;              file.  These are assertions about English, not about the
;;;;              implementation: `$4.50' is four dollars and fifty cents in any
;;;;              synthesiser worth having.
;;;;   KNOWN      an input we get WRONG, with what it should be and why we do not
;;;;              do it.  Printed, counted, and deliberately not fatal — a gate
;;;;              that only lists what already works is worth nothing.
;;;;   INVARIANT  properties that must hold for every input, checked over a
;;;;              corpus rather than case by case: only sayable characters come
;;;;              out, no clause is empty, nothing with a digit in it vanishes.
;;;;
;;;; The last of those is the one that matters most.  The failure this file
;;;; exists to prevent is silent deletion: a price or a date that never reaches
;;;; the listener, in a sentence that otherwise sounds perfectly fine.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :chord)))

(defpackage #:chord-text-gate (:use #:cl))
(in-package #:chord-text-gate)

(defvar *pass* 0)
(defvar *fail* 0)
(defvar *known* 0)

(defun section (title)
  (format t "~&~%-- ~a ~a~%" title
          (make-string (max 0 (- 58 (length title))) :initial-element #\-)))

(defun check (ok label &optional detail)
  (if ok (incf *pass*) (incf *fail*))
  (format t "  ~:[FAIL~;ok  ~] ~a~@[~%         ~a~]~%" ok label (unless ok detail)))

(defun say (input want)
  "INPUT must normalise to exactly the clauses WANT."
  (let ((got (chord:normalize-text input)))
    (check (equal got want) (format nil "~s" input)
           (format nil "got  ~s~%         want ~s" got want))))

(defun known (input want why)
  "INPUT is normalised WRONG on purpose; WANT is what a better rule would say."
  (incf *known*)
  (let ((got (chord:normalize-text input)))
    (format t "  ~:[known~;OK?  ~] ~s~%         got  ~s~%         want ~s~%         ~a~%"
            (equal got want) input got want why)))

;;; ---- cardinals -------------------------------------------------------------

(section "numbers said as quantities")

(say "0" '("zero"))
(say "7" '("seven"))
(say "13" '("thirteen"))
(say "20" '("twenty"))
(say "42" '("forty two"))
(say "101" '("one hundred one"))
(say "999" '("nine hundred ninety nine"))
(say "1000" '("one thousand"))
(say "1,000,000" '("one million"))
(say "10,000,000,000" '("ten billion"))
(say "1234567" '("one million two hundred thirty four thousand five hundred sixty seven"))
(say "-42" '("minus forty two"))
(say "3.14159" '("three point one four one five nine"))
(say "0.5" '("zero point five"))
(say ".5" '("point five"))
(say "-0.25" '("minus zero point two five"))
;; a leading zero, or ten digits of it, is an identifier and not a quantity
(say "007" '("zero zero seven"))
(say "1234567890" '("one two three four five six seven eight nine zero"))
(say "Version 1.2.3" '("Version one point two point three"))

;;; ---- ordinals, years, decades ----------------------------------------------

(section "numbers said as positions in a sequence")

(say "1st" '("first"))
(say "2nd" '("second"))
(say "3rd" '("third"))
(say "4th" '("fourth"))
(say "11th" '("eleventh"))
(say "21st" '("twenty first"))
(say "42nd" '("forty second"))
(say "103rd" '("one hundred third"))
(say "3rd-party" '("third party"))

(say "1984" '("nineteen eighty four"))
(say "2026" '("twenty twenty six"))
(say "1900" '("nineteen hundred"))
(say "1905" '("nineteen oh five"))
(say "2000" '("two thousand"))
(say "2005" '("two thousand five"))
;; grouping commas mean a count, so the year reading is off
(say "1,984" '("one thousand nine hundred eighty four"))
(say "1990s" '("nineteen nineties"))
(say "80's" '("eighties"))
(say "1900s" '("nineteen hundreds"))

;;; ---- money -----------------------------------------------------------------

(section "money")

(say "$4.50" '("four dollars and fifty cents"))
(say "$1" '("one dollar"))
(say "$1.00" '("one dollar"))
(say "$0.99" '("ninety nine cents"))
(say "$1,234.56"
     '("one thousand two hundred thirty four dollars and fifty six cents"))
(say "$5 million" '("five million dollars"))
(say "$1.5 billion" '("one point five billion dollars"))
(say "$20-$30" '("twenty to thirty dollars"))
(say "-$3" '("minus three dollars"))
(say (format nil "~a10" (code-char #x00A3)) '("ten pounds"))
(say (format nil "~a10.50" (code-char #x20AC)) '("ten euros and fifty cents"))

;;; ---- percent, units, temperature -------------------------------------------

(section "units")

(say "50%" '("fifty percent"))
(say "12.5%" '("twelve point five percent"))
(say "100%" '("one hundred percent"))
(say "3km" '("three kilometers"))
(say "1km" '("one kilometer"))
(say "5 kg" '("five kilograms"))
(say "60 mph" '("sixty miles per hour"))
(say "3 km/h" '("three kilometers per hour"))
(say "16 GB" '("sixteen gigabytes"))
(say (format nil "72~aF" (code-char #x00B0)) '("seventy two degrees fahrenheit"))
(say "10 x 20" '("ten by twenty"))

;;; ---- times and dates -------------------------------------------------------

(section "times and dates")

(say "12:30" '("twelve thirty"))
(say "12:00" '("twelve o'clock"))
(say "9:05" '("nine oh five"))
(say "18:45" '("eighteen forty five"))
(say "1:23:45" '("one hour twenty three minutes forty five seconds"))
(say "5pm" '("five P M"))
(say "5 pm" '("five P M"))
(say "5:30 p.m." '("five thirty P M"))
(say "at 5 a.m. sharp" '("at five A M sharp"))
(say "June 15" '("June fifteenth"))
(say "June 15, 2026" '("June fifteenth twenty twenty six"))
(say "Jan. 3rd, 2026" '("January third twenty twenty six"))
(say "12/25/2026" '("December twenty fifth twenty twenty six"))
(say "2026-06-15" '("June fifteenth twenty twenty six"))

;;; ---- fractions, ranges, telephone numbers ----------------------------------

(section "fractions, ranges and identifiers")

(say "1/2" '("one half"))
(say "3/4" '("three quarters"))
(say "2/3" '("two thirds"))
(say "5/16" '("five sixteenths"))
(say "5-10" '("five to ten"))
(say "1939-1945" '("nineteen thirty nine to nineteen forty five"))
(say "20-30%" '("twenty to thirty percent"))
(say "555-1234" '("five five five one two three four"))
(say "555-123-4567" '("five five five one two three four five six seven"))
(say "+1-555-0100" '("plus one five five five zero one zero zero"))

;;; ---- abbreviations ---------------------------------------------------------

(section "abbreviations, and what their period means")

(say "Mr. Smith" '("mister Smith"))
(say "Mrs. Smith" '("missus Smith"))
(say "Dr. Smith" '("doctor Smith"))
(say "Prof. Jones" '("professor Jones"))
(say "St. Louis" '("saint Louis"))
(say "Main St." '("Main street"))
(say "Mt. Everest" '("mount Everest"))
(say "Rev. Dr. King Jr. spoke" '("reverend doctor King junior spoke"))
;; the period that does not end the sentence — the interesting half
(say "Apples, e.g. Granny Smith, are green."
     '("Apples E G Granny Smith are green"))
(say "Cats vs. dogs is settled." '("Cats versus dogs is settled"))
(say "approx. 5 minutes" '("approximately five minutes"))
;; the same period, ending a sentence, told apart by the capital after it
(say "Bring books, pens, etc. Then leave."
     '("Bring books pens etcetera" "Then leave"))
(say "He works at Acme Inc. and likes it."
     '("He works at Acme incorporated and likes it"))
(say "He works at Acme Inc. It is fine."
     '("He works at Acme incorporated" "It is fine"))
(say "He lives in the U.S. economy" '("He lives in the U S economy"))
(say "He lives in the U.S. Then he left." '("He lives in the U S" "Then he left"))
(say "See vol. 3, fig. 2." '("See volume three figure two"))
(say "No. 5 is fine." '("number five is fine"))
(say "No parking here." '("No parking here"))
(say "A. Lincoln spoke." '("ay Lincoln spoke"))
(say "Ph.D. in physics" '("P H D in physics"))

;;; ---- acronyms and initialisms ----------------------------------------------

(section "acronyms said as words, initialisms said as letters")

(say "NASA" '("nasa"))
(say "NATO" '("nato"))
(say "SCUBA" '("scuba"))
(say "FBI" '("fbi"))
(say "IBM" '("ibm"))
(say "NSA" '("N S ay"))
(say "LSAT" '("L S ay T"))
(say "U.S." '("U S"))
(say "U.S.A." '("U S A"))
(say "F.B.I." '("F B I"))
(say "PNG" '("P N G"))
(say "AT&T" '("ay T and T"))
(say "MP3" '("M P three"))
(say "COVID-19" '("covid nineteen"))
(say "www" '("W W W"))
;; a capitalised sentence is emphasis, not an acronym: it must survive as words
;; rather than be spelled out letter by letter.  The dictionary answers in its
;; own lower case, and nothing downstream cares which case a word arrives in.
(say "THE DOG RAN AWAY" '("the dog ran away"))

;;; ---- symbols ---------------------------------------------------------------

(section "symbols said as the words they stand for")

(say "R&D" '("R and D"))
(say "5 + 3 = 8" '("five plus three equals eight"))
(say "#5" '("number five"))
(say "50% off" '("fifty percent off"))
(say "and/or" '("and slash or"))
(say "foo@bar.com" '("foo at bar dot com"))
(say "www.example.com/x" '("W W W dot example dot com slash x"))
(say "C++" '("C plus plus"))
(say "a < b > c" '("a less than b greater than c"))
(say (format nil "~a2026 Acme" (code-char #x00A9)) '("copyright twenty twenty six Acme"))

;;; ---- punctuation and clause breaks -----------------------------------------

(section "punctuation becomes a break, or nothing at all")

(say "Hello there. The desktop can speak now!"
     '("Hello there" "The desktop can speak now"))
(say "Really? Yes." '("Really" "Yes"))
(say "One; two: three." '("One" "two" "three"))
(say "He said \"hello\" (loudly) and left."
     '("He said hello loudly and left"))
(say "He left... and came back." '("He left" "and came back"))
(say (format nil "Don~at ~a go ~a he said." (code-char #x2019) (code-char #x2014)
             (code-char #x2014))
     '("Don't go he said"))
(say "well-known wi-fi don't" '("well-known wi-fi don't"))
(say (format nil "caf~a na~ave" (code-char #x00E9) (code-char #x00EF))
     '("cafe naive"))

;;; ---- degenerate input ------------------------------------------------------

(section "input with nothing in it to say")

(say "" '())
(say "   " '())
(say "..." '())
(say "!!!" '())
(say "---" '())
(say "()" '())
(say (format nil "~c~c" #\Newline #\Tab) '())
;; something with no letters that IS worth saying still is
(say "$" '("dollars"))
(say "%" '("percent"))
(say "42" '("forty two"))

;;; ---- known failures --------------------------------------------------------

(section "known wrong, on purpose")

(known "It was 72F outside."
       '("It was seventy two degrees fahrenheit outside")
       "A degree sign makes a temperature; a bare F or C after a number could as
         easily be a seat or a grade, so it is left as a letter.")

(known "World War II ended in 1945."
       '("World War two ended in nineteen forty five")
       "Roman numerals are not read.  II is two letters, and a rule that turned
         every I, V, X, L, C, D and M into a number would wreck ordinary words.")

(known "The WHO issued a warning."
       '("The W H O issued a warning")
       "CMUdict lists `who', so the dictionary — right nearly every other time —
         says it is a word here.  Reading ALL-CAPS emphasis correctly is worth
         more than the World Health Organization.")

(known "The US economy grew."
       '("The U S economy grew")
       "Bare capitals that spell a common word are read as that word, so US is
         the pronoun.  Written U.S., where the author's periods say what is
         meant, it comes out as two letters.")

(known "A.B.C. Corp. said so."
       '("ay B C corporation said so")
       "An abbreviation's period ends the sentence when a capital follows, which
         is what makes `etc. Then' work — and what breaks a capitalised word
         after an initialism.")

(known "The JPEG was large."
       '("The jay peg was large")
       "JPEG is said `jay peg', which is neither the letters nor the spelling.
         Only a per-word exception list would fix it, and it would be long.")

(known "Yes, of course, it works."
       '("Yes" "of course" "it works")
       "Commas do not break a clause.  A clause is the unit of synthesis, so a
         break is a full stop's worth of silence — too much for a comma, and the
         comma's own short pause has nowhere to live in a stream of words.")

(known "It happened on 9/11."
       '("It happened on nine eleven")
       "9/11 is read as a date, because 9/11 is a date.  The one American idiom
         that says the digits is not worth a special case.")

;;; ---- invariants ------------------------------------------------------------

(section "properties that hold for every input")

(defparameter *corpus*
  (list "The quick brown fox jumps over the lazy dog."
        "On Jan. 3rd, 2026 at 12:30 p.m. Dr. A. Smith paid $4,500.99 for 3km of
         cable — 50% off, vs. the $9,000 list price, No. 12 in the catalogue."
        "Call 555-123-4567 or +1-800-555-0100, ext. 7, between 9:00 and 17:30."
        "NASA, the FBI and the U.S. Dept. of Energy each own 1/3 of the 2026
         budget: $1.5 billion, up 12.5% from the 1990s."
        "Mr. & Mrs. Smith of 42 Main St. (apt. 3B) sent e-mail to foo@bar.com."
        "%%% 007 --- ... !!! $$$ @@@ ### 3.14.15 1/2/3/4 ????"
        "IT'S ALL CAPS, ISN'T IT?  Well... mostly; yes."
        (format nil "~a~a~a caf~a 72~aF 1~a5 no~ael"
                (code-char #x201C) "quoted" (code-char #x201D) (code-char #x00E9)
                (code-char #x00B0) (code-char #x2013) (code-char #x00EB))
        ""
        "   "
        "a"
        "1"))

(defun sayable-word-p (w)
  (and (plusp (length w))
       (some #'alpha-char-p w)
       (every (lambda (c) (or (alpha-char-p c) (find c "'-"))) w)))

(let ((bad '()))
  (dolist (text *corpus*)
    (dolist (clause (chord:normalize-text text))
      (dolist (word (chord::split-on #\Space clause))
        (unless (sayable-word-p word)
          (push (list text clause word) bad)))))
  (check (null bad) "every word is letters, apostrophes and hyphens only"
         (format nil "~s" (first bad))))

(let ((bad '()))
  (dolist (text *corpus*)
    (dolist (clause (chord:normalize-text text))
      (when (or (zerop (length clause))
                (find #\Space clause :start 0 :end 1)
                (search "  " clause)
                (eql (char clause (1- (length clause))) #\Space))
        (push (list text clause) bad))))
  (check (null bad) "no clause is empty, doubly spaced or padded"
         (format nil "~s" (first bad))))

;; nothing with a digit in it disappears: every number in the corpus reaches the
;; output as at least one word
(let ((bad '()))
  (dolist (case '(("I paid 27 dollars" "twenty" "seven")
                  ("in 1984 he wrote" "nineteen" "eighty")
                  ("the 3rd of 5" "third" "five")
                  ("$4.50 each" "fifty" "cents")
                  ("50% done" "percent")
                  ("at 12:30 sharp" "thirty")
                  ("room 007 please" "zero")))
    (let ((out (format nil "~{~a~^ ~}" (chord:normalize-text (first case)))))
      (dolist (want (rest case))
        (unless (search want out) (push (list (first case) want out) bad)))))
  (check (null bad) "every number reaches the output as words"
         (format nil "~s" (first bad))))

;; normalising what came out changes nothing: the output really is words
(let ((bad '()))
  (dolist (text *corpus*)
    (dolist (clause (chord:normalize-text text))
      (let ((again (chord:normalize-text clause)))
        (unless (equal again (list clause))
          (push (list clause again) bad)))))
  (check (null bad) "normalising the output again is the identity"
         (format nil "~s" (first bad))))

;; the whole point: sentences arrive one at a time
(let ((out (chord:normalize-text "One. Two. Three.")))
  (check (equal out '("One" "Two" "Three")) "three sentences in, three clauses out"
         (format nil "~s" out)))

(format t "~&~%chord text gate: ~d passed, ~d failed, ~d known wrong~%"
        *pass* *fail* *known*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
