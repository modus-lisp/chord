;;;; text.lisp — turning written text into words that can be said.
;;;;
;;;;   (normalize-text TEXT) -> list of clause strings
;;;;
;;;; Each clause is a sentence-sized unit of synthesis, containing only words the
;;;; lexicon can be asked about: letters, apostrophes and internal hyphens,
;;;; separated by single spaces.  Everything else — digits, currency, symbols,
;;;; abbreviations, punctuation — has become words or a break by the time it
;;;; leaves here.  The rule that matters most is the last one: nothing is
;;;; DROPPED silently.  A number that disappears takes the meaning of the
;;;; sentence with it, and the listener never finds out.
;;;;
;;;; The shape of the thing:
;;;;
;;;;   canonicalize   smart quotes, dashes, accents folded to ASCII
;;;;   tokenize       whitespace chunks, each split into lead / core / trail
;;;;                  punctuation, so the core can be classified on its own
;;;;   expand         one token (sometimes two) -> a list of sayable words,
;;;;                  plus a policy saying what its trailing period means
;;;;   assemble       words accumulate into a clause; a break flushes it
;;;;
;;;; Several rules here can only be approximately right — an English period is
;;;; genuinely ambiguous, and so is `St.'  Each of those is commented where it is
;;;; decided, with the reason the chosen failure is the cheaper one to hear.

(in-package #:chord)

;;; ---- small helpers ---------------------------------------------------------

(defun ascii-digit-p (ch)
  (and (char<= #\0 ch) (char<= ch #\9)))

(defun has-digit-p (s) (some #'ascii-digit-p s))

(defun capitalized-p (s)
  (and s (plusp (length s)) (upper-case-p (char s 0))))

(defun all-upper-p (s)
  "Every letter in S is upper case, and there is at least one."
  (and (some #'alpha-char-p s)
       (every (lambda (c) (or (not (alpha-char-p c)) (upper-case-p c))) s)))

(defun word-only-p (s)
  "S is a word already: letters, apostrophes, hyphens, nothing else."
  (and (plusp (length s))
       (some #'alpha-char-p s)
       (every (lambda (c) (or (alpha-char-p c) (find c "'-"))) s)))

(defun ends-with-p (s suffix)
  (let ((n (length s)) (k (length suffix)))
    (and (>= n k) (string-equal suffix s :start2 (- n k)))))

(defun pluralize (word)
  "The plural of a word we made up here — regular, because everything we make up
is regular.  Only ever applied to number words (`ninety' -> `nineties')."
  (cond ((string= word "half") "halves")
        ((and (> (length word) 1) (eql (char word (1- (length word))) #\y))
         (concatenate 'string (subseq word 0 (1- (length word))) "ies"))
        (t (concatenate 'string word "s"))))

;;; ---- numbers ---------------------------------------------------------------

(defparameter +ones-words+
  #("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
    "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen"
    "eighteen" "nineteen"))

(defparameter +tens-words+
  #("" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))

(defparameter +scale-words+
  #("" "thousand" "million" "billion" "trillion" "quadrillion" "quintillion"
    "sextillion")
  "Short scale, as American English uses it.  Past a sextillion a number is not
being read as a quantity by anyone, so it is read as digits instead.")

(defun small-cardinal (n)
  "0 < N < 1000 as words.  American English, so no `and': 101 is `one hundred
one', not `one hundred and one'."
  (let ((out '()))
    (multiple-value-bind (h r) (floor n 100)
      (when (plusp h)
        (setf out (list (aref +ones-words+ h) "hundred")))
      (cond ((zerop r))
            ((< r 20) (setf out (append out (list (aref +ones-words+ r)))))
            (t (multiple-value-bind (te on) (floor r 10)
                 (setf out (append out (list (aref +tens-words+ te))))
                 (when (plusp on)
                   (setf out (append out (list (aref +ones-words+ on))))))))
      out)))

(defun digit-words (s &optional (zero "zero"))
  "S said one digit at a time — for identifiers, which are digits rather than a
quantity: a phone number, a room number, `007'."
  (loop for ch across s
        when (ascii-digit-p ch)
          collect (if (eql ch #\0) zero (aref +ones-words+ (- (char-code ch) 48)))))

(defun cardinal-words (n)
  (cond ((minusp n) (cons "minus" (cardinal-words (- n))))
        ((zerop n) (list "zero"))
        ((>= n (expt 1000 (length +scale-words+))) (digit-words (princ-to-string n)))
        (t (let ((groups '()) (out '()))
             (loop with m = n
                   for k from 0
                   while (plusp m)
                   do (multiple-value-bind (q r) (floor m 1000)
                        (push (cons k r) groups)
                        (setf m q)))
             (dolist (g groups out)
               (when (plusp (cdr g))
                 (setf out (append out (small-cardinal (cdr g))
                                   (let ((s (aref +scale-words+ (car g))))
                                     (unless (string= s "") (list s)))))))))))

(defparameter +ordinal-word+
  '(("zero" . "zeroth") ("one" . "first") ("two" . "second") ("three" . "third")
    ("four" . "fourth") ("five" . "fifth") ("six" . "sixth") ("seven" . "seventh")
    ("eight" . "eighth") ("nine" . "ninth") ("ten" . "tenth")
    ("eleven" . "eleventh") ("twelve" . "twelfth")))

(defun ordinal-of-word (w)
  (let ((hit (assoc w +ordinal-word+ :test #'string=)))
    (cond (hit (cdr hit))
          ((and (> (length w) 1) (eql (char w (1- (length w))) #\y))
           (concatenate 'string (subseq w 0 (1- (length w))) "ieth"))
          (t (concatenate 'string w "th")))))

(defun ordinal-words (n)
  "N as an ordinal: only the last word changes, which is how English does it —
`twenty first', `one hundred third'."
  (let ((words (cardinal-words n)))
    (append (butlast words) (list (ordinal-of-word (car (last words)))))))

(defun year-words (n)
  "A year said as a year: 1984 is `nineteen eighty four', not `one thousand nine
hundred eighty four'.  2005 is `two thousand five' — the pair-of-two-digits rule
breaks down for the first decade of a century and English switches to the long
form there."
  (cond ((or (< n 1000) (> n 9999)) (cardinal-words n))
        ((zerop (mod n 1000)) (cardinal-words n))
        ((<= 2000 n 2009)
         (append (list "two" "thousand") (small-cardinal (mod n 10))))
        ((zerop (mod n 100))
         (append (small-cardinal (floor n 100)) (list "hundred")))
        ((< (mod n 100) 10)
         (append (small-cardinal (floor n 100)) (list "oh")
                 (small-cardinal (mod n 100))))
        (t (append (small-cardinal (floor n 100)) (small-cardinal (mod n 100))))))

(defun decimal-words (body)
  "Digits after the point are said one at a time — 3.14 is `three point one
four', because `point fourteen' means something else."
  (let* ((dot (position #\. body))
         (int (subseq body 0 dot))
         (frac (subseq body (1+ dot))))
    (append (if (plusp (length int)) (cardinal-words (parse-integer int)) nil)
            (list "point")
            (if (plusp (length frac)) (digit-words frac) nil))))

(defun plain-number-p (s)
  "S is a number as written: digits, grouping commas, at most one decimal point,
an optional leading sign."
  (let ((body (if (and (plusp (length s)) (find (char s 0) "-+")) (subseq s 1) s)))
    (and (plusp (length body))
         (has-digit-p body)
         (every (lambda (c) (or (ascii-digit-p c) (find c ".,"))) body)
         (<= (count #\. body) 1))))

(defun number-words (s &key year)
  "A written number as words.  YEAR allows the year reading for a bare four
digit number; callers that know better — money, units, anything with grouping
commas — leave it off."
  (let* ((s (or s ""))
         (neg (and (plusp (length s)) (eql (char s 0) #\-)))
         (grouped (find #\, s))
         (body (remove #\, (if (and (plusp (length s)) (find (char s 0) "-+"))
                               (subseq s 1)
                               s)))
         (words
           (cond ((zerop (length body)) nil)
                 ((> (count #\. body) 1)
                  ;; a version number, 1.2.3 — each group its own number
                  (loop for part in (split-on #\. body)
                        for first = t then nil
                        append (append (unless first (list "point"))
                                       (if (plusp (length part))
                                           (cardinal-words (parse-integer part))
                                           nil))))
                 ((find #\. body) (decimal-words body))
                 ((and (> (length body) 1) (eql (char body 0) #\0))
                  ;; a leading zero means it is an identifier, not a quantity
                  (digit-words body))
                 ((and (not grouped) (>= (length body) 10)) (digit-words body))
                 ((and year (not grouped) (= (length body) 4)
                       (<= 1100 (parse-integer body) 2099))
                  (year-words (parse-integer body)))
                 (t (cardinal-words (parse-integer body))))))
    (if neg (cons "minus" words) words)))

;;; ---- letters ---------------------------------------------------------------
;;;
;;; A spelled-out letter is emitted as the bare letter, because CMUdict lists all
;;; twenty six of them with their letter NAMES (`w' is D AH1 B AH0 L Y UW0).  The
;;; one exception is `a', whose dictionary entry is the unstressed article — a
;;; schwa, which inside F.B.I. would be wrong — so that one is spelled `ay'.

(defun letter-name (ch)
  (if (char-equal ch #\a) "ay" (string (char-upcase ch))))

(defparameter +letter-names+
  '((#\a . "ay") (#\b . "bee") (#\c . "cee") (#\d . "dee") (#\e . "ee")
    (#\f . "ef") (#\g . "gee") (#\h . "aitch") (#\i . "eye") (#\j . "jay")
    (#\k . "kay") (#\l . "el") (#\m . "em") (#\n . "en") (#\o . "oh")
    (#\p . "pee") (#\q . "cue") (#\r . "ar") (#\s . "ess") (#\t . "tee")
    (#\u . "you") (#\v . "vee") (#\w . "double u") (#\x . "ex") (#\y . "why")
    (#\z . "zee"))
  "Letter names spelled out as words — needed only for the plural of a spelled
acronym (`CDs' -> `C dees'), where the bare letter cannot take the s.")

(defun plural-letter-name (ch)
  (let ((hit (cdr (assoc (char-downcase ch) +letter-names+))))
    (if hit (pluralize hit) (string (char-upcase ch)))))

(defun spell-letters (s)
  (loop for ch across s when (alpha-char-p ch) collect (letter-name ch)))

;;; ---- acronyms --------------------------------------------------------------
;;;
;;; NASA is a word, FBI is three letters.  The best oracle for which is which is
;;; the dictionary itself: CMUdict lists `nasa' as N AE1 S AH0 and `fbi' as EH1 F
;;; B IY1 AY1 — it has already made the call, for hundreds of them.  So the
;;; dictionary is asked first, and the spelling rules below are the fallback for
;;; the ones it has never heard of.
;;;
;;; The fallback spells a capitalised token out when it cannot be a word: no
;;; vowel (PNG), only two letters (AI), more vowels than consonants (ETA — an
;;; acronym said as a word needs consonants to hang the syllables on), or a
;;; consonant cluster English does not start or end a word with (FBI's `fb').
;;;
;;; The deliberate failure: an ALL-CAPS common word is read as a word, because
;;; the dictionary knows it.  So SHOUTED prose survives, and `US' comes out as
;;; the pronoun.  Written `U.S.' — where the author's periods say what is meant —
;;; it comes out as two letters.  Emphasis is more common in the text a desktop
;;; reads aloud than the country's bare initials, so that is the way round it is.

(defparameter +spell-out-acronyms+
  '("HIV" "NAACP" "AAA" "IOU" "IEEE" "JPEG" "PNG" "LLC" "LLP" "MRSA")
  "Said as letters even though a rule might not think so.")

(defparameter +word-acronyms+
  '("NASA" "NATO" "SCUBA" "LASER" "RADAR" "SONAR" "OPEC" "UNESCO" "UNICEF"
    "AIDS" "ASCII" "NIMBY" "NAFTA" "GIF" "MIDI" "SIM")
  "Said as a word even though a rule might not think so.  Mostly a safety net for
running without the dictionary; CMUdict knows nearly all of these.")

(defparameter +onsets+
  '("BL" "BR" "CH" "CL" "CR" "DR" "DW" "FL" "FR" "GL" "GR" "GH" "KL" "KN" "KR"
    "PH" "PL" "PR" "PS" "QU" "RH" "SC" "SH" "SK" "SL" "SM" "SN" "SP" "ST" "SW"
    "TH" "TR" "TW" "VR" "WH" "WR" "CHR" "PHR" "SCH" "SCR" "SHR" "SPH" "SPL"
    "SPR" "SQU" "STR" "THR")
  "Consonant clusters an English word can begin with.")

(defparameter +codas+
  '("CT" "FT" "LB" "LD" "LF" "LK" "LM" "LN" "LP" "LS" "LT" "MB" "MP" "MN" "ND"
    "NG" "NK" "NS" "NT" "PT" "RB" "RD" "RK" "RL" "RM" "RN" "RP" "RS" "RT" "SH"
    "SK" "SP" "ST" "TH" "CH" "CK" "LL" "SS" "FF" "ZZ" "TS" "DS" "MS" "PS" "GS"
    "BS" "KS" "NCH" "NDS" "NGS" "NTS" "CKS" "LTS" "MPS" "RTS" "STS")
  "Consonant clusters an English word can end with.")

(defun consonant-runs (s)
  "S as (RUN START END) triples of consecutive non-vowels."
  (let ((out '()) (i 0) (n (length s)))
    (loop while (< i n)
          do (if (find (char s i) "AEIOU")
                 (incf i)
                 (let ((j (or (position-if (lambda (c) (find c "AEIOU")) s :start i) n)))
                   (push (list (subseq s i j) i j) out)
                   (setf i j))))
    (nreverse out)))

(defun syllable-shaped-p (s)
  "S could be a word: its first and last consonant clusters are ones English
allows there, and nothing in the middle is unpronounceably long."
  (let ((s (string-upcase s)))
    (loop for (run start end) in (consonant-runs s)
          always (let ((k (length run)))
                   (cond ((= k 1) t)
                         ((zerop start) (member run +onsets+ :test #'string=))
                         ((= end (length s)) (member run +codas+ :test #'string=))
                         (t (<= k 4)))))))

(defvar *acronym-dictionary* t
  "Whether the pronunciation dictionary may be consulted to decide whether an
acronym is a word.  Off makes normalisation standalone, and a little worse.")

(defun dict-known-p (word)
  (and *acronym-dictionary*
       (handler-case (and (dict-entry word) t) (error () nil))))

(defun spell-out-p (s)
  (let* ((n (length s))
         (v (count-if (lambda (c) (find (char-upcase c) "AEIOU")) s)))
    (or (< n 3)
        (zerop v)
        (> (* 5 v) (* 3 n))
        (not (syllable-shaped-p s)))))

(defun acronym-words (base &optional plural)
  "BASE is an all-capitals token; PLURAL if a lower case s was taken off it."
  (let* ((low (string-downcase base))
         (lows (concatenate 'string low "s")))
    (cond ((member base +spell-out-acronyms+ :test #'string-equal)
           (spelled-acronym base plural))
          ((member base +word-acronyms+ :test #'string-equal)
           (list (if plural lows low)))
          ((and plural (dict-known-p lows)) (list lows))
          ((dict-known-p low) (list (if plural lows low)))
          ((spell-out-p base) (spelled-acronym base plural))
          (t (list (if plural lows low))))))

(defun spelled-acronym (base plural)
  (let ((letters (spell-letters base)))
    (if (and plural letters)
        (append (butlast letters)
                (list (plural-letter-name (char base (1- (length base))))))
        letters)))

(defun acronym-plural-p (s)
  "PCs, URLs, CDs — capitals with a lower case plural s stuck on."
  (let ((n (length s)))
    (and (>= n 3)
         (eql (char s (1- n)) #\s)
         (all-upper-p (subseq s 0 (1- n)))
         (every #'alpha-char-p (subseq s 0 (1- n))))))

(defun expand-alpha-string (s)
  "A run of letters as words, deciding acronym from word."
  (cond ((zerop (length s)) nil)
        ((and (= 1 (length s)) (upper-case-p (char s 0)))
         ;; a lone capital is a word only if it is `A' or `I'
         (if (or (string= s "A") (string= s "I")) (list s) (list (letter-name (char s 0)))))
        ((all-upper-p s) (acronym-words s))
        ((acronym-plural-p s) (acronym-words (subseq s 0 (1- (length s))) t))
        ;; no vowel and no dictionary entry: `www', `mfg' — a word it is not
        ((and (>= (length s) 2)
              (every #'alpha-char-p s)
              (notany (lambda (c) (find (char-downcase c) "aeiouy")) s)
              (not (dict-known-p (string-downcase s))))
         (spell-letters s))
        (t (list s))))

;;; ---- money, units, symbols -------------------------------------------------
;;;
;;; Non-ASCII characters are written with CODE-CHAR rather than as literals so
;;; that this file says the same thing whatever encoding it is read in.

(defparameter +currency+
  (list (list #\$ "dollar" "dollars" "cent" "cents")
        (list (code-char #x00A3) "pound" "pounds" "penny" "pence")
        (list (code-char #x20AC) "euro" "euros" "cent" "cents")
        (list (code-char #x00A5) "yen" "yen" "sen" "sen")
        (list (code-char #x00A2) "cent" "cents" "cent" "cents")))

(defun currency-entry (ch) (assoc ch +currency+))

(defun currency-name (entry n &optional fraction)
  "The name of the unit, singular exactly when there is one of it."
  (if fraction
      (if (= n 1) (fourth entry) (fifth entry))
      (if (= n 1) (second entry) (third entry))))

(defparameter +units+
  '(("km" "kilometer" "kilometers" t)   ("cm" "centimeter" "centimeters" t)
    ("mm" "millimeter" "millimeters" t) ("nm" "nanometer" "nanometers" t)
    ("m" "meter" "meters" t)            ("mi" "mile" "miles" t)
    ("ft" "foot" "feet" t)              ("yd" "yard" "yards" t)
    ("in" "inch" "inches" nil)          ("kg" "kilogram" "kilograms" t)
    ("mg" "milligram" "milligrams" t)   ("g" "gram" "grams" t)
    ("lb" "pound" "pounds" t)           ("lbs" "pounds" "pounds" t)
    ("oz" "ounce" "ounces" t)           ("ml" "milliliter" "milliliters" t)
    ("mph" "mile per hour" "miles per hour" t)
    ("kph" "kilometer per hour" "kilometers per hour" t)
    ("hz" "hertz" "hertz" t)            ("khz" "kilohertz" "kilohertz" t)
    ("mhz" "megahertz" "megahertz" t)   ("ghz" "gigahertz" "gigahertz" t)
    ("kb" "kilobyte" "kilobytes" t)     ("mb" "megabyte" "megabytes" t)
    ("gb" "gigabyte" "gigabytes" t)     ("tb" "terabyte" "terabytes" t)
    ("ms" "millisecond" "milliseconds" t)
    ("sec" "second" "seconds" t)        ("secs" "seconds" "seconds" t)
    ("min" "minute" "minutes" nil)      ("mins" "minutes" "minutes" t)
    ("hr" "hour" "hours" t)             ("hrs" "hours" "hours" t)
    ("yr" "year" "years" t)             ("yrs" "years" "years" t)
    ("km/h" "kilometer per hour" "kilometers per hour" t)
    ("km/hr" "kilometer per hour" "kilometers per hour" t)
    ("mi/h" "mile per hour" "miles per hour" t)
    ("m/s" "meter per second" "meters per second" t)
    ("ft/s" "foot per second" "feet per second" t)
    ("mb/s" "megabit per second" "megabits per second" t)
    ("kb/s" "kilobit per second" "kilobits per second" t))
  "(ABBREVIATION SINGULAR PLURAL STANDS-ALONE).  STANDS-ALONE says whether the
abbreviation may be expanded when it is a word of its own after a number: `5 in'
is usually the preposition and `20 min' usually reads fine as written, but `5km'
written closed up is never anything but a distance.")

(defun unit-entry (s &optional attached)
  (let ((hit (assoc s +units+ :test #'string-equal)))
    (and hit (or attached (fourth hit)) hit)))

(defun unit-words (entry n)
  (let ((words (if (and n (= (abs n) 1)) (second entry) (third entry))))
    (split-on #\Space words)))

(defparameter +months+
  #("January" "February" "March" "April" "May" "June" "July" "August"
    "September" "October" "November" "December"))

(defun month-number (s)
  "0-based month for a month name or its usual abbreviation (`Sept', `Jan'), or
NIL.  Three letters is the shortest that is unambiguous."
  (and s (>= (length s) 3)
       (position-if (lambda (m)
                      (and (<= (length s) (length m))
                           (string-equal s m :end2 (length s))))
                    +months+)))

(defun symbol-words (ch prev next)
  "One symbol as words.  PREV and NEXT are the run kinds either side (:digit,
:alpha or NIL), because a symbol's reading depends on what it sits between."
  (let ((code (char-code ch)))
    (cond ((eql ch #\&) (list "and"))
          ((eql ch #\@) (list "at"))
          ((eql ch #\%) (list "percent"))
          ((eql ch #\+) (list "plus"))
          ((eql ch #\=) (list "equals"))
          ((eql ch #\<) (list "less" "than"))
          ((eql ch #\>) (list "greater" "than"))
          ((eql ch #\#) (if (eq next :digit) (list "number") (list "hash")))
          ((eql ch #\/) (list "slash"))
          ((eql ch #\\) (list "backslash"))
          ((eql ch #\.) (cond ((and (eq prev :digit) (eq next :digit)) (list "point"))
                              ((and prev next) (list "dot"))
                              (t nil)))
          ((eql ch #\*) (if (and (eq prev :digit) (eq next :digit)) (list "times") nil))
          ((eql ch #\~) (if (eq next :digit) (list "about") nil))
          ((eql ch #\_) nil)
          ;; a hyphen between two numbers is a range: 20-30% is twenty TO thirty
          ((eql ch #\-) (when (and (eq prev :digit) (eq next :digit)) (list "to")))
          ((eql ch #\,) nil)
          ((eql ch #\') nil)
          ((= code #x00B0) (list "degrees"))
          ((= code #x00D7) (list "times"))
          ((= code #x00F7) (list "divided" "by"))
          ((= code #x00B1) (list "plus" "or" "minus"))
          ((= code #x2260) (list "not" "equal" "to"))
          ((= code #x2248) (list "approximately"))
          ((= code #x2264) (list "less" "than" "or" "equal" "to"))
          ((= code #x2265) (list "greater" "than" "or" "equal" "to"))
          ((= code #x00A9) (list "copyright"))
          ((= code #x00A7) (list "section"))
          ((currency-entry ch) (list (third (currency-entry ch))))
          (t nil))))

;;; ---- abbreviations ---------------------------------------------------------
;;;
;;; The interesting half of the problem is not the expansion, it is the period.
;;; `Mr. Smith' has a period that does not end a sentence; `books, etc. The rest'
;;; has one that does; and nothing in the character stream distinguishes them.
;;; Each entry therefore carries a CLASS saying what its period means:
;;;
;;;   :never   an abbreviation that is never the last word of a sentence — `e.g.'
;;;   :title   a title, which is followed by a name: `Dr. Smith'.  Ends a
;;;            sentence only at the very end of the text.
;;;   :maybe   ends the sentence when the next word is capitalised, on the
;;;            assumption that the writer capitalises after a full stop.  Wrong
;;;            for `Acme Inc. Ltd.' and for a proper noun after `etc.'; right for
;;;            the overwhelmingly more common case.
;;;
;;; NEEDS-DOT keeps a word that happens to be spelled like an abbreviation from
;;; being expanded when it is just that word (`no', `co', `in').  NEEDS-CAP does
;;; the same job for `May' and `March', which are months only when capitalised.

(defparameter +abbreviations+
  '(;; titles — the word after them is a name
    ("mr" ("mister") :title nil t)      ("mrs" ("missus") :title nil t)
    ("ms" ("miz") :title nil t)         ("dr" :dr :title nil t)
    ("prof" ("professor") :title nil t) ("st" :st :title t t)
    ("mt" ("mount") :title t t)         ("ft" ("fort") :title t t)
    ("fr" ("father") :title t t)        ("rev" ("reverend") :title t t)
    ("hon" ("honorable") :title t t)    ("gov" ("governor") :title t t)
    ("sen" ("senator") :title t t)      ("rep" ("representative") :title t t)
    ("pres" ("president") :title t t)   ("gen" ("general") :title t t)
    ("col" ("colonel") :title t t)      ("sgt" ("sergeant") :title t t)
    ("cpl" ("corporal") :title t t)     ("capt" ("captain") :title t t)
    ("lt" ("lieutenant") :title t t)    ("maj" ("major") :title t t)
    ("adm" ("admiral") :title t t)      ("atty" ("attorney") :title t t)
    ("msgr" ("monsignor") :title t t)
    ;; never the end of a sentence
    ("e.g" ("E" "G") :never t nil)      ("i.e" ("I" "E") :never t nil)
    ("vs" ("versus") :never t nil)      ("v" ("versus") :never t t)
    ("cf" ("compare") :never t nil)     ("viz" ("namely") :never t nil)
    ("al" ("al") :never t nil)          ("et" ("et") :never nil nil)
    ("approx" ("approximately") :never t nil)
    ("ca" ("circa") :never t nil)       ("a.k.a" ("A" "K" "A") :never t nil)
    ("aka" ("A" "K" "A") :never t nil)
    ;; may or may not be the end of a sentence
    ("etc" ("etcetera") :maybe t nil)   ("inc" ("incorporated") :maybe t t)
    ("ltd" ("limited") :maybe t t)      ("co" ("company") :maybe t t)
    ("corp" ("corporation") :maybe t t) ("dept" ("department") :maybe t t)
    ("est" ("established") :maybe t nil) ("fig" ("figure") :maybe t nil)
    ("vol" ("volume") :maybe t nil)     ("pp" ("pages") :maybe t nil)
    ("no" :no :maybe t nil)
    ("jr" ("junior") :maybe t t)        ("sr" ("senior") :maybe t t)
    ("ave" ("avenue") :maybe t t)       ("blvd" ("boulevard") :maybe t t)
    ("rd" ("road") :maybe t t)          ("ln" ("lane") :maybe t t)
    ("ct" ("court") :maybe t t)         ("sq" ("square") :maybe t t)
    ("apt" ("apartment") :maybe t t)    ("dept" ("department") :maybe t t)
    ("ph.d" ("P" "H" "D") :maybe t t)   ("phd" ("P" "H" "D") :maybe t t)
    ("a.m" ("A" "M") :maybe t nil)      ("p.m" ("P" "M") :maybe t nil)
    ("u.s" ("U" "S") :maybe t t)        ("u.k" ("U" "K") :maybe t t)
    ("u.s.a" ("U" "S" "A") :maybe t t)  ("d.c" ("D" "C") :maybe t t)
    ("b.c" ("B" "C") :maybe t t)        ("a.d" ("A" "D") :maybe t t)
    ;; months and days
    ("jan" ("January") :maybe t t)      ("feb" ("February") :maybe t t)
    ("mar" ("March") :maybe t t)        ("apr" ("April") :maybe t t)
    ("jun" ("June") :maybe t t)         ("jul" ("July") :maybe t t)
    ("aug" ("August") :maybe t t)       ("sep" ("September") :maybe t t)
    ("sept" ("September") :maybe t t)   ("oct" ("October") :maybe t t)
    ("nov" ("November") :maybe t t)     ("dec" ("December") :maybe t t)
    ("mon" ("Monday") :maybe t t)       ("tue" ("Tuesday") :maybe t t)
    ("tues" ("Tuesday") :maybe t t)     ("wed" ("Wednesday") :maybe t t)
    ("thu" ("Thursday") :maybe t t)     ("thurs" ("Thursday") :maybe t t)
    ("fri" ("Friday") :maybe t t)       ("sat" ("Saturday") :maybe t t)))

(defun abbreviation-entry (core dotted)
  "The table entry for CORE, or NIL.  DOTTED says whether a period followed it."
  (let ((hit (assoc core +abbreviations+ :test #'string-equal)))
    (when (and hit
               (or dotted (not (fourth hit)))
               (or (not (fifth hit)) (capitalized-p core))
               ;; ALL CAPITALS is an acronym, not an abbreviation: `EST.' at the
               ;; end of a time is the zone, not `established'.  Initialisms are
               ;; exempt because their periods are inside them — `U.S.'
               (not (and (all-upper-p core)
                         (> (count-if #'alpha-char-p core) 1)
                         (not (find #\. core)))))
      hit)))

;;; ---- canonicalising --------------------------------------------------------

(defparameter +char-fixes+
  (let ((table (make-hash-table)))
    (flet ((fix (code string) (setf (gethash (code-char code) table) string)))
      ;; quotation marks: the right single quote is an apostrophe in `don't', so
      ;; it becomes one; the rest become space, which separates rather than joins
      (fix #x2018 "'") (fix #x2019 "'") (fix #x201B "'") (fix #x02BC "'")
      (fix #x201C " ") (fix #x201D " ") (fix #x00AB " ") (fix #x00BB " ")
      (fix #x2039 " ") (fix #x203A " ") (fix #x201E " ")
      ;; dashes: an en dash joins a range (1-5), an em dash separates a clause
      (fix #x2013 "-") (fix #x2014 " ") (fix #x2015 " ") (fix #x2212 "-")
      (fix #x2026 "...") (fix #x00A0 " ") (fix #x2007 " ") (fix #x202F " ")
      (fix #x2009 " ") (fix #x200B "") (fix #xFEFF "")
      (fix #x00BD " one half ") (fix #x00BC " one quarter ")
      (fix #x00BE " three quarters ")
      ;; letters with diacritics: the letter-to-sound rules only know ASCII, and
      ;; `cafe' said plainly beats `caf' with the last letter dropped
      (loop for (code plain) in '((#x00E0 "a") (#x00E1 "a") (#x00E2 "a") (#x00E3 "a")
                                  (#x00E4 "a") (#x00E5 "a") (#x00E6 "ae") (#x00E7 "c")
                                  (#x00E8 "e") (#x00E9 "e") (#x00EA "e") (#x00EB "e")
                                  (#x00EC "i") (#x00ED "i") (#x00EE "i") (#x00EF "i")
                                  (#x00F0 "d") (#x00F1 "n") (#x00F2 "o") (#x00F3 "o")
                                  (#x00F4 "o") (#x00F5 "o") (#x00F6 "o") (#x00F8 "o")
                                  (#x00F9 "u") (#x00FA "u") (#x00FB "u") (#x00FC "u")
                                  (#x00FD "y") (#x00FE "th") (#x00FF "y") (#x00DF "ss")
                                  (#x0153 "oe") (#x0152 "OE") (#x0161 "s") (#x017E "z"))
            do (fix code plain)
               (let ((up (char-upcase (code-char code))))
                 (unless (eql up (code-char code))
                   (setf (gethash up table) (string-upcase plain))))))
    table)
  "Characters that are replaced before anything else looks at the text.")

(defun canonicalize (text)
  "TEXT with typographic characters folded onto the ones the rest of this file
knows about."
  (if (every (lambda (ch) (< (char-code ch) 128)) text)
      text
      (with-output-to-string (out)
        (loop for ch across text
              for fix = (gethash ch +char-fixes+)
              do (cond (fix (write-string fix out))
                       ((and (>= (char-code ch) 128) (not (alpha-char-p ch))
                             (null (symbol-words ch nil nil)))
                        ;; an unknown symbol says nothing, but must not glue the
                        ;; words on either side of it together
                        (write-char #\Space out))
                       (t (write-char ch out)))))))

;;; ---- tokenising ------------------------------------------------------------

(defstruct (tok (:constructor make-tok (raw lead core trail)))
  raw lead core trail)

(defparameter +space-chars+ '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed))

(defun lead-punctuation-p (ch next)
  (or (find ch "\"'([{`*_|")
      ;; a leading minus belongs to the number, or to the money, after it
      (and (eql ch #\-) (not (and next (or (ascii-digit-p next) (currency-entry next)))))
      (and (eql ch #\.) (not (and next (ascii-digit-p next))))
      (find ch ",;:!?")))

(defun trail-punctuation-p (ch quoted)
  (or (find ch "\")]}`*_|,.!?;:")
      (eql ch #\-)
      (and quoted (eql ch #\'))))

(defun split-lead-and-trail (raw)
  "RAW as (LEAD CORE TRAIL): the punctuation before it, the thing itself, and the
punctuation after it.  A trailing period stays in TRAIL even when it belongs to
an abbreviation — whether it does is decided later, with the next token in hand."
  (let* ((n (length raw))
         (start 0)
         (end n))
    (loop while (and (< start end)
                     (lead-punctuation-p (char raw start)
                                         (when (< (1+ start) end) (char raw (1+ start)))))
          do (incf start))
    (let ((quoted (find-if (lambda (c) (find c "\"'`")) raw :end start)))
      (loop while (and (> end start) (trail-punctuation-p (char raw (1- end)) quoted))
            do (decf end)))
    (list (subseq raw 0 start) (subseq raw start end) (subseq raw end))))

(defun presplit-run-on (chunk)
  "CHUNK split where a period is a full stop with the space left out —
`end.Next'.  A short left side is left alone, because that is `Ph.D' and `U.S.A'
rather than a missing space."
  (let ((out '()) (start 0))
    (loop for i from 1 below (1- (length chunk))
          do (when (and (eql (char chunk i) #\.)
                        (alpha-char-p (char chunk (1- i)))
                        (upper-case-p (char chunk (1+ i)))
                        (>= (- i start) 3)
                        (not (find #\. (subseq chunk start i)))
                        (not (abbreviation-entry (subseq chunk start i) t)))
               (push (subseq chunk start (1+ i)) out)
               (setf start (1+ i))))
    (push (subseq chunk start) out)
    (nreverse out)))

(defun tokenize (text)
  "TEXT as a vector of tokens."
  (let ((toks '()))
    (dolist (chunk (remove "" (split-on-any (canonicalize text) +space-chars+)
                           :test #'string=))
      (dolist (piece (presplit-run-on chunk))
        (when (plusp (length piece))
          (destructuring-bind (lead core trail) (split-lead-and-trail piece)
            (push (make-tok piece lead core trail) toks)))))
    (coerce (nreverse toks) 'vector)))

(defun core-at (toks i)
  (when (and (>= i 0) (< i (length toks))) (tok-core (aref toks i))))

(defun trail-at (toks i)
  (when (and (>= i 0) (< i (length toks))) (tok-trail (aref toks i))))

;;; ---- anything else: run by run ---------------------------------------------
;;;
;;; The safety net.  Whatever has no better reading is cut into runs of letters,
;;; runs of digits and single symbols, and each is said in turn.  It is what
;;; makes an email address or a product code come out as words rather than as
;;; nothing at all, and it is why nothing can leave here unsaid.

(defun scan-runs (s)
  (let ((out '()) (i 0) (n (length s)))
    (loop while (< i n)
          do (let ((ch (char s i)))
               (cond ((alpha-char-p ch)
                      (let ((j (or (position-if-not #'alpha-char-p s :start i) n)))
                        (push (cons :alpha (subseq s i j)) out)
                        (setf i j)))
                     ((ascii-digit-p ch)
                      (let ((j (or (position-if-not #'ascii-digit-p s :start i) n)))
                        (push (cons :digit (subseq s i j)) out)
                        (setf i j)))
                     (t (push (cons :sym ch) out)
                        (incf i)))))
    (nreverse out)))

(defparameter +temperature-scales+
  '(("F" . "fahrenheit") ("C" . "celsius") ("K" . "kelvin")))

(defun expand-runs (s)
  (let* ((v (coerce (scan-runs s) 'vector))
         (out '())
         (last-number nil))
    (dotimes (k (length v))
      (let* ((kind (car (aref v k)))
             (value (cdr (aref v k)))
             (prev-kind (when (plusp k) (car (aref v (1- k)))))
             (next-kind (when (< (1+ k) (length v)) (car (aref v (1+ k)))))
             (previous (car (last out)))
             (words
               (case kind
                 (:digit (setf last-number (parse-integer value :junk-allowed t))
                         (number-words value :year t))
                 (:alpha
                  (let ((unit (and (eq prev-kind :digit) (unit-entry value t)))
                        (mer (and (eq prev-kind :digit) (meridiem-words value)))
                        (scale (and (equal previous "degrees")
                                    (cdr (assoc value +temperature-scales+
                                                :test #'string-equal)))))
                    (cond (scale (list scale))
                          (mer mer)
                          (unit (unit-words unit last-number))
                          ;; an ordinal ending written up against its number, as
                          ;; in `3rd-party': the number said before it becomes
                          ;; the ordinal rather than the suffix becoming a word
                          ((and (eq prev-kind :digit) out
                                (member value '("st" "nd" "rd" "th") :test #'string-equal))
                           (setf out (append (butlast out)
                                             (list (ordinal-of-word (car (last out))))))
                           nil)
                          ;; capitals inside a token that is not all letters are
                          ;; initials far more often than they are a word: AT&T
                          ((and (all-upper-p value) (spell-out-p value))
                           (spell-letters value))
                          (t (expand-alpha-string value)))))
                 (t (symbol-words value prev-kind next-kind)))))
        (setf out (append out words))))
    out))

;;; ---- numbers in context ----------------------------------------------------

(defun all-digits-p (s) (and (plusp (length s)) (every #'ascii-digit-p s)))

(defun ordinal-suffix-number (s)
  "21st -> 21."
  (let ((n (length s)))
    (when (> n 2)
      (let ((tail (subseq s (- n 2)))
            (head (subseq s 0 (- n 2))))
        (when (and (member tail '("st" "nd" "rd" "th") :test #'string-equal)
                   (all-digits-p head))
          (parse-integer head))))))

(defun decade-number (s)
  "1990s, 90s, 1990's -> 1990, 90, 1990."
  (let ((body (cond ((ends-with-p s "'s") (subseq s 0 (- (length s) 2)))
                    ((ends-with-p s "s") (subseq s 0 (1- (length s))))
                    (t nil))))
    (when (and body (>= (length body) 2) (all-digits-p body))
      (parse-integer body))))

(defun decade-words (n four-digit)
  (let ((words (if four-digit (year-words n) (cardinal-words n))))
    (append (butlast words) (list (pluralize (car (last words)))))))

(defun time-parts (s)
  "12:30 and 1:23:45 as a list of integers, or NIL."
  (let ((parts (split-on #\: s)))
    (when (and (<= 2 (length parts) 3)
               (every #'all-digits-p parts)
               (<= (length (first parts)) 2)
               (= 2 (length (second parts)))
               (or (null (third parts)) (= 2 (length (third parts)))))
      (mapcar #'parse-integer parts))))

(defun time-words (parts)
  "A clock time or, with seconds, a duration — which is what three fields
almost always is."
  (let ((h (first parts)) (m (second parts)) (s (third parts)))
    (if s
        (append (cardinal-words h) (list (if (= h 1) "hour" "hours"))
                (cardinal-words m) (list (if (= m 1) "minute" "minutes"))
                (cardinal-words s) (list (if (= s 1) "second" "seconds")))
        (cond ((zerop m) (append (cardinal-words h) (list "o'clock")))
              ((< m 10) (append (cardinal-words h) (list "oh") (cardinal-words m)))
              (t (append (cardinal-words h) (cardinal-words m)))))))

(defun meridiem-words (s)
  (let ((k (remove #\. (string-downcase (or s "")))))
    (cond ((string= k "am") (list "A" "M"))
          ((string= k "pm") (list "P" "M")))))

(defun date-words (m d &optional y)
  (append (if (<= 1 m 12) (list (aref +months+ (1- m))) (cardinal-words m))
          (if (<= 1 d 31) (ordinal-words d) (cardinal-words d))
          (when y (year-words y))))

(defparameter +fraction-denominators+ '(2 3 4 5 6 7 8 9 10 12 16 32 64 100)
  "Denominators common enough that `3/4' is more likely a fraction than a date.
Anything else two-part — 9/11, 12/25 — is read as a date.")

(defun fraction-words (num den)
  (let ((name (case den
                (2 "half")
                (4 "quarter")
                (t (ordinal-of-word (car (last (cardinal-words den))))))))
    (append (cardinal-words num)
            (list (if (= num 1) name (pluralize name))))))

(defun money-words (entry numeric scale)
  (let* ((neg (and (plusp (length numeric)) (eql (char numeric 0) #\-)))
         (body (remove #\, (if neg (subseq numeric 1) numeric)))
         (dot (position #\. body))
         (whole (if dot (subseq body 0 dot) body))
         (frac (when dot (subseq body (1+ dot))))
         (value (or (parse-integer whole :junk-allowed t) 0))
         (words
           (cond (scale (append (number-words body) (list scale) (list (third entry))))
                 ((and frac (= 2 (length frac)))
                  (let ((cents (parse-integer frac)))
                    (cond ((and (zerop value) (plusp cents))
                           (append (cardinal-words cents)
                                   (list (currency-name entry cents t))))
                          ((zerop cents)
                           (append (cardinal-words value) (list (currency-name entry value))))
                          (t (append (cardinal-words value)
                                     (list (currency-name entry value)) (list "and")
                                     (cardinal-words cents)
                                     (list (currency-name entry cents t)))))))
                 (dot (append (number-words body) (list (third entry))))
                 (t (append (number-words body) (list (currency-name entry value)))))))
    (if neg (cons "minus" words) words)))

(defparameter +scale-following+
  '("hundred" "thousand" "million" "billion" "trillion" "quadrillion")
  "Words that carry a number written before them, so that `$5 million' puts the
dollars at the end where they are said.")

(defun expand-numeric (toks i)
  "A token with a digit in it -> (values WORDS CONSUMED POLICY).  A leading plus
is said and set aside, so that +1-555-0100 is still a telephone number."
  (let ((core (tok-core (aref toks i))))
    (if (and (> (length core) 1) (eql (char core 0) #\+))
        (multiple-value-bind (words consumed policy)
            (expand-numeric-core toks i (subseq core 1))
          (values (cons "plus" words) consumed policy))
        (expand-numeric-core toks i core))))

(defun money-range (entry rest)
  "$20-$30 and $20-30 — a range, which is two numbers and one currency."
  (let ((parts (split-on #\- rest)))
    (when (= 2 (length parts))
      (let* ((a (first parts))
             (b (second parts))
             (b (if (and (plusp (length b)) (currency-entry (char b 0))) (subseq b 1) b)))
        (when (and (plain-number-p a) (plain-number-p b))
          (append (number-words a) (list "to") (number-words b)
                  (list (third entry))))))))

(defun expand-numeric-core (toks i core)
  (let ((n (length core)))
    (block nil
      ;; money: $4.50, -$3, $5 million
      (let* ((neg (eql (char core 0) #\-))
             (at (if neg 1 0))
             (entry (and (> n (1+ at)) (currency-entry (char core at)))))
        (when entry
          (let* ((rest (concatenate 'string (if neg "-" "") (subseq core (1+ at))))
                 (next (core-at toks (1+ i)))
                 (scale (and next (member (string-downcase next) +scale-following+
                                          :test #'string=)
                             (string-downcase next))))
            (return (cond ((plain-number-p rest)
                           (values (money-words entry rest scale) (if scale 2 1) nil))
                          ((money-range entry rest)
                           (values (money-range entry rest) 1 nil))
                          (t (values (append (expand-runs rest) (list (third entry)))
                                     1 nil)))))))
      ;; percent: 50%, 0.5%
      (when (and (> n 1) (eql (char core (1- n)) #\%)
                 (plain-number-p (subseq core 0 (1- n))))
        (return (values (append (number-words (subseq core 0 (1- n))) (list "percent"))
                        1 nil)))
      ;; clock time: 12:30, 5:30pm, and 5:30 p.m. where the meridiem is its own
      ;; token — which is also a word whose period does not end the sentence
      (let* ((letters (position-if #'alpha-char-p core))
             (head (if letters (subseq core 0 letters) core))
             (tail (when letters (subseq core letters)))
             (parts (time-parts head)))
        (when parts
          (let ((attached (and tail (meridiem-words tail))))
            (cond (attached (return (values (append (time-words parts) attached) 1 nil)))
                  (tail nil)
                  (t (let ((following (meridiem-words (core-at toks (1+ i)))))
                       (if following
                           (return (values (append (time-words parts) following) 2 :maybe))
                           (return (values (time-words parts) 1 nil)))))))))
      ;; ordinals: 3rd, 21st
      (let ((ord (ordinal-suffix-number core)))
        (when ord (return (values (ordinal-words ord) 1 nil))))
      ;; decades: the 1990s, the 90s
      (let ((dec (decade-number core)))
        (when dec
          (return (values (decade-words dec (>= (length core) 5)) 1 nil))))
      ;; slashes: a fraction, a date, or neither
      (let ((parts (split-on #\/ core)))
        (when (and (>= (length parts) 2) (every #'all-digits-p parts))
          (let ((nums (mapcar #'parse-integer parts)))
            (cond ((and (= 2 (length parts))
                        (member (second nums) +fraction-denominators+)
                        (< (first nums) (second nums)))
                   (return (values (fraction-words (first nums) (second nums)) 1 nil)))
                  ((and (= 2 (length parts)) (<= 1 (first nums) 12) (<= 1 (second nums) 31))
                   (return (values (date-words (first nums) (second nums)) 1 nil)))
                  ((and (= 3 (length parts)) (= 4 (length (third parts)))
                        (<= 1 (first nums) 12) (<= 1 (second nums) 31))
                   (return (values (date-words (first nums) (second nums) (third nums))
                                   1 nil)))
                  ((and (= 3 (length parts)) (= 4 (length (first parts))))
                   (return (values (date-words (second nums) (third nums) (first nums))
                                   1 nil)))
                  (t (return (values (loop for p in parts
                                           for first = t then nil
                                           append (append (unless first (list "slash"))
                                                          (number-words p)))
                                     1 nil)))))))
      ;; hyphens between digits: a phone number, an ISO date, or a range
      (let ((parts (split-on #\- core)))
        (when (and (>= (length parts) 2) (every #'all-digits-p parts))
          (let ((lens (mapcar #'length parts))
                (nums (mapcar #'parse-integer parts)))
            (cond ((member lens '((3 4) (3 3 4) (1 3 4) (1 3 3 4)) :test #'equal)
                   ;; a phone number is digits, not a quantity
                   (return (values (loop for p in parts append (digit-words p)) 1 nil)))
                  ((equal lens '(4 2 2))
                   (return (values (date-words (second nums) (third nums) (first nums))
                                   1 nil)))
                  ((= 2 (length parts))
                   (return (values (append (number-words (first parts) :year t)
                                           (list "to")
                                           (number-words (second parts) :year t))
                                   1 nil)))
                  (t (return (values (loop for p in parts append (digit-words p))
                                     1 nil)))))))
      ;; a plain number, which is where the context lives
      (when (plain-number-p core)
        (let* ((prev (core-at toks (1- i)))
               (next (core-at toks (1+ i)))
               (value (parse-integer (remove #\, core) :junk-allowed t))
               (unit (and next (unit-entry next)))
               (mer (meridiem-words next))
               (month (month-number prev)))
          (cond ((and month value (<= 1 value 31) (<= (length core) 2))
                 ;; `June 15' is `June fifteenth': a date, not a count
                 (return (values (ordinal-words value) 1 nil)))
                ((and mer value) (return (values (append (cardinal-words value) mer) 2 :maybe)))
                (unit (return (values (append (number-words core) (unit-words unit value))
                                      2 nil)))
                (t (return (values (number-words core :year t) 1 nil))))))
      ;; whatever is left: 3km, COVID-19, B2B, an order number
      (values (expand-runs core) 1 nil))))

;;; ---- one token -------------------------------------------------------------

(defun expand-abbrev (toks i)
  "-> (values WORDS CONSUMED POLICY FOUND)."
  (let* ((core (tok-core (aref toks i)))
         (dotted (find #\. (tok-trail (aref toks i))))
         (entry (abbreviation-entry core dotted)))
    (if (null entry)
        (values nil nil nil nil)
        (let* ((next (core-at toks (1+ i)))
               (words (second entry))
               (class (third entry)))
          (case words
            ;; `Dr.' and `St.' are two words each.  A capitalised word after them
            ;; means the title reading — Doctor Smith, St. Louis — because a name
            ;; follows a title far more often than a street name ends a sentence.
            (:dr (setf words (if (capitalized-p next) '("doctor") '("drive"))))
            (:st (setf words (if (capitalized-p next) '("saint") '("street"))))
            ;; `No.' is a number only when a number follows it; otherwise it is
            ;; the word `no', and its period is a full stop
            (:no (if (and next (or (has-digit-p next) (eql (char next 0) #\#)))
                     (setf words '("number"))
                     (setf words '("no") class :maybe))))
          (values words 1 class t)))))

(defun expand-word-token (toks i)
  "A token that is already a word -> (values WORDS POLICY)."
  (let* ((tk (aref toks i))
         (core (tok-core tk)))
    (cond
      ;; `10 x 20' is a size, and x is the word `by'
      ((and (string-equal core "x")
            (plain-number-p (or (core-at toks (1- i)) ""))
            (plain-number-p (or (core-at toks (1+ i)) "")))
       (values (list "by") nil))
      ;; a lone capital with a period, before a name, is an initial: A. Lincoln
      ((and (= 1 (length core))
            (upper-case-p (char core 0))
            (find #\. (tok-trail tk))
            (capitalized-p (core-at toks (1+ i))))
       (values (list (letter-name (char core 0))) :never))
      (t (values (expand-alpha-string core) nil)))))

(defun expand-core (toks i)
  "-> (values WORDS CONSUMED POLICY)."
  (let* ((core (tok-core (aref toks i))))
    (if (zerop (length core))
        (values nil 1 nil)
        (multiple-value-bind (words consumed policy found) (expand-abbrev toks i)
          (cond
            (found (values words consumed policy))
            ;; a unit of measure after a number: 3 km, 20 mph, 100 km/h
            ((let ((prev (core-at toks (1- i))))
               (and prev (plain-number-p prev) (unit-entry core)))
             (let ((prev (core-at toks (1- i))))
               (values (unit-words (unit-entry core)
                                   (parse-integer (remove #\, prev) :junk-allowed t))
                       1 nil)))
            ;; U.S., F.B.I., e.e. cummings — single letters joined by periods
            ((initialism-p core) (values (spell-letters core) 1 :maybe))
            ((word-only-p core)
             (multiple-value-bind (words policy) (expand-word-token toks i)
               (values words 1 policy)))
            ((has-digit-p core) (expand-numeric toks i))
            (t (values (expand-runs core) 1 nil)))))))

(defun initialism-p (s)
  (let ((parts (split-on #\. s)))
    (and (>= (length parts) 2)
         (every (lambda (p) (or (string= p "")
                                (and (= 1 (length p)) (alpha-char-p (char p 0)))))
                parts)
         (some (lambda (p) (plusp (length p))) parts))))

(defun token-break (trail policy toks next)
  "Whether the sentence ends here.  TRAIL is the punctuation after the token and
POLICY what its period means; NEXT is the index of the token after it."
  (let ((stop (find-if (lambda (c) (find c ".!?")) (or trail "")))
        (clause (find-if (lambda (c) (find c ";:")) (or trail ""))))
    (cond ((and stop (eql stop #\.) policy)
           (case policy
             (:never nil)
             (:title (>= next (length toks)))
             (t (or (>= next (length toks)) (capitalized-p (core-at toks next))))))
          (stop t)
          (clause t)
          (t nil))))

(defun expand-token (toks i)
  "-> (values WORDS BREAK CONSUMED)."
  (let ((tk (aref toks i)))
    (if (zerop (length (tok-core tk)))
        ;; punctuation on its own — an ellipsis, a stray dash, a bullet
        (values nil (and (find-if (lambda (c) (find c ".!?")) (tok-raw tk)) t) 1)
        (multiple-value-bind (words consumed policy) (expand-core toks i)
          (let* ((consumed (max 1 (or consumed 1)))
                 (last (+ i consumed -1)))
            (values words
                    (token-break (trail-at toks last) policy toks (1+ last))
                    consumed))))))

;;; ---- assembly --------------------------------------------------------------

(defun sayable (word)
  "WORD with anything the lexicon cannot be asked about taken out, or NIL if
nothing with a letter in it is left.  The last guarantee before the clause goes
out: whatever the rules above did, this is only ever letters, apostrophes and
hyphens."
  (let ((w (string-trim "-"
                        (remove-if-not (lambda (c) (or (alpha-char-p c) (find c "'-")))
                                       word))))
    (when (some #'alpha-char-p w) w)))

(defun normalize-text (text)
  "Sentence-sized clauses of sayable words: numbers spelled out, abbreviations
expanded, symbols said, punctuation turned into a break."
  (let ((toks (tokenize (or text "")))
        (clauses '())
        (words '()))
    (flet ((flush ()
             (let ((clean (remove nil (mapcar #'sayable (nreverse words)))))
               (setf words '())
               (when clean
                 (push (format nil "~{~a~^ ~}" clean) clauses)))))
      (let ((i 0))
        (loop while (< i (length toks))
              do (multiple-value-bind (new break consumed) (expand-token toks i)
                   (dolist (w new) (push w words))
                   (when break (flush))
                   (incf i (max 1 consumed)))))
      (flush))
    (nreverse clauses)))
