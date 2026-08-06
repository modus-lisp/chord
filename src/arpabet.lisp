;;;; arpabet.lisp — ARPAbet to IPA, in the dialect the voice was trained on.
;;;;
;;;; CMUdict speaks ARPAbet: 39 phones, ASCII, with a stress digit on every vowel
;;;; (AH0 the schwa in `sofa', AH1 the vowel in `but').  A Piper VITS voice speaks
;;;; IPA — and not IPA in general but the particular transcription its training
;;;; front end produced, which is what the voice's phoneme_id_map enumerates.  This
;;;; file is the bridge, and it is the one place where being merely reasonable is
;;;; not enough: a symbol outside the voice's inventory is an error at synthesis
;;;; time, and a symbol inside it but wrong is a mispronunciation nobody can see.
;;;;
;;;; So the table is checked two ways.  ARPABET-COVERS-INVENTORY-P proves every
;;;; symbol we can emit exists in a given voice, which is mechanical.  Whether the
;;;; CHOICE is right — a for AE rather than æ, ɜː for stressed ER — is a claim
;;;; about US English transcription convention, and the only honest gate on it is
;;;; a person listening.  Each such choice is commented with what it rests on.
;;;;
;;;; Stress: the mark goes immediately before the stressed vowel, not at the head
;;;; of the syllable — `hello' is hɛlˈoʊ.  Primary is ˈ (U+02C8), secondary ˌ
;;;; (U+02CC); ARPAbet's 0 is unstressed and gets no mark.

(in-package #:chord)

(defparameter +arpabet-ipa+
  '(;; ---- vowels ----------------------------------------------------------
    ;; Length marks are part of the symbol: this transcription writes the tense
    ;; vowels long (iː uː ɑː ɔː) and the lax ones bare (ɪ ʊ ɛ).
    ("AA" . "ɑː")   ; odd, father
    ("AE" . "a")    ; at, cat — the US front vowel is written a, not æ
    ("AH" . "ʌ")    ; hut — but see ARPA-VOWEL-IPA: AH0 is the schwa, ə
    ("AO" . "ɔː")   ; ought, law
    ("AW" . "aʊ")   ; cow
    ("AY" . "aɪ")   ; hide
    ("EH" . "ɛ")    ; Ed
    ("ER" . "ɜː")   ; hurt — stressed; unstressed ER0 is ɚ (see below)
    ("EY" . "eɪ")   ; ate
    ("IH" . "ɪ")    ; it
    ("IY" . "iː")   ; eat — unstressed final IY0 is the shorter i (`happy')
    ("OW" . "oʊ")   ; oat
    ("OY" . "ɔɪ")   ; toy
    ("UH" . "ʊ")    ; hood
    ("UW" . "uː")   ; two
    ;; ---- consonants ------------------------------------------------------
    ("B" . "b") ("CH" . "tʃ") ("D" . "d") ("DH" . "ð")
    ("F" . "f") ("G" . "ɡ")                ; U+0261, the IPA g, not ASCII g
    ("HH" . "h") ("JH" . "dʒ") ("K" . "k") ("L" . "l")
    ("M" . "m") ("N" . "n") ("NG" . "ŋ") ("P" . "p")
    ("R" . "ɹ")                            ; the English approximant, not a trill
    ("S" . "s") ("SH" . "ʃ") ("T" . "t") ("TH" . "θ")
    ("V" . "v") ("W" . "w") ("Y" . "j") ("Z" . "z") ("ZH" . "ʒ"))
  "ARPAbet phone (no stress digit) -> IPA, before the stress-dependent cases.")

(defparameter +arpabet-vowels+
  '("AA" "AE" "AH" "AO" "AW" "AY" "EH" "ER" "EY" "IH" "IY" "OW" "OY" "UH" "UW")
  "The phones that carry a stress digit.")

(defun split-stress (phone)
  "PHONE as (values BASE STRESS): \"AH0\" -> \"AH\", 0.  STRESS is NIL for a
consonant, which has no digit at all."
  (let* ((n (length phone))
         (last (char phone (1- n))))
    (if (digit-char-p last)
        (values (subseq phone 0 (1- n)) (digit-char-p last))
        (values phone nil))))

(defun arpa-vowel-ipa (base stress final-p)
  "The IPA for a vowel, where the stress digit changes the vowel itself and not
just the mark before it.  Three cases, all of them ordinary US English:

  AH0  the schwa.  ARPAbet spells `sofa' S OW1 F AH0 and `but' B AH1 T with the
       same phone; IPA does not — ə and ʌ are different vowels.
  ER0  r-coloured schwa (`butter' -> ɚ), against ɜː when stressed (`bird').
  IY0  word-final unstressed is the short i of `happy', not the long iː of `eat'."
  (cond ((and (string= base "AH") (eql stress 0)) "ə")
        ((and (string= base "ER") (eql stress 0)) "ɚ")
        ((and (string= base "IY") (eql stress 0) final-p) "i")
        (t (cdr (assoc base +arpabet-ipa+ :test #'string=)))))

(defun stress-mark (stress)
  (case stress (1 "ˈ") (2 "ˌ") (t "")))

(defun arpabet-to-ipa (phones)
  "PHONES (ARPAbet with stress digits) as one IPA string.

The stress mark is emitted before its vowel, so it lands mid-word exactly where
the transcription the voice learned puts it."
  (with-output-to-string (out)
    (loop for rest on phones
          for phone = (car rest)
          do (multiple-value-bind (base stress) (split-stress phone)
               (let ((vowel (member base +arpabet-vowels+ :test #'string=)))
                 (write-string (if vowel (stress-mark stress) "") out)
                 (let ((ipa (if vowel
                                (arpa-vowel-ipa base stress (null (cdr rest)))
                                (cdr (assoc base +arpabet-ipa+ :test #'string=)))))
                   (when ipa (write-string ipa out))))))))

(defun arpabet-ipa-symbols ()
  "Every IPA character this table can ever emit — what a voice must know."
  (let ((chars '()))
    (dolist (entry +arpabet-ipa+)
      (map nil (lambda (c) (pushnew c chars)) (cdr entry)))
    (dolist (extra '("ə" "ɚ" "i" "ˈ" "ˌ"))
      (map nil (lambda (c) (pushnew c chars)) extra))
    (sort chars #'char<)))

(defun arpabet-covers-inventory-p (voice)
  "T if VOICE knows every symbol ARPABET-TO-IPA can produce; otherwise the list of
symbols it does not.  Mechanical, and worth running against any new voice before
trusting a word of what it says."
  (let ((missing (remove-if (lambda (c) (gethash (string c) (voice-id-map voice)))
                            (arpabet-ipa-symbols))))
    (or (null missing) missing)))
