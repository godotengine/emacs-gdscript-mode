;;; gdscript-rx.el --- Regex for GDScript -*- lexical-binding: t; -*-

;; Copyright (C) 2020 GDQuest

;; Author: Nathan Lovato <nathan@gdquest.com>, Fabián E. Gallina <fgallina@gnu.org>
;; URL: https://github.com/godotengine/emacs-gdscript-mode/
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: nathan@gdquest.com
;; Created: Feb 2020
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Macro that defines named regular expressions to detect syntactic elements in
;; GDScript code.
;; Starts with a copy of the rx package that ships with Emacs 27.

;;; Code:

;; gdscript-rx is a copy of Emacs 27's rx module, to ensure compatibility with
;; Emacs 26
;; Copy of Emacs 27's rx package

;; This facility allows writing regexps in a sexp-based language
;; instead of strings.  Regexps in the `rx' notation are easier to
;; read, write and maintain; they can be indented and commented in a
;; natural way, and are easily composed by program code.
;; The translation to string regexp is done by a macro and does not
;; incur any extra processing during run time.  Example:
;;
;;  (rx bos (or (not (any "^"))
;;              (seq "^" (or " *" "["))))
;;
;; => "\\`\\(?:[^^]\\|\\^\\(?: \\*\\|\\[\\)\\)"
;;
;; The notation is much influenced by and retains some compatibility with
;; Olin Shivers's SRE, with concessions to Emacs regexp peculiarities,
;; and the older Emacs package Sregex.

;; The `gdscript-rx--translate...' functions below return (REGEXP . PRECEDENCE),
;; where REGEXP is a list of string expressions that will be
;; concatenated into a regexp, and PRECEDENCE is one of
;;
;;  t    -- can be used as argument to postfix operators (eg. "a")
;;  seq  -- can be concatenated in sequence with other seq or higher (eg. "ab")
;;  lseq -- can be concatenated to the left of rseq or higher (eg. "^a")
;;  rseq -- can be concatenated to the right of lseq or higher (eg. "a$")
;;  nil  -- can only be used in alternatives (eg. "a\\|b")
;;
;; They form a lattice:
;;
;;           t          highest precedence
;;           |
;;          seq
;;         /   \
;;      lseq   rseq
;;         \   /
;;          nil         lowest precedence


(defconst gdscript-rx--char-classes
  '((digit         . digit)
    (numeric       . digit)
    (num           . digit)
    (control       . cntrl)
    (cntrl         . cntrl)
    (hex-digit     . xdigit)
    (hex           . xdigit)
    (xdigit        . xdigit)
    (blank         . blank)
    (graphic       . graph)
    (graph         . graph)
    (printing      . print)
    (print         . print)
    (alphanumeric  . alnum)
    (alnum         . alnum)
    (letter        . alpha)
    (alphabetic    . alpha)
    (alpha         . alpha)
    (ascii         . ascii)
    (nonascii      . nonascii)
    (lower         . lower)
    (lower-case    . lower)
    (punctuation   . punct)
    (punct         . punct)
    (space         . space)
    (whitespace    . space)
    (white         . space)
    (upper         . upper)
    (upper-case    . upper)
    (word          . word)
    (wordchar      . word)
    (unibyte       . unibyte)
    (multibyte     . multibyte))
  "Alist mapping rx symbols to character classes.
Most of the names are from SRE.")

(defvar gdscript-rx-constituents nil
  "Alist of old-style rx extensions, for compatibility.
For new code, use `gdscript-rx-define', `gdscript-rx-let',
or `gdscript-rx-let-eval'.

Each element is (SYMBOL . DEF).

If DEF is a symbol, then SYMBOL is an alias of DEF.

If DEF is a string, then SYMBOL is a plain rx symbol defined as the
   regexp string DEF.

If DEF is a list on the form (FUN MIN-ARGS MAX-ARGS PRED), then
   SYMBOL is an rx form with at least MIN-ARGS and at most
   MAX-ARGS arguments.  If MAX-ARGS is nil, then there is no upper limit.
   FUN is a function taking the entire rx form as single argument
   and returning the translated regexp string.
   If PRED is non-nil, it is a predicate that all actual arguments must
   satisfy.")

(defvar gdscript-rx--local-definitions nil
  "Alist of dynamic local rx definitions.
Each entry is:
 (NAME DEF)      -- NAME is an rx symbol defined as the rx form DEF.
 (NAME ARGS DEF) -- NAME is an rx form with arglist ARGS, defined
                    as the rx form DEF (which can contain members of ARGS).")

(defsubst gdscript-rx--lookup-def (name)
  "Current definition of NAME: (DEF) or (ARGS DEF), or nil if none."
  (or (cdr (assq name gdscript-rx--local-definitions))
      (get name 'gdscript-rx-definition)))

(defun gdscript-rx--expand-def (form)
  "FORM expanded (once) if a user-defined construct; otherwise nil."
  (cond ((symbolp form)
         (let ((def (gdscript-rx--lookup-def form)))
           (and def
                (if (cdr def)
                    (error "Not an `rx' symbol definition: %s" form)
                  (car def)))))
        ((consp form)
         (let* ((op (car form))
                (def (gdscript-rx--lookup-def op)))
           (and def
                (if (cdr def)
                    (gdscript-rx--expand-template
                     op (cdr form) (nth 0 def) (nth 1 def))
                  (error "Not an `rx' form definition: %s" op)))))))

;; TODO: Additions to consider:
;; - A construct like `or' but without the match order guarantee,
;;   maybe `unordered-or'.  Useful for composition or generation of
;;   alternatives; permits more effective use of regexp-opt.

(defun gdscript-rx--translate-symbol (sym)
  "Translate the rx symbol SYM. Return (REGEXP . PRECEDENCE)."
  (pcase sym
    ;; Use `list' instead of a quoted list to wrap the strings here,
    ;; since the return value may be mutated.
    ((or 'nonl 'not-newline 'any) (cons (list ".") t))
    ((or 'anychar 'anything)      (cons (list "[^z-a]") t))
    ('unmatchable                 (gdscript-rx--empty))
    ((or 'bol 'line-start)        (cons (list "^") 'lseq))
    ((or 'eol 'line-end)          (cons (list "$") 'rseq))
    ((or 'bos 'string-start 'bot 'buffer-start) (cons (list "\\`") t))
    ((or 'eos 'string-end   'eot 'buffer-end)   (cons (list "\\'") t))
    ('point                       (cons (list "\\=") t))
    ((or 'bow 'word-start)        (cons (list "\\<") t))
    ((or 'eow 'word-end)          (cons (list "\\>") t))
    ('word-boundary               (cons (list "\\b") t))
    ('not-word-boundary           (cons (list "\\B") t))
    ('symbol-start                (cons (list "\\_<") t))
    ('symbol-end                  (cons (list "\\_>") t))
    ('not-wordchar                (cons (list "\\W") t))
    (_
     (cond
      ((let ((class (cdr (assq sym gdscript-rx--char-classes))))
         (and class (cons (list (concat "[[:" (symbol-name class) ":]]")) t))))

      ((let ((expanded (gdscript-rx--expand-def sym)))
         (and expanded (gdscript-rx--translate expanded))))

      ;; For compatibility with old rx.
      ((let ((entry (assq sym gdscript-rx-constituents)))
         (and (progn
                (while (and entry (not (stringp (cdr entry))))
                  (setq entry
                        (if (symbolp (cdr entry))
                            ;; Alias for another entry.
                            (assq (cdr entry) gdscript-rx-constituents)
                          ;; Wrong type, try further down the list.
                          (assq (car entry)
                                (cdr (memq entry gdscript-rx-constituents))))))
                entry)
              (cons (list (cdr entry)) nil))))
      (t (error "Unknown rx symbol `%s'" sym))))))

(defun gdscript-rx--enclose (left-str rexp right-str)
  "Bracket REXP by LEFT-STR and RIGHT-STR."
  (append (list left-str) rexp (list right-str)))

(defun gdscript-rx--bracket (rexp)
  (gdscript-rx--enclose "\\(?:" rexp "\\)"))

(defun gdscript-rx--sequence (left right)
  "Return the concatenation of LEFT and RIGHT, translated items.
Each item is of the form (REGEXP . PRECEDENCE), returning (REGEXP . PRECEDENCE)."
  ;; Concatenation rules:
  ;;  seq  ++ seq  -> seq
  ;;  lseq ++ seq  -> lseq
  ;;  seq  ++ rseq -> rseq
  ;;  lseq ++ rseq -> nil
  (cond ((not (car left)) right)
        ((not (car right)) left)
        (t
         (let ((l (if (memq (cdr left) '(nil rseq))
                      (cons (gdscript-rx--bracket (car left)) t)
                    left))
               (r (if (memq (cdr right) '(nil lseq))
                      (cons (gdscript-rx--bracket (car right)) t)
                    right)))
           (cons (append (car l) (car r))
                 (if (eq (cdr l) 'lseq)
                     (if (eq (cdr r) 'rseq)
                         nil                   ; lseq ++ rseq
                       'lseq)                  ; lseq ++ seq
                   (if (eq (cdr r) 'rseq)
                       'rseq                   ; seq ++ rseq
                     'seq)))))))               ; seq ++ seq

(defun gdscript-rx--translate-seq (body)
  "Translate the sequence BODY of zero or more rx items.
Return (REGEXP . PRECEDENCE)."
  (if body
      (let* ((items (mapcar #'gdscript-rx--translate body))
             (result (car items)))
        (dolist (item (cdr items))
          (setq result (gdscript-rx--sequence result item)))
        result)
    (cons nil 'seq)))

(defun gdscript-rx--empty ()
  "Regexp that never match anything."
  (cons (list "\\`a\\`") 'seq))

;; `cl-every' replacement to avoid bootstrapping problems.
(defun gdscript-rx--every (pred list)
  "Whether PRED is true for every element of LIST."
  (while (and list (funcall pred (car list)))
    (setq list (cdr list)))
  (null list))

(defun gdscript-rx--foldl (f x l)
  "(F (F (F X L0) L1) L2) ...
Left-fold the list L, starting with X, by the binary function F."
  (while l
    (setq x (funcall f x (car l)))
    (setq l (cdr l)))
  x)

(defun gdscript-rx--translate-or (body)
  "Translate the or-pattern BODY of zero or more rx items.
Return (REGEXP . PRECEDENCE)."
  ;; FIXME: Possible improvements:
  ;;
  ;; - Turn single characters to strings: (or ?a ?b) -> (or "a" "b"),
  ;;   so that they can be candidates for regexp-opt.
  ;;
  ;; - Translate compile-time strings (`eval' forms), again for regexp-opt.
  ;;
  ;; - Flatten sub-patterns first: (or (or A B) (or C D)) -> (or A B C D)
  ;;   in order to improve effectiveness of regexp-opt.
  ;;   This would also help composability.
  ;;
  ;; - Use associativity to run regexp-opt on contiguous subsets of arguments
  ;;   if not all of them are strings.  Example:
  ;;   (or (+ digit) "CHARLIE" "CHAN" (+ blank))
  ;;   -> (or (+ digit) (or "CHARLIE" "CHAN") (+ blank))
  ;;
  ;; - Optimise single-character alternatives better:
  ;;     * classes: space, alpha, ...
  ;;     * (syntax S), for some S (whitespace, word)
  ;;   so that (or "@" "%" digit (any "A-Z" space) (syntax word))
  ;;        -> (any "@" "%" digit "A-Z" space word)
  ;;        -> "[A-Z@%[:digit:][:space:][:word:]]"
  ;;
  ;; Problem: If a subpattern is carefully written to be
  ;; optimizable by regexp-opt, how do we prevent the transforms
  ;; above from destroying that property?
  ;; Example: (or "a" (or "abc" "abd" "abe"))
  (cond
   ((null body)                    ; No items: a never-matching regexp.
    (gdscript-rx--empty))
   ((null (cdr body))              ; Single item.
    (gdscript-rx--translate (car body)))
   ((gdscript-rx--every #'stringp body)     ; All strings.
    (cons (list (regexp-opt body nil))
          t))
   ((gdscript-rx--every #'gdscript-rx--charset-p body)  ; All charsets.
    (gdscript-rx--translate-union nil body))
   (t
    (cons (append (car (gdscript-rx--translate (car body)))
                  (mapcan (lambda (item)
                            (cons "\\|" (car (gdscript-rx--translate item))))
                          (cdr body)))
          nil))))

(defun gdscript-rx--charset-p (form)
  "Return t if FORM is like a charset.
A charset only consists of character intervals and set operations."
  (or (and (consp form)
           (or (and (memq (car form) '(any 'in 'char))
                    (gdscript-rx--every (lambda (x) (not (symbolp x))) (cdr form)))
               (and (memq (car form) '(not or | intersection))
                    (gdscript-rx--every #'gdscript-rx--charset-p (cdr form)))))
      (characterp form)
      (and (stringp form) (= (length form) 1))
      (and (or (symbolp form) (consp form))
           (let ((expanded (gdscript-rx--expand-def form)))
             (and expanded
                  (gdscript-rx--charset-p expanded))))))

(defun gdscript-rx--string-to-intervals (str)
  "Decode STR as intervals.
A-Z becomes (?A . ?Z), and the single character X becomes (?X .
?X). Return the intervals in a list."
  ;; We could just do string-to-multibyte on the string and work with
  ;; that instead of this `decode-char' workaround.
  (let ((decode-char
         (if (multibyte-string-p str)
             #'identity
           #'unibyte-char-to-multibyte))
        (len (length str))
        (i 0)
        (intervals nil))
    (while (< i len)
      (cond ((and (< i (- len 2))
                  (= (aref str (1+ i)) ?-))
             ;; Range.
             (let ((start (funcall decode-char (aref str i)))
                   (end   (funcall decode-char (aref str (+ i 2)))))
               (cond ((and (<= start #x7f) (>= end #x3fff80))
                      ;; Ranges between ASCII and raw bytes are split to
                      ;; avoid having them absorb Unicode characters
                      ;; caught in-between.
                      (push (cons start #x7f) intervals)
                      (push (cons #x3fff80 end) intervals))
                     ((<= start end)
                      (push (cons start end) intervals))
                     (t
                      (error "Invalid rx `any' range: %s"
                             (substring str i 3))))
               (setq i (+ i 3))))
            (t
             ;; Single character.
             (let ((char (funcall decode-char (aref str i))))
               (push (cons char char) intervals))
             (setq i (+ i 1)))))
    intervals))

(defun gdscript-rx--condense-intervals (intervals)
  "Merge adjacent and overlapping intervals by mutation, preserving the order.
INTERVALS is a list of (START . END) with START ≤ END, sorted by START."
  (let ((tail intervals)
        d)
    (while (setq d (cdr tail))
      (if (>= (cdar tail) (1- (caar d)))
          (progn
            (setcdr (car tail) (max (cdar tail) (cdar d)))
            (setcdr tail (cdr d)))
        (setq tail d)))
    intervals))

(defun gdscript-rx--parse-any (body)
  "Parse arguments of an (any ...) construct.
Return (INTERVALS . CLASSES), where INTERVALS is a sorted list of
disjoint intervals (each a cons of chars), and CLASSES
a list of named character classes in the order they occur in BODY."
  (let ((classes nil)
        (strings nil)
        (conses nil))
    ;; Collect strings, conses and characters, and classes in separate bins.
    (dolist (arg body)
      (cond ((stringp arg)
             (push arg strings))
            ((and (consp arg)
                  (characterp (car arg))
                  (characterp (cdr arg))
                  (<= (car arg) (cdr arg)))
             ;; Copy the cons, in case we need to modify it.
             (push (cons (car arg) (cdr arg)) conses))
            ((characterp arg)
             (push (cons arg arg) conses))
            ((and (symbolp arg)
                  (let ((class (cdr (assq arg gdscript-rx--char-classes))))
                    (and class
                         (or (memq class classes)
                             (progn (push class classes) t))))))
            (t (error "Invalid rx `any' argument: %s" arg))))
    (cons (gdscript-rx--condense-intervals
           (sort (append conses
                         (mapcan #'gdscript-rx--string-to-intervals strings))
                 #'car-less-than-car))
          (reverse classes))))

(defun gdscript-rx--generate-alt (negated intervals classes)
  "Generate a character alternative.  Return (REGEXP . PRECEDENCE).
If NEGATED is non-nil, negate the result; INTERVALS is a sorted
list of disjoint intervals and CLASSES a list of named character
classes."
  (let ((items (append intervals classes)))
    ;; Move lone ] and range ]-x to the start.
    (let ((rbrac-l (assq ?\] items)))
      (when rbrac-l
        (setq items (cons rbrac-l (delq rbrac-l items)))))

    ;; Split x-] and move the lone ] to the start.
    (let ((rbrac-r (rassq ?\] items)))
      (when (and rbrac-r (not (eq (car rbrac-r) ?\])))
        (setcdr rbrac-r ?\\)
        (setq items (cons '(?\] . ?\]) items))))

    ;; Split ,-- (which would end up as ,- otherwise).
    (let ((dash-r (rassq ?- items)))
      (when (eq (car dash-r) ?,)
        (setcdr dash-r ?,)
        (setq items (nconc items '((?- . ?-))))))

    ;; Remove - (lone or at start of interval)
    (let ((dash-l (assq ?- items)))
      (when dash-l
        (if (eq (cdr dash-l) ?-)
            (setq items (delq dash-l items))   ; Remove lone -
          (setcar dash-l ?.))                  ; Reduce --x to .-x
        (setq items (nconc items '((?- . ?-))))))

    ;; Deal with leading ^ and range ^-x.
    (when (and (consp (car items))
               (eq (caar items) ?^)
               (cdr items))
      ;; Move ^ and ^-x to second place.
      (setq items (cons (cadr items)
                        (cons (car items) (cddr items)))))

    (cond
     ;; Empty set: if negated, any char, otherwise match-nothing.
     ((null items)
      (if negated
          (gdscript-rx--translate-symbol 'anything)
        (gdscript-rx--empty)))
     ;; Single non-negated character.
     ((and (null (cdr items))
           (consp (car items))
           (eq (caar items) (cdar items))
           (not negated))
      (cons (list (regexp-quote (char-to-string (caar items))))
            t))
     ;; At least one character or class, possibly negated.
     (t
      (cons
       (list
        (concat
         "["
         (and negated "^")
         (mapconcat (lambda (item)
                      (cond ((symbolp item)
                             (format "[:%s:]" item))
                            ((eq (car item) (cdr item))
                             (char-to-string (car item)))
                            ((eq (1+ (car item)) (cdr item))
                             (string (car item) (cdr item)))
                            (t
                             (string (car item) ?- (cdr item)))))
                    items nil)
         "]"))
       t)))))

(defun gdscript-rx--translate-any (negated body)
  "Translate the (any ...) construct BODY.  Return (REGEXP . PRECEDENCE).
If NEGATED, negate the sense."
  (let ((parsed (gdscript-rx--parse-any body)))
    (gdscript-rx--generate-alt negated (car parsed) (cdr parsed))))

(defun gdscript-rx--intervals-to-alt (negated intervals)
  "Generate a character alternative from an interval set.
Return (REGEXP . PRECEDENCE).
INTERVALS is a sorted list of disjoint intervals.
If NEGATED, negate the sense."
  ;; Detect whether the interval set is better described in
  ;; complemented form.  This is not just a matter of aesthetics: any
  ;; range from ASCII to raw bytes will automatically exclude the
  ;; entire non-ASCII Unicode range by the regexp engine.
  (if (gdscript-rx--every (lambda (iv) (not (<= (car iv) #x3ffeff (cdr iv))))
                          intervals)
      (gdscript-rx--generate-alt negated intervals nil)
    (gdscript-rx--generate-alt
     (not negated) (gdscript-rx--complement-intervals intervals) nil)))

;; FIXME: Consider turning `not' into a variadic operator, following SRE:
;; (not A B) = (not (or A B)) = (intersection (not A) (not B)), and
;; (not) = anychar.
;; Maybe allow singleton characters as arguments.

(defun gdscript-rx--translate-not (negated body)
  "Translate the (not ...) construct BODY.  Return (REGEXP . PRECEDENCE).
If NEGATED, negate the sense (thus making it positive)."
  (unless (and body (null (cdr body)))
    (error "The rx `not' form takes exactly one argument"))
  (let ((arg (car body)))
    (cond
     ((and (consp arg)
           (pcase (car arg)
             ((or 'any 'in 'char)
              (gdscript-rx--translate-any      (not negated) (cdr arg)))
             ('syntax
              (gdscript-rx--translate-syntax   (not negated) (cdr arg)))
             ('category
              (gdscript-rx--translate-category (not negated) (cdr arg)))
             ('not
              (gdscript-rx--translate-not      (not negated) (cdr arg)))
             ((or 'or '|)
              (gdscript-rx--translate-union    (not negated) (cdr arg)))
             ('intersection
              (gdscript-rx--translate-intersection (not negated) (cdr arg))))))
     ((let ((class (cdr (assq arg gdscript-rx--char-classes))))
        (and class
             (gdscript-rx--generate-alt (not negated) nil (list class)))))
     ((eq arg 'word-boundary)
      (gdscript-rx--translate-symbol
       (if negated 'word-boundary 'not-word-boundary)))
     ((characterp arg)
      (gdscript-rx--generate-alt (not negated) (list (cons arg arg)) nil))
     ((and (stringp arg) (= (length arg) 1))
      (let ((char (string-to-char arg)))
        (gdscript-rx--generate-alt (not negated) (list (cons char char)) nil)))
     ((let ((expanded (gdscript-rx--expand-def arg)))
        (and expanded
             (gdscript-rx--translate-not negated (list expanded)))))
     (t (error "Illegal argument to rx `not': %S" arg)))))

(defun gdscript-rx--complement-intervals (intervals)
  "Complement of the interval list INTERVALS."
  (let ((compl nil)
        (c 0))
    (dolist (iv intervals)
      (when (< c (car iv))
        (push (cons c (1- (car iv))) compl))
      (setq c (1+ (cdr iv))))
    (when (< c (max-char))
      (push (cons c (max-char)) compl))
    (nreverse compl)))

(defun gdscript-rx--intersect-intervals (ivs-a ivs-b)
  "Intersection of the interval lists IVS-A and IVS-B."
  (let ((isect nil))
    (while (and ivs-a ivs-b)
      (let ((a (car ivs-a))
            (b (car ivs-b)))
        (cond
         ((< (cdr a) (car b)) (setq ivs-a (cdr ivs-a)))
         ((> (car a) (cdr b)) (setq ivs-b (cdr ivs-b)))
         (t
          (push (cons (max (car a) (car b))
                      (min (cdr a) (cdr b)))
                isect)
          (setq ivs-a (cdr ivs-a))
          (setq ivs-b (cdr ivs-b))
          (cond ((< (cdr a) (cdr b))
                 (push (cons (1+ (cdr a)) (cdr b))
                       ivs-b))
                ((> (cdr a) (cdr b))
                 (push (cons (1+ (cdr b)) (cdr a))
                       ivs-a)))))))
    (nreverse isect)))

(defun gdscript-rx--union-intervals (ivs-a ivs-b)
  "Union of the interval lists IVS-A and IVS-B."
  (gdscript-rx--complement-intervals
   (gdscript-rx--intersect-intervals
    (gdscript-rx--complement-intervals ivs-a)
    (gdscript-rx--complement-intervals ivs-b))))

(defun gdscript-rx--charset-intervals (charset)
  "Return a sorted list of non-adjacent disjoint intervals from CHARSET.
CHARSET is any expression allowed in a character set expression:
characters, single-char strings, `any' forms (no classes permitted),
or `not', `or' or `intersection' forms whose arguments are charsets."
  (pcase charset
    (`(,(or 'any 'in 'char) . ,body)
     (let ((parsed (gdscript-rx--parse-any body)))
       (when (cdr parsed)
         (error
          "Character class not permitted in set operations: %S"
          (cadr parsed)))
       (car parsed)))
    (`(not ,x) (gdscript-rx--complement-intervals (gdscript-rx--charset-intervals x)))
    (`(,(or 'or '|) . ,body) (gdscript-rx--charset-union body))
    (`(intersection . ,body) (gdscript-rx--charset-intersection body))
    ((pred characterp)
     (list (cons charset charset)))
    ((guard (and (stringp charset) (= (length charset) 1)))
     (let ((char (string-to-char charset)))
       (list (cons char char))))
    (_ (let ((expanded (gdscript-rx--expand-def charset)))
         (if expanded
             (gdscript-rx--charset-intervals expanded)
           (error "Bad character set: %S" charset))))))

(defun gdscript-rx--charset-union (charsets)
  "Union of CHARSETS, as a set of intervals."
  (gdscript-rx--foldl #'gdscript-rx--union-intervals nil
                      (mapcar #'gdscript-rx--charset-intervals charsets)))

(defconst gdscript-rx--charset-all (list (cons 0 (max-char))))

(defun gdscript-rx--charset-intersection (charsets)
  "Intersection of CHARSETS, as a set of intervals."
  (gdscript-rx--foldl #'gdscript-rx--intersect-intervals gdscript-rx--charset-all
                      (mapcar #'gdscript-rx--charset-intervals charsets)))

(defun gdscript-rx--translate-union (negated body)
  "Translate the (or ...) construct of charsets BODY.
Return (REGEXP . PRECEDENCE). If NEGATED, negate the sense."
  (gdscript-rx--intervals-to-alt negated (gdscript-rx--charset-union body)))

(defun gdscript-rx--translate-intersection (negated body)
  "Translate the (intersection ...) construct BODY.
Return (REGEXP . PRECEDENCE). If NEGATED, negate the sense."
  (gdscript-rx--intervals-to-alt negated (gdscript-rx--charset-intersection body)))

(defun gdscript-rx--atomic-regexp (item)
  "ITEM is (REGEXP . PRECEDENCE); return a regexp of precedence t."
  (if (eq (cdr item) t)
      (car item)
    (gdscript-rx--bracket (car item))))

(defun gdscript-rx--translate-counted-repetition (min-count max-count body)
  (let ((operand (gdscript-rx--translate-seq body)))
    (if (car operand)
        (cons (append
               (gdscript-rx--atomic-regexp operand)
               (list (concat "\\{"
                             (number-to-string min-count)
                             (cond ((null max-count) ",")
                                   ((< min-count max-count)
                                    (concat "," (number-to-string max-count))))
                             "\\}")))
              t)
      operand)))

(defun gdscript-rx--check-repeat-arg (name min-args body)
  (unless (>= (length body) min-args)
    (error "The rx `%s' requires at least %d argument%s"
           name min-args (if (= min-args 1) "" "s")))
  ;; There seems to be no reason to disallow zero counts.
  (unless (natnump (car body))
    (error "The rx `%s' first argument must be nonnegative" name)))

(defun gdscript-rx--translate-bounded-repetition (name body)
  (let ((min-count (car body))
        (max-count (cadr body))
        (items (cddr body)))
    (unless (and (natnump min-count)
                 (natnump max-count)
                 (<= min-count max-count))
      (error "Range error `%s'" name))
    (gdscript-rx--translate-counted-repetition min-count max-count items)))

(defun gdscript-rx--translate-repeat (body)
  (gdscript-rx--check-repeat-arg 'repeat 2 body)
  (if (= (length body) 2)
      (gdscript-rx--translate-counted-repetition (car body) (car body) (cdr body))
    (gdscript-rx--translate-bounded-repetition 'repeat body)))

(defun gdscript-rx--translate-** (body)
  (gdscript-rx--check-repeat-arg '** 2 body)
  (gdscript-rx--translate-bounded-repetition '** body))

(defun gdscript-rx--translate->= (body)
  (gdscript-rx--check-repeat-arg '>= 1 body)
  (gdscript-rx--translate-counted-repetition (car body) nil (cdr body)))

(defun gdscript-rx--translate-= (body)
  (gdscript-rx--check-repeat-arg '= 1 body)
  (gdscript-rx--translate-counted-repetition (car body) (car body) (cdr body)))

(defvar gdscript-rx--greedy t)

(defun gdscript-rx--translate-rep (op-string greedy body)
  "Translate the repetition BODY; OP-STRING is one of \"*\", \"+\" or \"?\".
GREEDY is a boolean.  Return (REGEXP . PRECEDENCE)."
  (let ((operand (gdscript-rx--translate-seq body)))
    (if (car operand)
        (cons (append (gdscript-rx--atomic-regexp operand)
                      (list (concat op-string (unless greedy "?"))))
              ;; The result has precedence seq to avoid (? (* "a")) -> "a*?"
              'seq)
      operand)))

(defun gdscript-rx--control-greedy (greedy body)
  "Translate the sequence BODY with greediness GREEDY.
Return (REGEXP . PRECEDENCE)."
  (let ((gdscript-rx--greedy greedy))
    (gdscript-rx--translate-seq body)))

(defun gdscript-rx--translate-group (body)
  "Translate the `group' form BODY.  Return (REGEXP . PRECEDENCE)."
  (cons (gdscript-rx--enclose "\\("
                              (car (gdscript-rx--translate-seq body))
                              "\\)")
        t))

(defun gdscript-rx--translate-group-n (body)
  "Translate the `group-n' form BODY.  Return (REGEXP . PRECEDENCE)."
  (unless (and (integerp (car body)) (> (car body) 0))
    (error "The rx `group-n' requires a positive number as first argument"))
  (cons (gdscript-rx--enclose (concat "\\(?" (number-to-string (car body)) ":")
                              (car (gdscript-rx--translate-seq (cdr body)))
                              "\\)")
        t))

(defun gdscript-rx--translate-backref (body)
  "Translate the `backref' form BODY.  Return (REGEXP . PRECEDENCE)."
  (unless (and (= (length body) 1) (integerp (car body)) (<= 1 (car body) 9))
    (error "The rx `backref' requires an argument in the range 1..9"))
  (cons (list "\\" (number-to-string (car body))) t))

(defconst gdscript-rx--syntax-codes
  '((whitespace         . ?-)           ; SPC also accepted
    (punctuation        . ?.)
    (word               . ?w)           ; W also accepted
    (symbol             . ?_)
    (open-parenthesis   . ?\()
    (close-parenthesis  . ?\))
    (expression-prefix  . ?\')
    (string-quote       . ?\")
    (paired-delimiter   . ?$)
    (escape             . ?\\)
    (character-quote    . ?/)
    (comment-start      . ?<)
    (comment-end        . ?>)
    (string-delimiter   . ?|)
    (comment-delimiter  . ?!)))

(defun gdscript-rx--translate-syntax (negated body)
  "Translate the `syntax' form BODY.  Return (REGEXP . PRECEDENCE).
If NEGATED, inverse the effect."
  (unless (and body (null (cdr body)))
    (error "The rx `syntax' form takes exactly one argument"))
  (let* ((sym (car body))
         (syntax (cdr (assq sym gdscript-rx--syntax-codes))))
    (unless syntax
      (cond
       ;; Syntax character directly (sregex compatibility)
       ((and (characterp sym) (rassq sym gdscript-rx--syntax-codes))
        (setq syntax sym))
       ;; Syntax character as symbol (sregex compatibility)
       ((symbolp sym)
        (let ((name (symbol-name sym)))
          (when (= (length name) 1)
            (let ((char (string-to-char name)))
              (when (rassq char gdscript-rx--syntax-codes)
                (setq syntax char)))))))
      (unless syntax
        (error "Unknown rx syntax name `%s'" sym)))
    (cons (list (string ?\\ (if negated ?S ?s) syntax))
          t)))

(defconst gdscript-rx--categories
  '((space-for-indent           . ?\s)
    (base                       . ?.)
    (consonant                  . ?0)
    (base-vowel                 . ?1)
    (upper-diacritical-mark     . ?2)
    (lower-diacritical-mark     . ?3)
    (tone-mark                  . ?4)
    (symbol                     . ?5)
    (digit                      . ?6)
    (vowel-modifying-diacritical-mark . ?7)
    (vowel-sign                 . ?8)
    (semivowel-lower            . ?9)
    (not-at-end-of-line         . ?<)
    (not-at-beginning-of-line   . ?>)
    (alpha-numeric-two-byte     . ?A)
    (chinese-two-byte           . ?C)
    (chinse-two-byte            . ?C)   ; A typo in Emacs 21.1-24.3.
    (greek-two-byte             . ?G)
    (japanese-hiragana-two-byte . ?H)
    (indian-two-byte            . ?I)
    (japanese-katakana-two-byte . ?K)
    (strong-left-to-right       . ?L)
    (korean-hangul-two-byte     . ?N)
    (strong-right-to-left       . ?R)
    (cyrillic-two-byte          . ?Y)
    (combining-diacritic        . ?^)
    (ascii                      . ?a)
    (arabic                     . ?b)
    (chinese                    . ?c)
    (ethiopic                   . ?e)
    (greek                      . ?g)
    (korean                     . ?h)
    (indian                     . ?i)
    (japanese                   . ?j)
    (japanese-katakana          . ?k)
    (latin                      . ?l)
    (lao                        . ?o)
    (tibetan                    . ?q)
    (japanese-roman             . ?r)
    (thai                       . ?t)
    (vietnamese                 . ?v)
    (hebrew                     . ?w)
    (cyrillic                   . ?y)
    (can-break                  . ?|)))

(defun gdscript-rx--translate-category (negated body)
  "Translate the `category' form BODY.  Return (REGEXP . PRECEDENCE).
If NEGATED, inverse the effect."
  (unless (and body (null (cdr body)))
    (error "The rx `category' form takes exactly one argument"))
  (let* ((arg (car body))
         (category
          (cond ((symbolp arg)
                 (let ((cat (assq arg gdscript-rx--categories)))
                   (unless cat
                     (error "Unknown rx category `%s'" arg))
                   (cdr cat)))
                ((characterp arg) arg)
                (t (error "Invalid rx `category' argument `%s'" arg)))))
    (cons (list (string ?\\ (if negated ?C ?c) category))
          t)))

(defvar gdscript-rx--delayed-evaluation nil
  "Whether to allow certain forms to be evaluated at runtime.")

(defun gdscript-rx--translate-literal (body)
  "Translate the `literal' form BODY.  Return (REGEXP . PRECEDENCE)."
  (unless (and body (null (cdr body)))
    (error "The rx `literal' form takes exactly one argument"))
  (let ((arg (car body)))
    (cond ((stringp arg)
           (cons (list (regexp-quote arg)) (if (= (length arg) 1) t 'seq)))
          (gdscript-rx--delayed-evaluation
           (cons (list (list 'regexp-quote arg)) 'seq))
          (t (error "The rx `literal' form with non-string argument")))))

(defun gdscript-rx--translate-eval (body)
  "Translate the `eval' form BODY.  Return (REGEXP . PRECEDENCE)."
  (unless (and body (null (cdr body)))
    (error "The rx `eval' form takes exactly one argument"))
  (gdscript-rx--translate (eval (car body))))

(defvar gdscript-rx--regexp-atomic-regexp nil)

(defun gdscript-rx--translate-regexp (body)
  "Translate the `regexp' form BODY.  Return (REGEXP . PRECEDENCE)."
  (unless (and body (null (cdr body)))
    (error "The rx `regexp' form takes exactly one argument"))
  (let ((arg (car body)))
    (cond ((stringp arg)
           ;; Generate the regexp when needed, since rx isn't
           ;; necessarily present in the byte-compilation environment.
           (unless gdscript-rx--regexp-atomic-regexp
             (setq gdscript-rx--regexp-atomic-regexp
                   ;; Match atomic (precedence t) regexps: may give
                   ;; false negatives but no false positives, assuming
                   ;; the target string is syntactically correct.
                   (gdscript-rx-to-string
                    '(seq
                      bos
                      (or (seq "["
                               (opt "^")
                               (opt "]")
                               (* (or (seq "[:" (+ (any "a-z")) ":]")
                                      (not (any "]"))))
                               "]")
                          anything
                          (seq "\\"
                               (or anything
                                   (seq (any "sScC_") anything)
                                   (seq "("
                                        (* (or (not (any "\\"))
                                               (seq "\\" (not (any ")")))))
                                        "\\)"))))
                      eos)
                    t)))
           (cons (list arg)
                 (if (string-match-p gdscript-rx--regexp-atomic-regexp arg) t nil)))
          (gdscript-rx--delayed-evaluation
           (cons (list arg) nil))
          (t (error "The rx `regexp' form with non-string argument")))))

(defun gdscript-rx--translate-compat-form (def form)
  "Translate a compatibility form from `gdscript-rx-constituents'.
DEF is the definition tuple.  Return (REGEXP . PRECEDENCE)."
  (let* ((fn (nth 0 def))
         (min-args (nth 1 def))
         (max-args (nth 2 def))
         (predicate (nth 3 def))
         (nargs (1- (length form))))
    (when (< nargs min-args)
      (error "The `%s' form takes at least %d argument(s)"
             (car form) min-args))
    (when (and max-args (> nargs max-args))
      (error "The `%s' form takes at most %d argument(s)"
             (car form) max-args))
    (when (and predicate (not (gdscript-rx--every predicate (cdr form))))
      (error "The `%s' form requires arguments satisfying `%s'"
             (car form) predicate))
    (let ((regexp (funcall fn form)))
      (unless (stringp regexp)
        (error "The `%s' form did not expand to a string" (car form)))
      (cons (list regexp) nil))))

(defun gdscript-rx--substitute (bindings form)
  "Substitute BINDINGS in FORM.  BINDINGS is an alist of (NAME . VALUES)
where VALUES is a list to splice into FORM wherever NAME occurs.
Return the substitution result wrapped in a list, since a single value
can expand to any number of values."
  (cond ((symbolp form)
         (let ((binding (assq form bindings)))
           (if binding
               (cdr binding)
             (list form))))
        ((consp form)
         (if (listp (cdr form))
             ;; Proper list.  We substitute variables even in the head
             ;; position -- who knows, might be handy one day.
             (list (mapcan (lambda (x) (copy-sequence
                                        (gdscript-rx--substitute bindings x)))
                           form))
           ;; Cons pair (presumably an interval).
           (let ((first (gdscript-rx--substitute bindings (car form)))
                 (second (gdscript-rx--substitute bindings (cdr form))))
             (if (and first (not (cdr first))
                      second (not (cdr second)))
                 (list (cons (car first) (car second)))
               (error
                "Cannot substitute a &rest parameter into a dotted pair")))))
        (t (list form))))

;; FIXME: Consider adding extensions in Lisp macro style, where
;; arguments are passed unevaluated to code that returns the rx form
;; to use.  Example:
;;
;;   (gdscript-rx-let ((radix-digit (radix)
;;             :lisp (list 'any (cons ?0 (+ ?0 (eval radix) -1)))))
;;     (rx (radix-digit (+ 5 3))))
;; =>
;;   "[0-7]"
;;
;; While this would permit more powerful extensions, it's unclear just
;; how often they would be used in practice.  Let's wait until there is
;; demand for it.

;; FIXME: An alternative binding syntax would be
;;
;;   (NAME RXs...)
;; and
;;   ((NAME ARGS...) RXs...)
;;
;; which would have two minor advantages: multiple RXs with implicit
;; `seq' in the definition, and the arglist is no longer an optional
;; element in the middle of the list.  On the other hand, it's less
;; like traditional lisp arglist constructs (defun, defmacro).
;; Since it's a Scheme-like syntax, &rest parameters could be done using
;; dotted lists:
;;  (gdscript-rx-let (((name arg1 arg2 . rest) ...definition...)) ...)

(defun gdscript-rx--expand-template (op values arglist template)
  "Return TEMPLATE with variables in ARGLIST replaced with VALUES."
  (let ((bindings nil)
        (value-tail values)
        (formals arglist))
    (while formals
      (pcase (car formals)
        ('&rest
         (unless (cdr formals)
           (error
            "Expanding rx def `%s': missing &rest parameter name" op))
         (push (cons (cadr formals) value-tail) bindings)
         (setq formals nil)
         (setq value-tail nil))
        (name
         (unless value-tail
           (error
            "Expanding rx def `%s': too few arguments (got %d, need %s%d)"
            op (length values)
            (if (memq '&rest arglist) "at least " "")
            (- (length arglist) (length (memq '&rest arglist)))))
         (push (cons name (list (car value-tail))) bindings)
         (setq value-tail (cdr value-tail))))
      (setq formals (cdr formals)))
    (when value-tail
      (error
       "Expanding rx def `%s': too many arguments (got %d, need %d)"
       op (length values) (length arglist)))
    (let ((subst (gdscript-rx--substitute bindings template)))
      (if (and subst (not (cdr subst)))
          (car subst)
        (error "Expanding rx def `%s': must result in a single value" op)))))

(defun gdscript-rx--translate-form (form)
  "Translate an rx FORM (list structure).  Return (REGEXP . PRECEDENCE)."
  (let ((body (cdr form)))
    (pcase (car form)
      ((or 'seq : 'and 'sequence) (gdscript-rx--translate-seq body))
      ((or 'or '|)              (gdscript-rx--translate-or body))
      ((or 'any 'in 'char)      (gdscript-rx--translate-any nil body))
      ('not-char                (gdscript-rx--translate-any t body))
      ('not                     (gdscript-rx--translate-not nil body))
      ('intersection            (gdscript-rx--translate-intersection nil body))

      ('repeat                  (gdscript-rx--translate-repeat body))
      ('=                       (gdscript-rx--translate-= body))
      ('>=                      (gdscript-rx--translate->= body))
      ('**                      (gdscript-rx--translate-** body))

      ((or 'zero-or-more '0+)           (gdscript-rx--translate-rep "*" gdscript-rx--greedy body))
      ((or 'one-or-more '1+)            (gdscript-rx--translate-rep "+" gdscript-rx--greedy body))
      ((or 'zero-or-one 'opt 'optional) (gdscript-rx--translate-rep "?" gdscript-rx--greedy body))

      ('*                       (gdscript-rx--translate-rep "*" t body))
      ('+                       (gdscript-rx--translate-rep "+" t body))
      ((or '\? ?\s)             (gdscript-rx--translate-rep "?" t body))

      ('*?                      (gdscript-rx--translate-rep "*" nil body))
      ('+?                      (gdscript-rx--translate-rep "+" nil body))
      ((or '\?? ??)             (gdscript-rx--translate-rep "?" nil body))

      ('minimal-match           (gdscript-rx--control-greedy nil body))
      ('maximal-match           (gdscript-rx--control-greedy t   body))

      ((or 'group 'submatch)     (gdscript-rx--translate-group body))
      ((or 'group-n 'submatch-n) (gdscript-rx--translate-group-n body))
      ('backref                  (gdscript-rx--translate-backref body))

      ('syntax                  (gdscript-rx--translate-syntax nil body))
      ('not-syntax              (gdscript-rx--translate-syntax t body))
      ('category                (gdscript-rx--translate-category nil body))

      ('literal                 (gdscript-rx--translate-literal body))
      ('eval                    (gdscript-rx--translate-eval body))
      ((or 'regexp 'regex)      (gdscript-rx--translate-regexp body))

      (op
       (cond
        ((not (symbolp op)) (error "Bad rx operator `%S'" op))

        ((let ((expanded (gdscript-rx--expand-def form)))
           (and expanded
                (gdscript-rx--translate expanded))))

        ;; For compatibility with old rx.
        ((let ((entry (assq op gdscript-rx-constituents)))
           (and (progn
                  (while (and entry (not (consp (cdr entry))))
                    (setq entry
                          (if (symbolp (cdr entry))
                              ;; Alias for another entry.
                              (assq (cdr entry) gdscript-rx-constituents)
                            ;; Wrong type, try further down the list.
                            (assq (car entry)
                                  (cdr (memq entry gdscript-rx-constituents))))))
                  entry)
                (gdscript-rx--translate-compat-form (cdr entry) form))))

        (t (error "Unknown rx form `%s'" op)))))))

(defconst gdscript-rx--builtin-forms
  '(seq sequence : and or | any in char not-char not intersection
        repeat = >= **
        zero-or-more 0+ *
        one-or-more 1+ +
        zero-or-one opt optional \?
        *? +? \??
        minimal-match maximal-match
        group submatch group-n submatch-n backref
        syntax not-syntax category
        literal eval regexp regex)
  "List of built-in rx function-like symbols.")

(defconst gdscript-rx--builtin-symbols
  (append '(nonl not-newline any anychar anything unmatchable
                 bol eol line-start line-end
                 bos eos string-start string-end
                 bow eow word-start word-end
                 symbol-start symbol-end
                 point word-boundary not-word-boundary not-wordchar)
          (mapcar #'car gdscript-rx--char-classes))
  "List of built-in rx variable-like symbols.")

(defconst gdscript-rx--builtin-names
  (append gdscript-rx--builtin-forms gdscript-rx--builtin-symbols)
  "List of built-in rx names.  These cannot be redefined by the user.")

(defun gdscript-rx--translate (item)
  "Translate the gdscript-rx-expression ITEM.  Return (REGEXP . PRECEDENCE)."
  (cond
   ((stringp item)
    (if (= (length item) 0)
        (cons nil 'seq)
      (cons (list (regexp-quote item)) (if (= (length item) 1) t 'seq))))
   ((characterp item)
    (cons (list (regexp-quote (char-to-string item))) t))
   ((symbolp item)
    (gdscript-rx--translate-symbol item))
   ((consp item)
    (gdscript-rx--translate-form item))
   (t (error "Bad rx expression: %S" item))))


;;;###autoload
(defun gdscript-rx-to-string (form &optional no-group)
  "Translate FORM from `rx' sexp syntax into a string regexp.
The arguments to `literal' and `regexp' forms inside FORM must be
constant strings.
If NO-GROUP is non-nil, don't bracket the result in a non-capturing
group.

For extending the `rx' notation in FORM, use `gdscript-rx-define' or `gdscript-rx-let-eval'."
  (let* ((item (gdscript-rx--translate form))
         (exprs (if no-group
                    (car item)
                  (gdscript-rx--atomic-regexp item))))
    (apply #'concat exprs)))

(defun gdscript-rx--to-expr (form)
  "Translate the gdscript-rx-expression FORM to a Lisp expression yielding a regexp."
  (let* ((gdscript-rx--delayed-evaluation t)
         (elems (car (gdscript-rx--translate form)))
         (args nil))
    ;; Merge adjacent strings.
    (while elems
      (let ((strings nil))
        (while (and elems (stringp (car elems)))
          (push (car elems) strings)
          (setq elems (cdr elems)))
        (let ((s (apply #'concat (nreverse strings))))
          (unless (zerop (length s))
            (push s args))))
      (when elems
        (push (car elems) args)
        (setq elems (cdr elems))))
    (cond ((null args) "")                             ; 0 args
          ((cdr args) (cons 'concat (nreverse args)))  ; ≥2 args
          (t (car args)))))                            ; 1 arg


;;;###autoload
(defmacro gdscript-rx-build-rx (&rest regexps)
  "Translate regex REGEXPS in sexp form to a regexp string.
Each argument is one of the forms below; RX is a subform, and
RX... stands for zero or more RXs. For details, see Info
node `(elisp) Rx Notation'. See `gdscript-rx-to-string' for the
corresponding function.

STRING         Match a literal string.
CHAR           Match a literal character.

\(seq RX...)    Match the RXs in sequence.  Alias: :, sequence, and.
\(or RX...)     Match one of the RXs.  Alias: |.

\(zero-or-more RX...) Match RXs zero or more times.  Alias: 0+.
\(one-or-more RX...)  Match RXs one or more times.  Alias: 1+.
\(zero-or-one RX...)  Match RXs or the empty string.
Alias: opt, optional.
\(* RX...)       Match RXs zero or more times; greedy.
\(+ RX...)       Match RXs one or more times; greedy.
\(? RX...)       Match RXs or the empty string; greedy.
\(*? RX...)      Match RXs zero or more times; non-greedy.
\(+? RX...)      Match RXs one or more times; non-greedy.
\(?? RX...)      Match RXs or the empty string; non-greedy.
\(= N RX...)     Match RXs exactly N times.
\(>= N RX...)    Match RXs N or more times.
\(** N M RX...)  Match RXs N to M times.  Alias: repeat.
\(minimal-match RX)  Match RX, with zero-or-more, one-or-more, zero-or-one
                and aliases using non-greedy matching.
\(maximal-match RX)  Match RX, with zero-or-more, one-or-more, zero-or-one
                and aliases using greedy matching, which is the default.

\(any SET...)    Match a character from one of the SETs.  Each SET is a
                character, a string, a range as string \"A-Z\" or cons
                (?A . ?Z), or a character class (see below).  Alias: in, char.
\(not CHARSPEC)  Match one character not matched by CHARSPEC.  CHARSPEC
                can be a character, single-char string, (any ...), (or ...),
                (intersection ...), (syntax ...), (category ...),
                or a character class.
\(intersection CHARSET...) Match all CHARSETs.
                CHARSET is (any...), (not...), (or...) or (intersection...),
                a character or a single-char string.
not-newline     Match any character except a newline.  Alias: nonl.
anychar         Match any character.  Alias: anything.
unmatchable     Never match anything at all.

CHARCLASS       Match a character from a character class.  One of:
 alpha, alphabetic, letter   Alphabetic characters (defined by Unicode).
 alnum, alphanumeric         Alphabetic or decimal digit chars (Unicode).
 digit numeric, num          0-9.
 xdigit, hex-digit, hex      0-9, A-F, a-f.
 cntrl, control              ASCII codes 0-31.
 blank                       Horizontal whitespace (Unicode).
 space, whitespace, white    Chars with whitespace syntax.
 lower, lower-case           Lower-case chars, from current case table.
 upper, upper-case           Upper-case chars, from current case table.
 graph, graphic              Graphic characters (Unicode).
 print, printing             Whitespace or graphic (Unicode).
 punct, punctuation          Not control, space, letter or digit (ASCII);
                              not word syntax (non-ASCII).
 word, wordchar              Characters with word syntax.
 ascii                       ASCII characters (codes 0-127).
 nonascii                    Non-ASCII characters (but not raw bytes).

\(syntax SYNTAX)  Match a character with syntax SYNTAX, being one of:
  whitespace, punctuation, word, symbol, open-parenthesis,
  close-parenthesis, expression-prefix, string-quote,
  paired-delimiter, escape, character-quote, comment-start,
  comment-end, string-delimiter, comment-delimiter

\(category CAT)   Match a character in category CAT, being one of:
  space-for-indent, base, consonant, base-vowel,
  upper-diacritical-mark, lower-diacritical-mark, tone-mark, symbol,
  digit, vowel-modifying-diacritical-mark, vowel-sign,
  semivowel-lower, not-at-end-of-line, not-at-beginning-of-line,
  alpha-numeric-two-byte, chinese-two-byte, greek-two-byte,
  japanese-hiragana-two-byte, indian-two-byte,
  japanese-katakana-two-byte, strong-left-to-right,
  korean-hangul-two-byte, strong-right-to-left, cyrillic-two-byte,
  combining-diacritic, ascii, arabic, chinese, ethiopic, greek,
  korean, indian, japanese, japanese-katakana, latin, lao,
  tibetan, japanese-roman, thai, vietnamese, hebrew, cyrillic,
  can-break

Zero-width assertions: these all match the empty string in specific places.
 line-start         At the beginning of a line.  Alias: bol.
 line-end           At the end of a line.  Alias: eol.
 string-start       At the start of the string or buffer.
                     Alias: buffer-start, bos, bot.
 string-end         At the end of the string or buffer.
                     Alias: buffer-end, eos, eot.
 point              At point.
 word-start         At the beginning of a word.  Alias: bow.
 word-end           At the end of a word.  Alias: eow.
 word-boundary      At the beginning or end of a word.
 not-word-boundary  Not at the beginning or end of a word.
 symbol-start       At the beginning of a symbol.
 symbol-end         At the end of a symbol.

\(group RX...)  Match RXs and define a capture group.  Alias: submatch.
\(group-n N RX...) Match RXs and define capture group N.  Alias: submatch-n.
\(backref N)    Match the text that capture group N matched.

\(literal EXPR) Match the literal string from evaluating EXPR at run time.
\(regexp EXPR)  Match the string regexp from evaluating EXPR at run time.
\(eval EXPR)    Match the rx sexp from evaluating EXPR at compile time.

Additional constructs can be defined using `gdscript-rx-define' and
`gdscript-rx-let',which see.

\(fn REGEXPS...)"
  ;; Retrieve local definitions from the macroexpansion environment.
  ;; (It's unclear whether the previous value of `gdscript-rx--local-definitions'
  ;; should be included, and if so, in which order.)
  (let ((gdscript-rx--local-definitions
         (cdr (assq :gdscript-rx-locals macroexpand-all-environment))))
    (gdscript-rx--to-expr (cons 'seq regexps))))

(defun gdscript-rx--make-binding (name tail)
  "Make a definitions entry named NAME out of TAIL.
TAIL is on the form ([ARGLIST] DEFINITION)."
  (unless (symbolp name)
    (error "Bad `rx' definition name: %S" name))
  ;; FIXME: Consider using a hash table or symbol property, for speed.
  (when (memq name gdscript-rx--builtin-names)
    (error "Cannot redefine built-in rx name `%s'" name))
  (pcase tail
    (`(,def)
     (list def))
    (`(,args ,def)
     (unless (and (listp args) (gdscript-rx--every #'symbolp args))
       (error "Bad argument list for `rx' definition %s: %S" name args))
     (list args def))
    (_ (error "Bad `rx' definition of %s: %S" name tail))))

(defun gdscript-rx--make-named-binding (bindspec)
  "Make a definitions entry out of BINDSPEC.
BINDSPEC is on the form (NAME [ARGLIST] DEFINITION)."
  (unless (consp bindspec)
    (error "Bad `gdscript-rx-let' binding: %S" bindspec))
  (cons (car bindspec)
        (gdscript-rx--make-binding (car bindspec) (cdr bindspec))))

(defun gdscript-rx--extend-local-defs (bindspecs)
  (append (mapcar #'gdscript-rx--make-named-binding bindspecs)
          gdscript-rx--local-definitions))

;;;###autoload
(defmacro gdscript-rx-let-eval (bindings &rest body)
  "Evaluate BODY with local BINDINGS for `gdscript-rx-to-string'.
BINDINGS, after evaluation, is a list of definitions each on the form
\(NAME [(ARGS...)] RX), in effect for calls to `gdscript-rx-to-string'
in BODY.

For bindings without an ARGS list, NAME is defined as an alias
for the `rx' expression RX.  Where ARGS is supplied, NAME is
defined as an `rx' form with ARGS as argument list.  The
parameters are bound from the values in the (NAME ...) form and
are substituted in RX.  ARGS can contain `&rest' parameters,
whose values are spliced into RX where the parameter name occurs.

Any previous definitions with the same names are shadowed during
the expansion of BODY only.
For extensions when using the `rx' macro, use `gdscript-rx-let'.
To make global rx extensions, use `gdscript-rx-define'.
For more details, see Info node `(elisp) Extending Rx'.

\(fn BINDINGS BODY...)"
  (declare (indent 1) (debug (form body)))
  ;; FIXME: this way, `gdscript-rx--extend-local-defs' may need to be autoloaded.
  `(let ((gdscript-rx--local-definitions (gdscript-rx--extend-local-defs ,bindings)))
     ,@body))

;;;###autoload
(defmacro gdscript-rx-let (bindings &rest body)
  "Evaluate BODY with local BINDINGS for `rx'.
BINDINGS is an unevaluated list of bindings each on the form
\(NAME [(ARGS...)] RX).
They are bound lexically and are available in `rx' expressions in
BODY only.

For bindings without an ARGS list, NAME is defined as an alias
for the `rx' expression RX.  Where ARGS is supplied, NAME is
defined as an `rx' form with ARGS as argument list.  The
parameters are bound from the values in the (NAME ...) form and
are substituted in RX.  ARGS can contain `&rest' parameters,
whose values are spliced into RX where the parameter name occurs.

Any previous definitions with the same names are shadowed during
the expansion of BODY only.
For local extensions to `gdscript-rx-to-string', use `gdscript-rx-let-eval'.
To make global rx extensions, use `gdscript-rx-define'.
For more details, see Info node `(elisp) Extending Rx'.

\(fn BINDINGS BODY...)"
  (declare (indent 1) (debug (sexp body)))
  (let ((prev-locals (cdr (assq :gdscript-rx-locals macroexpand-all-environment)))
        (new-locals (mapcar #'gdscript-rx--make-named-binding bindings)))
    (macroexpand-all (cons 'progn body)
                     (cons (cons :gdscript-rx-locals (append new-locals prev-locals))
                           macroexpand-all-environment))))

;;;###autoload
(defmacro gdscript-rx-define (name &rest definition)
  "Define NAME as a global `rx' definition.
If the DEFINITION args list is omitted, define NAME as an alias for the `rx'
expression RX.

If the args list is supplied, define NAME as an `rx' form with
args as argument list.  The parameters are bound from the values
in the (NAME ...) form and are substituted in RX.
args can contain `&rest' parameters, whose values are spliced
into RX where the parameter name occurs.

Any previous global definition of NAME is overwritten with the new one.
To make local rx extensions, use `gdscript-rx-let' for `rx',
`gdscript-rx-let-eval' for `gdscript-rx-to-string'.
For more details, see Info node `(elisp) Extending Rx'.

\(fn NAME [(args...)] RX)"
  (declare (indent 1))
  `(eval-and-compile
     (put ',name 'gdscript-rx-definition ',(gdscript-rx--make-binding name definition))
     ',name))

;; During `gdscript-rx--pcase-transform', list of defined variables in right-to-left
;; order.
(defvar gdscript-rx--pcase-vars)

;; FIXME: The rewriting strategy for pcase works so-so with extensions;
;; definitions cannot expand to `let' or named `backref'.  If this ever
;; becomes a problem, we can handle those forms in the ordinary parser,
;; using a dynamic variable for activating the augmented forms.

(defun gdscript-rx--pcase-transform (rx)
  "Transform RX into a plain gdscript-rx-expression.
RX is an gdscript-rx-expression augmented with `let' and named `backref',
 collecting names into `gdscript-rx--pcase-vars'."
  (pcase rx
    (`(let ,name . ,body)
     (let* ((index (length (memq name gdscript-rx--pcase-vars)))
            (i (if (zerop index)
                   (length (push name gdscript-rx--pcase-vars))
                 index)))
       `(group-n ,i ,(gdscript-rx--pcase-transform (cons 'seq body)))))
    ((and `(backref ,ref)
          (guard (symbolp ref)))
     (let ((index (length (memq ref gdscript-rx--pcase-vars))))
       (when (zerop index)
         (error "The rx `backref' variable must be one of: %s"
                (mapconcat #'symbol-name gdscript-rx--pcase-vars " ")))
       `(backref ,index)))
    ((and `(,head . ,rest)
          (guard (and (symbolp head)
                      (not (memq head '(literal regexp regex eval))))))
     (cons head (mapcar #'gdscript-rx--pcase-transform rest)))
    (_ rx)))

(pcase-defmacro gdscript-rx (&rest regexps)
  "A pattern that matches strings against `rx' REGEXPS in sexp form.
REGEXPS are interpreted as in `rx'.  The pattern matches any
string that is a match for REGEXPS, as if by `string-match'.

In addition to the usual `rx' syntax, REGEXPS can contain the
following constructs:

  (let REF RX...)  binds the symbol REF to a submatch that matches
                   the regular expressions RX.  REF is bound in
                   CODE to the string of the submatch or nil, but
                   can also be used in `backref'.
  (backref REF)    matches whatever the submatch REF matched.
                   REF can be a number, as usual, or a name
                   introduced by a previous (let REF ...)
                   construct."
  (let* ((gdscript-rx--pcase-vars nil)
         (regexp (gdscript-rx--to-expr (gdscript-rx--pcase-transform (cons 'seq regexps)))))
    `(and (pred (string-match ,regexp))
          ,@(let ((i 0))
              (mapcar (lambda (name)
                        (setq i (1+ i))
                        `(app (match-string ,i) ,name))
                      (reverse gdscript-rx--pcase-vars))))))

;; gdscript-rx's unique code starts here
(defmacro gdscript-rx (&rest regexps)
  "Gdscript mode specialized rx macro.
This variant of `rx' supports common Gdscript named REGEXPS."
  `(gdscript-rx-let (
                     (block-start
                      (or (seq (or "if" "elif" "while" "func") (+? (not ":")) ":")
                          (seq (zero-or-more nonl)
                               ":"
                               (or (seq (zero-or-more " ") eol)
                                   (seq (zero-or-more " ") "#" (zero-or-more nonl) eol)))))
                     (dedenter          (seq symbol-start
                                             (or "elif" "else")
                                             symbol-end))
                     (block-ender       (seq symbol-start
                                             (or "break" "continue" "pass" "return")
                                             symbol-end))
                     (defun             (seq symbol-start
                                             (or "func" "class" "static func")
                                             symbol-end))
                     (symbol-name       (seq (any letter ?_) (* (any word ?_))))
                     (open-paren        (or "{" "[" "("))
                     (close-paren       (or "}" "]" ")"))
                     (simple-operator   (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))
                     (not-simple-operator (not simple-operator))
                     ;; TODO: clean up operators that don't exist in GDScript
                     (operator          (or "==" ">=" "is" "not"
                                            "**" "//" "<<" ">>" "<=" "!="
                                            "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                            "=" "%"))
                     (assignment-operator (or "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                              ">>=" "<<=" "&=" "^=" "|="
                                              "="))
                     (string-delimiter  (seq
                                         ;; Match even number of backslashes.
                                         (or (not (any ?\\ ?\' ?\")) point
                                             ;; Quotes might be preceded by an
                                             ;; escaped quote.
                                             (and (or (not (any ?\\)) point) ?\\
                                                  (* ?\\ ?\\) (any ?\' ?\")))
                                         (* ?\\ ?\\)
                                         ;; Match single or triple quotes of any kind.
                                         (group (or  "\"\"\"" "\"" "'''" "'")))))
     (gdscript-rx-build-rx ,@regexps)))

(provide 'gdscript-rx)

;;; gdscript-rx.el ends here
