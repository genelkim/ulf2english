;;; Gene Kim 11-24-2018
;;; Functions for generating English text from ULF formulas.

(in-package :ulf2english)

(setq *debug-ulf2english* nil)

;; Post formats a ULF-to-string mapping.
;; If it is a name (e.g. |John|), the pipes are stripped off.
;; If not a name replace dash and underscores with spaces.
;; Otherwise, the string is made lowercase.
(defun post-format-ulf-string (s)
  (let ((sstr (if (symbolp s) (util:sym2str s) s)))
    (if (ulf:is-strict-name? sstr)
      (coerce (subseq (coerce sstr 'list) 1 (1- (length sstr)))
              'string)
      (cl-strings:replace-all
        (cl-strings:replace-all (string-downcase sstr) "-" " ")
        "_" " "))))

;; TODO: probably put a version of thi in ulf-lib.
;; Returns t if 'token' is an atomic ULF element that has a corresponding token
;; in the surface string and return nil otherwise.  e.g.,
;;   man.n -> t
;;   that -> t
;;   tht -> nil
;;   k -> nil
;;   to -> t
;;   perf -> t
;;   {man}.n -> nil
(defun is-surface-token? (token)
  (or (and (ulf:has-suffix? token) (not (ulf:lex-elided? token)))
      (ulf:is-strict-name? token)
      (member token'(that not and or to))))


;; Maps the ULF suffix to a pure POS symbol for UPPEN MORPH.
;; TODO: complete...
(defun suffix-to-pos (suffix)
  (case suffix
    (adv-a 'adv)
    (adv-e 'adv)
    (adv-s 'adv)
    (adv-f 'adv)
    ; Uppen morph calls auxiliaries verbs.
    (aux-s 'v)
    (aux-v 'v)
    ;(aux-s 'aux)
    ;(aux-v 'aux)
    (otherwise suffix)))


(defun pluralize! (ulf)
;``````````````````````
; Converts the given ULF to the plural version of the surface form.
  (cond
    ((null ulf) nil)
    ((atom ulf)
     (multiple-value-bind (word suffix) (ulf:split-by-suffix ulf)
       (ulf:add-suffix
         (um-conjugate word (suffix-to-pos suffix) (list '3pl))
         suffix)))
    ;; TODO: handle recursive cases...
    (t ulf)))

(defun add-tense! (ulf)
;``````````````````````
; Converts the given ULF to the tensed surface form if the input is of the form
; (tense ulf).  Otherwise, it just returns the ulf.
;
; e.g.
;   (past run.v) -> ran.v
;   (pres sleep.v) -> sleep.v
; TODO: handle prog, perf...
  (cond
    ;; Simple case where there's tense and a simple verb.
    ((and (= 2 (length ulf))
          (ulf:lex-tense? (first ulf))
          (or (verb? (second ulf))
              (aux? (second ulf))))
     (let ((tense (first ulf))
           (verb (second ulf)))
       (multiple-value-bind (word suffix) (ulf:split-by-suffix verb)
         (ulf:add-suffix
           (intern (pattern-en-conjugate (string word) :tense (ulf2pen-tense tense)))
           suffix))))
    ;; Ignore all other cases for now.
    (t ulf)))

(defun dumb-ppart (word)
;```````````````````````
; Takes a word symbol and uses a simple heuristic to make it into the past
; participle form.
;   if it ends in 'e', append 'd'
;   otherwise, append 'ed'
  (if (atom word)
    (let ((letters (util:split-into-atoms word)))
      (if (member (car (last letters)) '(#\e #\E))
        (util:fuse-into-atom (append letters '(#\D)))
        (util:fuse-into-atom (append letters '(#\E #\D)))))
    ;; Just return since it's not an atom.
    word))

(defun pen-ppart (word)
;```````````````````````
; Takes a word symbol and uses a pattern.en to make it past participle form.
   ;; TODO: enable aliases and tags in pattern-en-conjugate so we can do this
   ;; with "ppart".
   (intern (pattern-en-conjugate (string word) :tense 'PAST
                                 :aspect 'PROGRESSIVE)))

(defun pasv-to-surface! (ulf)
;`````````````````````
; Converts the given ULF to the pasv form if the input is of the form
; (pasv ulf).  Otherwise, it returns the input directly.
;
; e.g.
;   (pasv hit.v) -> ((past be.v) hit.v)
;   (pasv confuse.v) -> ((past be.v) confused.v)
;   TODO: get the inherited tense
  (cond
    ((and (= (length ulf))
          (eq 'pasv (first ulf))
          (verb? (second ulf)))
     (multiple-value-bind (word suffix) (ulf:split-by-suffix (second ulf))
       (let ((pasvd (pen-ppart word)))
         (list '(past be.v)
               (ulf:add-suffix pasvd suffix)))))))

(defparameter *plur-to-surface*
  '(/ (plur _!)
      (pluralize! _!)))
(defparameter *tense-to-surface*
  '(/ ((!1 ulf:lex-tense?) _!2)
      (add-tense! (!1 _!2))))
(defparameter *pasv-to-surface*
  '(/ (pasv _!)
      (pasv-to-surface! (pasv _!))))

(defun add-morphology (ulf)
  (ttt:apply-rules (list *pasv-to-surface*
                         *plur-to-surface*
                         *tense-to-surface*)
                   ulf :max-n 500
                   :rule-order :slow-forward))

(defun capitalize-first (string)
  (if (zerop (length string))
      ""
      (let ((copy (copy-seq string)))
        (setf (char copy 0)
              (char-upcase (char copy 0)))
        copy)))


;; Function that takes a relational noun in ULF and transforms it into bare
;; noun form. It's meant for TTT mapping, hence the exclamation mark ending.
(defun unrel-noun! (ulf)
  (multiple-value-bind (word suffix) (ulf:split-by-suffix ulf)
    (let ((wchars (util:split-into-atoms word))
          (tchars (util:split-into-atoms suffix)))
      (util:fuse-into-atom
        (append (reverse (nthcdr 3 (reverse wchars)))
                '(\.)
                tchars)))))

;; Converts relational nouns to versions closer to surface form. Implicit
;; referents lead to a deletion of the preposition, e.g.
;;   (on.p ({the}.d (top-of.n *ref)))
;;   -> (on.p ({the}.d top.n))
(defun relational-nouns-to-surface (ulf)
  (util:unhide-ttt-ops
    (ttt:apply-rules '((/ (lex-rel-noun? (! [*S] [*REF]))
                          (unrel-noun! lex-rel-noun?)))
                     (util:hide-ttt-ops ulf) :max-n 500
                     :rule-order :slow-forward)))

;; List of functions for preprocessing ULFs in context (i.e. before any
;; transformation of the ULF).
(defparameter *contextual-preprocess-fns*
  (list #'relational-nouns-to-surface))
;; Preprocesses the ULF formula according to contextual cues, which are
;; separate from morphological modifications.
(defun contextual-preprocess (ulf)
  (reduce #'(lambda (acc fn) (funcall fn acc))
          *contextual-preprocess-fns*
          :initial-value ulf))

;; Maps a ULF formula to a corresponding surface string.
;; NB: currently this is incomplete and not fluent.
(defun ulf2english (ulf)
  ;; TODO: make more sophisticated version (tenses, quotes, perfect, passive, prog, etc.).

  ;; For now just drop all special operators and just take the suffixed tokens.
  ;; The only non-suffixed tokens that we preserve are "that", "not", "and",
  ;; "or", "to".
  ;; TODO: just have a canonicalization function for introducing implicit
  ;; suffixes and another canonicalization function for identifying words that
  ;; appear in the surface form.
  (let* ((cntxt-preprocd (contextual-preprocess ulf))
         (morph-added (add-morphology cntxt-preprocd))
         (surface-only (remove-if-not #'is-surface-token?
                                      (alexandria:flatten morph-added)))
         (stringified (mapcar #'util:sym2str surface-only))
         ;(dotsplit (mapcar #'(lambda (x) (cl-strings:split x ".")) stringified))
         ;(pruned (mapcar #'(lambda (x) (subseq x 0 (max 1 (1- (length x))))) dotsplit))
         ;(rejoined (mapcar #'(lambda (x) (cl-strings:join x :separator ".")) pruned))
         (rejoined (mapcar #'ulf:strip-suffix stringified))
         (postform (mapcar #'post-format-ulf-string rejoined)))
    (if *debug-ulf2english*
      (progn
        (format t "contextual-preprocess ~s" cntxt-preprocd)
        (format t "morph-added ~s~%" morph-added)
        (format t "Surface-only ~s~%" surface-only)
        (format t "stringified ~s~%" stringified)
        (format t "rejoined ~s~%" rejoined)
        (format t "postform ~s~%" postform)))
    (capitalize-first (cl-strings:join postform :separator " "))))

