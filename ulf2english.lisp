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

(defun pen-past-participle (word)
;```````````````````````
; Takes a word symbol and uses a pattern.en to make it past participle form.
;   confuse -> confused
;   run -> run
;   mention -> mentioned
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

(defun verb-to-present-participle! (verb)
;`````````````````````````````````
; Converts the given verb symbol to present participle form.
;  run.v -> running.v
;  is.v -> being.v
  (if (and (symbolp verb) (verb? verb))
    (multiple-value-bind (word suffix) (ulf:split-by-suffix verb)
      (ulf:add-suffix 
        (intern (pattern-en-conjugate (string word) :tense 'PRESENT
                                      :aspect 'PROGRESSIVE))
        suffix))))


(defun search-main-verb (vp &key (sub nil))
;``````````````````````
; Searches vp (a ULF VP) for the main verb. If sub is not nil, sub substitutes
; for the main verb.
;
; Returns the following values in a list
;   main verb
;   whether is was found
;   new vp
  (cond
    ;; Simple lexical case.
    ((lex-verb? vp) (values vp t (if sub sub vp)))
    ;; Starts with a verb -- recurse into verb.
    ((and (listp vp)
          (verb? (car vp)))
     (multiple-value-bind (mv found new-carvp) (search-main-verb (car vp) :sub sub)
       (values mv found (cons new-carvp (cdr vp)))))
    ;; Starts with adv-a or phrasal sentence operator -- recurse into cdr.
    ((and (listp vp)
          (or (adv-a? (car vp)) (phrasal-sent-op? (car vp))))
     (multiple-value-bind (mv found new-cdrvp) (search-main-verb (cdr vp) :sub sub)
       (values mv found (cons (car vp) new-cdrvp))))
    ;; Passivized verb.
    ((and (listp vp)
          (= (length vp) 2)
          (equal 'pasv (first vp))
          (lex-verb? (second vp)))
     (multiple-value-bind (mv found new-cdrvp) (search-main-verb (cdr vp) :sub sub)
       (values mv found (cons (car vp) new-cdrvp))))
    ;; Otherwise, it's not found.
    (t (values nil nil vp))))

(defun find-main-verb (vp)
;````````````````````
; Finds the main verb in a ULF VP.  
  (search-main-verb vp))

(defun replace-main-verb (vp sub)
;```````````````````````
; Find the main verb and returns a new VP with the substitute value.
  (multiple-value-bind (mv found newvp) (search-main-verb vp :sub sub)
    newvp))

(defun vp-to-present-participle! (vp)
;```````````````````````````````
; Converts a VP so that the main verb is in present-participle form.
;   '(sleep.v (adv-a (in.p (the.d bed.n))))
;   -> '(sleeping.v (adv-a (in.p (the.d bed.n))))
;   '(quickly.adv-a (look.v around.adv-a))
;   -> '(quickly.adv-a (looking.v around.adv-a))
  (cond 
    ((verb? vp) 
     (let* ((main-verb (find-main-verb vp))
            (pres-part (verb-to-present-participle! main-verb)))
       (replace-main-verb vp pres-part)))
    ;; If it isn't a verb phrase, just return.
    (t vp)))

(defun all-vp-to-present-participle! (ulf-frag)
  ;(mapcar #'vp-to-present-participle! ulf-frag))
  (vp-to-present-participle! ulf-frag))

(defparameter *plur-to-surface*
  '(/ (plur _!)
      (pluralize! _!)))
(defparameter *tense-to-surface*
  '(/ ((!1 ulf:lex-tense?) _!2)
      (add-tense! (!1 _!2))))
(defparameter *pasv-to-surface*
  '(/ (pasv _!)
      (pasv-to-surface! (pasv _!))))

(defparameter *pres-part-for-post-modifying-verbs*
;`````````````````````````````````````````````
; Transform noun and np post-modifying verbs to present-pariciple
; form. E.g. (n+preds man.n walk.v) -> 
  '(/ ((!1 n+preds np+preds n+post) _!2 _+3)
      (!1 _!2 (all-vp-to-present-participle! _+3))))

(defun add-morphology (ulf)
  (ttt:apply-rules (list *pasv-to-surface*
                         *plur-to-surface*
                         *tense-to-surface*
                         *pres-part-for-post-modifying-verbs*)
                   ulf :max-n 500
                   :rule-order :slow-forward))

(defun capitalize-first (string)
  (if (zerop (length string))
      ""
      (let ((copy (copy-seq string)))
        (setf (char copy 0)
              (char-upcase (char copy 0)))
        copy)))

(defun add-punct-curried (punct)
  (lambda (sent)
    (cl-strings:join (list sent punct) :separator "")))

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

;;Extracts and returns the sentence punctuation from the ULF (sentence
;;termination symbol only)
;;Parameters:
;;ulf - the ULF as a list
;;Retval:
;;the sentence termination character as a string
;;---Georgiy
(defun extract-punctuation (ulf)
  (if (and (> (length ulf) 1)
	   (or (eq (cadr ulf) '?)
	       (eq (cadr ulf) '!)))
      (string (cadr ulf))
    "."))

;; Input: a list of tokens.
;; Output: tokens with certain determiner-"thing"/"one" combinations merged.
;; 
;; Ex. "any" "one" -> "anyone"
;;     "every" "thing" -> "everything"
(defun merge-det-thing-combos (tokens)
  (cond 
    ((null tokens) nil)
    (t (let* ((rec (merge-det-thing-combos (cdr tokens)))
              (cur (car tokens))
              (toprec (car rec)))
         (if (and (member (string-downcase cur) '("any" "no" "every") 
                          :test #'equal)
                  (member (string-downcase toprec) '("one" "thing") 
                          :test #'equal))
           ;; If the pattern matches, merge the strings.
           (cons (cl-strings:join (list cur toprec)) (cdr rec))
           ;; Otherwise, just build up the list again.
           (cons cur rec))))))

(defparameter *ulf2english-stages*
  '((contextual-preprocess "Contextual preprocess")
    (add-morphology "Adding morphology")
    ((lambda (x) (remove-if-not #'is-surface-token? (alexandria:flatten x)))
     "Only retaining surface symbols")
    ((lambda (x) (mapcar #'util:sym2str x)) "Stringify symbols")
    ((lambda (x) (mapcar #'ulf:strip-suffix x)) "Strip suffixes")
    ((lambda (x) (mapcar #'post-format-ulf-string x)) "Post-format strings")
    (merge-det-thing-combos "Merge special determiner-noun combinations")
    ((lambda (x) (cl-strings:join x :separator " ")) "Glue together")))

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
  (let* ((punct (extract-punctuation ulf))
         (add-punct-fn (add-punct-curried punct))
        staged)
    (setq staged (reduce #'(lambda (acc new)
                             (let* ((fn (first new))
                                    (desc (second new))
                                    (res (funcall fn acc)))
                               (if *debug-ulf2english*
                                 (format t "~a: ~s~%" desc res))
                               res))
                         *ulf2english-stages* :initial-value ulf))
    (funcall (compose add-punct-fn #'capitalize-first) staged)))

