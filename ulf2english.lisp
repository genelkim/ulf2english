;;; Gene Kim 11-24-2018
;;; Functions for generating English text from ULF formulas.

(in-package :ulf2english)

(defparameter *debug-ulf2english* nil)

;; Give cl-ppcre a nickname.
(add-nickname "CL-PPCRE" "RE")

;; Post formats a ULF-to-string mapping.
;; If it is a name (e.g. |John|), the pipes are stripped off.
;; If not a name replace dash and underscores with spaces.
;; Otherwise, the string is made lowercase.
(defun post-format-ulf-string (s)
  (let ((sstr (if (symbolp s) (util:sym2str s) s)))
    (util:trim
      (if (ulf:is-strict-name? sstr)
        (coerce (subseq (coerce sstr 'list) 1 (1- (length sstr)))
                'string)
        (cl-strings:replace-all
          (cl-strings:replace-all (string-downcase sstr) "-" " ")
          "_" " ")))))


;; TODO: probably put a version of this in ulf-lib.
;; Returns t if 'token' is an atomic ULF element that has a corresponding token
;; in the surface string and return nil otherwise.  e.g.,
;;   man.n -> t
;;   that -> t
;;   tht -> nil
;;   k -> nil
;;   to -> t
;;   perf -> t
;;   {man}.n -> nil
;;   2 -> t
;;   "a" -> t
(defun is-surface-token? (token+)
  (let ((token (util:safe-intern token+ :ulf2english)))
    (or (not (symbolp token))
        (and (ulf:has-suffix? token)
             (not (ulf:lex-elided? token))
             (not (ulf:lex-hole-variable? token)))
        (ulf:is-strict-name? token)
        (member token '(that not and or to most some all every whether if)))))


(defun set-of-to-and (ulf)
  (util:unhide-ttt-ops
    (ttt:apply-rule
      '(/ (set-of _+ _!) (_+ and.cc _!))
      (util:hide-ttt-ops ulf))))


(defun pluralize! (ulf)
;``````````````````````
; Converts the given ULF noun phrase to the plural version of the surface form.
; For complex noun phrases, this amounts to pluralizing the head of the noun phrase.
    ;; TODO: handle pluralization of relational nouns [child-of.n -> children-of.n, not
    ;;                                                 child-of.n -> child-ofs.n]
  (cond
    ((null ulf) nil)
    ((and (atom ulf) (ulf:lex-elided? ulf)) ulf)
    ;; Atomic case.
    ((atom ulf)
     (let ((pkg (symbol-package ulf)))
       (multiple-value-bind (word suffix) (ulf:split-by-suffix ulf)
         (ulf:add-suffix
           (cond
             ;; Preserve case for strict names.
             ((ulf:is-strict-name? word)
              (intern (pattern-en-pluralize (string word) :preserve-case t)
                      pkg))
             ;; Otherwise ignore case.
             (t (intern (pattern-en-pluralize (string word) :preserve-case nil)
                        pkg)))
           suffix :pkg pkg))))
    ;; NP case.
    (t
      (let* ((hn (find-np-head ulf :callpkg :ulf2english))
             (plurhn (pluralize! hn)))
        (replace-np-head ulf plurhn)))))



(defun ulf-quote? (ulf)
  (and (listp ulf) (= (length ulf) 3)
       (eql '|"| (first ulf)) (eql '|"| (third ulf))))


(defun quote2surface! (ulf)
  (cond
    ((ulf-quote? ulf)
     (ulf2english (second ulf) :add-punct? nil :capitalize-front? nil))
    (t ulf)))


(defun quotes2surface! (ulf)
  (ttt:apply-rule '(/ (! ulf-quote?) (quote2surface! !)) ulf
                  :max-n 1000))

(defun post-poss? (ulf)
  (and (listp ulf) (= (length ulf) 2)
       (eql (first ulf) 'quote) (eql (second ulf) 's)))

(defun post-poss2surface! (ulf)
  (declare (ignore ulf))
  '|'s|)

(defun post-posses2surface! (ulf)
  (ttt:apply-rule '(/ (! post-poss?) (post-poss2surface! !)) ulf
                  :max-n 1000))

(defvar *superlative-special-case-alist*
  '((left.a . leftmost.a)
    (right.a . rightmost.a)
    (upper.a . uppermost.a)
    (lower.a . lowermost.a)))

(defun lex-superlative! (ulf)
;````````````````````````
; Converts the given adjective to superlative form.
;   'bad.a -> 'worst.a
;   'left.a -> 'leftmost.a
  (cond
    ((assoc ulf *superlative-special-case-alist*)
     (cdr (assoc ulf *superlative-special-case-alist*)))
    ((lex-adjective? ulf)
     (multiple-value-bind (word suffix) (ulf:split-by-suffix ulf)
       (let ((pkg (symbol-package ulf)))
       (ulf:add-suffix
         (intern (pattern-en-superlative (string-downcase (string word)))
                 pkg)
         suffix :pkg pkg))))
    ;; Not something that can be turned superlative so just return.
    (t ulf)))


(defun ap-superlative! (apulf)
;`````````````````````
; Converts the given adjective phrase and converts it to superlative form.
; This amounts to finding the head adjective and making it superlative most of
; the time.  If the head isn't found, we just wrap 'most' around the whole thing.
  (multiple-value-bind (ha found _1) (ulf:search-ap-head apulf)
    (declare (ignore _1))
    (cond
      (found
        (multiple-value-bind (_2 _3 newap) (ulf:search-ap-head apulf :sub (lex-superlative! ha))
          (declare (ignore _2))
          (declare (ignore _3))
          newap))
      (t (list 'most apulf)))))


;(defparameter *special-ulf-verb-tensing*
;  '(((past be-to.aux-v) . was.aux-v)
;    ((past be-to.aux-s) . was.aux-s)

(defun add-tense! (ulf)
;``````````````````````
; Converts the given ULF to the tensed surface form if the input is of the form
; (tense ulf).  Otherwise, it just returns the ulf.
;
; e.g.
;   (past run.v) -> ran.v
;   (pres sleep.v) -> sleep.v
  (cond
    ;; Simple case where there's tense and a simple verb.
    ((and (= 2 (length ulf))
          (ulf:lex-tense? (first ulf))
          (or (verb? (second ulf))
              (aux? (second ulf))))
     (let* ((tense (first ulf))
            (verb (second ulf))
            (pkg (symbol-package verb)))
       (multiple-value-bind (word suffix) (ulf:split-by-suffix verb)
         (ulf:add-suffix
           (cond
             ((eql word 'were) 'were) ; TODO: this is repeated from conjugate-vp-head!, factorize into a seaprate function.  add a separate function conjugate-tensed-lex-verb...
             ((eql word 'would) 'would)
             ((eql word 'could) 'could)
             ((eql word 'should) 'should)
             ((and (eql word 'will) (member tense '(past cf))
                   (member suffix '(aux aux-s aux-v)))
              'would)
             ((and (eql word 'forsee) (eql tense 'past)) 'forsaw)
             ((not (is-surface-token? verb)) word) ; {be}.v -> {be}.v
             (t (safe-intern (pattern-en-conjugate (string word) :tense (ulf2pen-tense tense))
                             pkg)))
           suffix :pkg pkg))))
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


(defun pasv2surface! (ulf)
;`````````````````````
; Converts the given ULF to the pasv form if the input is of the form
; (pasv ulf).  Otherwise, it returns the input directly.
;
; e.g.
;   (pasv hit.v) -> (be.v hit.v)
;   (pasv confuse.v) -> (be.v confused.v)
;   (pres (pasv confuse.v)) -> ((pres be.v) confused.v)
;   TODO: get the inherited tense
  (cond
    ((or (not (listp ulf)) (not (= (length ulf) 2))) ulf)
    ((and (eq 'pasv (first ulf)) (verb? (second ulf)))
     (list 'be.v (verb-to-past-participle! (second ulf))))
    ((lex-tense? (first ulf))
     (let ((tenseless (pasv2surface! (second ulf))))
       (list (list (first ulf) (first tenseless))
             (second tenseless))))
    (t ulf)))


(defun verb-to-participle (verb &key (part-type 'PRESENT) (force nil))
;`````````````````````````````````
; Converts the given verb symbol to participle form of the given type.
; PRESENT
;  run.v -> running.v
;  be.v -> being.v
; PAST
;  run.v -> run.v
;  confuse.v confused.v
;  mention.v -> mentioned.v
;
; The parameter 'force' forces the participle generation even if this word is
; not in infinitive form. This parameter ensures that words that are already in
; participle form aren't accidentally re-processed.
   ;; TODO: enable aliases and tags in pattern-en-conjugate so we can do this
   ;; with "ppart".
  (assert (member part-type '(PRESENT PAST)))
  (cond
    ((and (symbolp verb) (verb? verb)
          (or force (infinitive? verb)))
     (multiple-value-bind (word suffix) (ulf:split-by-suffix verb)
       (let ((pkg (symbol-package verb)))
         (ulf:add-suffix
           (cond
             ((and (eql word 'forsee) (eql part-type 'past)) 'forseen)
             (t
               (intern (pattern-en-conjugate (string word) :tense part-type
                                             :aspect 'PROGRESSIVE)
                       pkg)))
           suffix :pkg pkg))))
    (t verb)))


(defun verb-to-past-participle! (verb)
  (verb-to-participle verb :part-type 'PAST))

(defun verb-to-present-participle! (verb)
;`````````````````````````````````
; Converts the given verb symbol to present participle form.
;  run.v -> running.v
;  is.v -> being.v
  (verb-to-participle verb :part-type 'PRESENT))


(defun vp-to-participle! (vp &key (part-type nil))
;```````````````````````````````
; Converts a VP so that the main verb is in participle form.
; If no specification is made to the participle form, it assumes that it's
; present participle except in presence of a 'pasv' operator.
; PRESENT PARTICIPLE
;   '(sleep.v (adv-a (in.p (the.d bed.n))))
;   -> '(sleeping.v (adv-a (in.p (the.d bed.n))))
;   '(quickly.adv-a (look.v around.adv-a))
;   -> '(quickly.adv-a (looking.v around.adv-a))
; PAST PARTICIPLE
;   '((pasv sleep.v) (adv-a (in.p (the.d bed.n))))
;   -> '((pasv slept.v) (adv-a (in.p (the.d bed.n))))
;   '(carefully.adv-a (pasv write.v))
;   -> '(carefully.adv-a (pasv written.v))
  (assert (member part-type '(PRESENT PAST nil)))
  (cond
    ((verb? vp)
     (let* ((head-verb (ulf:find-vp-head vp))
            (participle
              (cond
                ;; Present participle condition.
                ((eql part-type 'PRESENT)
                 (verb-to-present-participle! head-verb))
                ;; Various past participle conditions.
                ((and (eql part-type 'PAST) (atom head-verb))
                 (verb-to-past-participle! head-verb))
                ((and (eql part-type 'PAST) (pasv-lex-verb? head-verb))
                 (verb-to-past-participle! (second head-verb)))
                ((eql part-type 'PAST)
                 (error "verb ~s is not a form that can become a past participle"
                        head-verb))
                ;; Default cases when part-type is nil.
                ((atom head-verb)
                 (verb-to-present-participle! head-verb))
                ((pasv-lex-verb? head-verb)
                 (verb-to-past-participle! (second head-verb)))
                (t (error "verb ~s is not a form that can become a past participle"
                          head-verb)))))
       (ulf:replace-vp-head vp participle)))
    ;; If it isn't a verb phrase, just return.
    (t vp)))


(defun conjugate-infinitive (verb)
  (if (not (atom verb))
    verb
    (multiple-value-bind (word suffix) (split-by-suffix verb)
      (let ((pkg (symbol-package verb)))
        (ulf:add-suffix
          (intern
            (pattern-en-conjugate (string word) :tense 'infinitive)
            pkg)
          suffix :pkg pkg)))))

(defun infinitive? (verb)
  (equal verb (conjugate-infinitive verb)))


;; Convenience functions (also for TTT).
(defun vp-to-past-participle! (vp)
  (vp-to-participle! vp :part-type 'PAST))

(defun vp-to-present-participle! (vp)
  (vp-to-participle! vp :part-type 'PRESENT))

(defun term-to-subj! (term)
  (case term
    (me.pro 'i.pro)
    (us.pro 'we.pro)
    (her.pro 'she.pro)
    (him.pro 'he.pro)
    (them.pro 'they.pro)
    (whom.pro 'who.pro)
    (otherwise term)))

(defun term-to-obj! (term)
  (case term
    (i.pro 'me.pro)
    (we.pro 'us.pro)
    (she.pro 'her.pro)
    (he.pro 'him.pro)
    (they.pro 'them.pro)
    (whom.pro 'who.pro)
    (otherwise term)))

(defparameter *plur2surface*
  '(/ (plur _!)
      (pluralize! _!)))
(defparameter *tense2surface*
  '(/ ((!1 ulf:lex-tense?) _!2)
      (add-tense! (!1 _!2))))
(defparameter *tensed-pasv2surface*
  '(/ (!1 (lex-tense? (pasv _!)))
      (pasv2surface! !1)))
(defparameter *pasv2surface*
  '(/ (!1 (pasv _!))
      (pasv2surface! !1)))
(defparameter *tense-n-number2surface*
  '(/ ((!1 term?) (*1 phrasal-sent-op?)
                  (!2 pred?)
                  (*2 phrasal-sent-op?))
      ((term-to-subj! !1) *1 (conjugate-vp-head! !2 !1) *2)))
(defparameter *inv-copula-tense-n-number2surface*
  '(/ ((!1 (lex-tense? be.v))
       (*1 phrasal-sent-op?)
       (!2 term?)
       (*2 phrasal-sent-op?)
       (!3 pred?)
       (*3 phrasal-sent-op?))
      ((conjugate-vp-head! !1 !2) *1 (term-to-subj! !2) *2 !3 *3)))
(defparameter *inv-aux-tense-n-number2surface*
  '(/ ((!1 (lex-tense? (! aux? have.v)))
       (*1 phrasal-sent-op?)
       (!2 term?)
       (*2 phrasal-sent-op?)
       (!3 verb?)
       (*3 phrasal-sent-op?))
      ((conjugate-vp-head! !1 !2) *1 (term-to-subj! !2) *2 !3 *3)))
(defparameter *exist-there-tense-n-number2surface*
  '(/ (there.pro ((!1 (lex-tense? lex-verb?))
                  (*1 phrasal-sent-op?)
                  (!2 term?)
                  (*2 phrasal-sent-op?)))
      (there.pro ((conjugate-vp-head! !1 !2) *1 !2 *2))))
(defparameter *inv-exist-there-tense-n-number2surface*
  '(/ ((!1 (lex-tense? lex-verb?))
       there.pro
       (*1 phrasal-sent-op?)
       (!2 term?)
       (*2 phrasal-sent-op?))
      ((conjugate-vp-head! !1 !2) there.pro *1 !2 *2)))

;; This is dealing with the over simplification of "what is/are" questions.
;; Normally they should be inverted, but we allow them not to be so.
(defparameter *simple-what-is-tense-n-number2surface*
  '(/ (what.pro
       (*1 phrasal-sent-op?)
       ((!1 (lex-tense? be.v))
        (*2 phrasal-sent-op?)
        (= (!2 term?))))
      (what.pro *1 ((conjugate-vp-head! !1 !2) *2 (= !2)))))

(defparameter *most-n-morph*
  '(/ (most-n (!1 lex-adjective?) (!2 noun?))
      ((lex-superlative! !1) !2)))
(defparameter *most-morph*
  '(/ (most (!1 adj?) (*2 phrasal-sent-op?))
      ((ap-superlative! !1) *2)))

(defun subj2person! (subj)
  (if (atom subj)
    (let ((subj+ (ulf:make-explicit! subj)))
      (cond
        ;; ULF doesn't actually care about the i/me distinction in its symbols,
        ;; so we allow both. Based on the position in the ULF formula we can
        ;; determine if it's the subject.
        ((member subj+ '(i.pro me.pro we.pro us.pro)) 1)
        ((member subj+ '(you.pro ye.pro)) 2)
        (t 3)))
      3))

(defun conjugate-vp-head! (vp subj)
;``````````````````````````
; Conjugates the head of vp according to the tense attached to it and the
; number of the subject.
; Assumes there's no passive operator on the verb, since this should be appled
; after (tense (pasv <verb>)) is expanded to ((tense be.v) (<past part verb>
; ..))
  (let* ((num (if (plur-term? subj) 'PL 'SG))
         (pers (subj2person! subj))
         (hv (ulf:find-vp-head vp))
         (tense (if (or (tensed-verb? hv) (tensed-aux? hv)) (first hv) nil))
         (lex-verb (if tense (second hv) hv))
         conjugated pkg)
    (when (not (null lex-verb))
      (setf pkg (symbol-package lex-verb))
      (multiple-value-bind (word suffix) (split-by-suffix lex-verb)
        ;; Special cases that still need conjugation.
        (setf word
              (case word
                (be-to 'be)
                (be-destined 'be)
                (otherwise word)))
        (setf
          conjugated
          (add-suffix
            (cond
              ;; TODO: figure out a way to generate 'was' and 'were' alternatives for
              ;; (cf be.v). This needs to be integrated in general to the ulf2english
              ;; function so we can add these alternatives at multiple points in the
              ;; pipeline.
              ;; Special cases that don't need conjugation.
              ((eql word 'were) 'were)
              ((eql word 'would) 'would)
              ((eql word 'could) 'could)
              ((eql word 'should) 'should)
              ((and (eql word 'will) (member tense '(past cf))
                    (member suffix '(aux aux-s aux-v)))
               'would)
              ((and (eql word 'forsee) (eql tense 'past)) 'forsaw)
              ((not (is-surface-token? lex-verb)) word) ; {be}.v -> {be}.v
              (t
                (safe-intern
                  (if tense
                    (pattern-en-conjugate (string word) :tense (ulf2pen-tense tense) :number num :person pers)
                    (pattern-en-conjugate (string word) :number num :person pers))
                  pkg)))
            ; NB: special suffix so we don't recurse... TODO: rename as somthing more descriptive (e.g. conjugatedv)
            'vp-head :pkg pkg))))
    (ulf:replace-vp-head vp conjugated)))


(defparameter *participle-for-post-modifying-verbs*
;`````````````````````````````````````````````
; Transform noun and np post-modifying verbs to present-pariciple
; form. E.g. (n+preds man.n walk.v) ->
  '(/ ((!1 n+preds np+preds n+post) _!2 _*3 (!4 verb? tensed-verb?) _*5)
      (!1 _!2 _*3 (vp-to-participle! !4) _*5)))

(defparameter *participle-for-mod-x*
  '(/ ((!1 mod-n mod-a) (!2 verb?))
      (!1 (vp-to-participle! !2))))
(defparameter *participle-for-implicit-mod-x*
  '(/ ((!1 verb?) (!2 noun? adj?))
      ((vp-to-participle! !1) !2)))
(defparameter *participle-for-adv-a*
  '(/ (adv-a (!1 verb?))
      (adv-a (vp-to-participle! !1))))
(defparameter *pres-part-for-ka*
  '(/ (ka (!1 verb?))
      (vp-to-present-participle! !1)))

;; Functions to help with writing TTT rules on prog and perf since they have
;; tensed variants.
(defun prog2be! (proginst)
  (subst 'be.v 'prog proginst))

(defun perf2have! (perfinst)
  (subst 'have.v 'perf perfinst))


;; All the prog handling rules.
(defparameter *prog2surface*
  '(/ ((!1 prog (lex-tense? prog)) ; prog, (pres prog), etc.
       (*1 phrasal-sent-op?)       ; not, definitely.adv-s, etc.
       (!2 verb?))
      ((prog2be! !1) *1 (vp-to-present-participle! !2))))
(defparameter *inv-prog2surface*
  '(/ ((!1 prog (lex-tense? prog)) ; prog, (pres prog), etc.
       (*1 phrasal-sent-op?)       ; not, definitely.adv-s , etc.
       (!2 term?)                  ; he.pro, |John|, etc.
       (*2 phrasal-sent-op?)
       (!3 verb?) _*3)             ; verb + rest
      ;; Keep everything the same, except prog and the verb.
      ((prog2be! !1) *1 !2 *2 (vp-to-present-participle! !3) _*3)))

;; All the perf handling rules.
(defparameter *perf2surface*
  '(/ ((!1 perf (lex-tense? perf)) ; perf, (past perf), etc.
       (*1 phrasal-sent-op?)       ; not, probably.adv-s, etc.
       (!2 verb?))
      ((perf2have! !1) *1 (vp-to-past-participle! !2))))
(defparameter *inv-perf2surface*
  '(/ ((!1 perf (lex-tense? perf)) ; perf, (past perf), etc.
       (*1 phrasal-sent-op?)       ; not, probably.adv-s, etc.
       (!2 term?)                  ; he.pro, |John|, etc.
       (*2 phrasal-sent-op?)
       (!3 verb?) _*3)
      ((perf2have! !1) *1 !2 *2 (vp-to-past-participle! !3) _*3)))


(defun add-morphology (ulf)
  (util:unhide-ttt-ops
    (ttt:apply-rules
      (list
        ;; NB: THE ORDER HERE MATTERS
        ;; - Many later stages rely on perf and prog having already been
        ;;   processed.
        ;; - Some rules apply to the same stuff, but the more specific ones
        ;;   are done first.

        ;; Initial interactive changes.
        *prog2surface*
        *inv-prog2surface*
        *perf2surface*
        *inv-perf2surface*
        ;; Various participles
        *participle-for-post-modifying-verbs*
        *participle-for-adv-a*
        *participle-for-mod-x*
        *participle-for-implicit-mod-x*
        *pres-part-for-ka*
        ;; Core non-interactive pieces.
        *tensed-pasv2surface*
        *pasv2surface*
        *simple-what-is-tense-n-number2surface*
        *tense-n-number2surface*
        *inv-copula-tense-n-number2surface*
        *inv-aux-tense-n-number2surface*
        *exist-there-tense-n-number2surface*
        *inv-exist-there-tense-n-number2surface*
        ;*inv-simple-sub-tense-n-number2surface*
        ; NB: comment below when testing tense-n-number2surface, but uncomment during use.
        ;*tense2surface* ; default tense if above didn't work.
        *plur2surface*
        *most-n-morph*
        *most-morph*)
      (util:hide-ttt-ops ulf) :max-n 1000
      :rule-order :slow-forward)))


(defun capitalize-first (string)
  (if (zerop (length string))
      ""
      (let ((copy (copy-seq string)))
        (setf (char copy 0)
              (char-upcase (char copy 0)))
        copy)))


(defun capitalize-i (string)
  (re:regex-replace-all "\\bi\\b" string "I"))


(defun add-punct-curried (punct)
  (lambda (sent)
    (cl-strings:join (list sent punct) :separator "")))


;; Function that takes a relational noun in ULF and transforms it into bare
;; noun form. It's meant for TTT mapping, hence the exclamation mark ending.
(defun unrel-noun! (ulf)
  (multiple-value-bind (word suffix) (ulf:split-by-suffix ulf)
    (let ((pkg (symbol-package ulf))
          (wchars (util:split-into-atoms word))
          (tchars (util:split-into-atoms suffix)))
      (util:fuse-into-atom
        (append (reverse (nthcdr 3 (reverse wchars)))
                '(\.)
                tchars)
        :pkg pkg))))


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
  (cond
    ((and (listp ulf) (> (length ulf) 1) (eq (car ulf) 'sub))
     (extract-punctuation (third ulf)))
    ((and (listp ulf) (> (length ulf) 1) (eq (car ulf) 'rep))
     (extract-punctuation (second ulf)))
    ((and (listp ulf) (= (length ulf) 2) (member (cadr ulf) '(? !)))
     (string (cadr ulf)))
    ;; TODO: tag questions...
    ((and (listp ulf) (= (length ulf) 2) (eq (cadr ulf) '.?))
     (string "?"))
    (t ".")))

;; TODO: poss-by (see example (h) on page 34 of the guidelines)

(defvar *post-processed-equivalencies*
  '(;; Prepositional wh-relative clauses
    ("(?i)at which" "when")
    ("(?i)at time which" "when")
    ("(?i)on which" "where")
    ("(?i)at what place" "where")
    ("(?i)at which place" "where")
    ("(?i)at loc which" "where")
    ("(?i)at what time" "when")
    ("(?i)at which time" "when")
    ("(?i)in what way" "how")
    ("(?i)in which way" "how")
    ("(?i)on what place" "whereon")
    ("(?i)on which place" "whereon")
    ("(?i)on loc which" "whereon")
    ;; Emphatic wh-words
    ("(?i)what em" "what")
    ("(?i)how em" "how")
    ;; whoever, whatever, wherever
    ("any person" "whoever")
    ("any one" "whoever")
    ("any thing" "whatever")
    ("any place" "wherever")
    ))

(defvar *general-equivalencies*
  '(("anyone" "whoever")
    ("anything" "whatever")
    ("anywhere" "wherever")))

;; Change this to generate all possible equivalencies.
(defun apply-post-processed-equivalencies (str)
  (reduce
    #'(lambda (acc cur)
        (re:regex-replace-all (first cur) acc (second cur)))
    *post-processed-equivalencies* :initial-value str))


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
  (list ;; TODO: generalize this function to adding types to all the hole variables.
    (list #'set-of-to-and "Set-of to and.cc")
    (list #'(lambda (x) (ulf:add-info-to-sub-vars x :calling-package :ulf2english))
          "Add type/plurality info to 'sub' variables")
    (list #'(lambda (x) (ulf:add-info-to-relativizers x :calling-package :ulf2english))
     "Add info to relativiers")
    (list #'contextual-preprocess "Contextual preprocess")
    (list #'add-morphology "Adding morphology")
    (list #'quotes2surface! "Handle quotes")
    (list #'post-posses2surface! "Handle post-nominal possessive (i.e. 's)")
    (list #'(lambda (x) (remove-if-not #'is-surface-token? (alexandria:flatten x)))
     "Only retaining surface symbols")
    (list #'(lambda (x) (mapcar #'(lambda (y) (util:atom2str y)) x))
          "Stringify symbols")
    (list #'(lambda (x) (mapcar #'ulf:strip-suffix x)) "Strip suffixes")
    (list #'(lambda (x) (mapcar #'post-format-ulf-string x)) "Post-format strings")
    (list #'merge-det-thing-combos "Merge special determiner-noun combinations")
    (list #'(lambda (x) (cl-strings:join x :separator " ")) "Glue together")))


;; Maps a ULF formula to a corresponding surface string.
;; NB: currently this is incomplete and not fluent.
(defun ulf2english (inulf &key (add-punct? t) (capitalize-front? t))
  ;; TODO: make more sophisticated version (quotes, ds, etc.).

  ;; For now just drop all special operators and just take the suffixed tokens.
  ;; The only non-suffixed tokens that we preserve are "that", "not", "and",
  ;; "or", "to".
  (util:in-intern (inulf ulf :ulf2english)
    (let* ((idfn #'(lambda (x) x))
           (punct (extract-punctuation ulf))
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
      (funcall (compose
                 (if add-punct? add-punct-fn idfn)
                 (if capitalize-front? #'capitalize-first idfn)
                 #'capitalize-i
                 #'apply-post-processed-equivalencies)
               staged))))

