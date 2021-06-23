;;; Gene Louis Kim, 2-14-2019
;;; Unit tests for verifying the bugs independent of some dataset are fixed.

(in-package :ulf2english)

(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(setq *summarize-results* t)

(define-test no-type-error-on-number
  "Ensure that there isn't a type error on numbers in ULFs"
  (:tag :bugfixes :numbers)
  (let ((ulf '((SUB HOW.PQ
                    ((PRES DO.AUX-S) YOU.PRO
                     ((SAY.V (|"| 2 |"|)) (ADV-A (IN.P | Latin|)) *H)) ?)))
        (expected "How do you say 2 in Latin?"))
    ;; Check that it outputs a string.
    (assert-true (stringp (ulf2english ulf))
                 ulf)
    ;; Check that the output is actually correct.
    (assert-equal expected (ulf2english ulf)
                  expected (ulf2english ulf) ulf)))

;; TODO: clean this up by using lists of pairs....
(define-test participles-from-doc
  "Proper handling of participles (examples from documentation on predicate modification)"
  (:tag :bugfixes :participle)
  (let ((sent-ulf-pairs
          '(("I greeted the frequently returning member"
             (i.pro ((past greet.v)
                     (the.d ((mod-n (frequently.adv-f return.v))
                             (member-of.n *ref))))))
            ("We're required to submit a carefully written notice"
             (we.pro ((pres (pasv require.v))
                      (to (submit.v
                            (a.d ((mod-n (carefully.adv-a (pasv write.v)))
                                  notice.n)))))))
            ("Kenneth nervously watched the woman, alarmed by her gun"
             (|Kenneth|
               (nervously.adv-a
                 ((past watch.v) (the.d woman.n)
                                 (adv-a ((pasv alarm.v) (by.p-arg (her.d gun.n))))))))
            ("I went back to sleep, having heard this before"
             (i.pro ((past go.v) back.adv-a (to.p-arg (k sleep.n))
                                 (adv-a (perf (hear.v this.pro before.adv-e))))))
            ("Lifting weights for two hours, Ron developed sore muscles"
             (sub
               (adv-a (lift.v (k (plur weight.n))
                              (adv-e (for.p (two.d (plur hour.n))))))
               (|Ron| ((past develop.v) (k (sore.a (plur muscle.n))) *h))))
            ("Any student scoring a good grade on the exam will receive an award"
             ((any.d (n+preds student.n
                              (score.v (a.d (good.a grade.n))
                                       (on.p-arg (the.d exam.n)))))
              ((pres will.aux-s) (receive.v (an.d award.n)))))))
        ;; We don't care about punctuation, casing or spaces in these tests.
        (strclean (compose
                    #'cl-strings:clean
                    #'remove-punctuation
                    #'string-downcase)))
    (mapcar
      ;; For each pair, generate the ulf2english, clean the strings and compare.
      #'(lambda (x)
          (let ((expected (funcall strclean (first x)))
                (ulf (second x))
                generated variants)
            (setf generated (funcall strclean (ulf2english ulf)))
            (setf variants (gute:contraction-possibilities generated))
            (assert-true (member expected variants :test #'equal)
                         generated ulf)))
      sent-ulf-pairs)))

(define-test vocatives-from-doc
  "finding issues with vocatives (examples from documentation on vocatives)"
  (:tag :bugfixes :vocatives)
  (let ((sent-ulf-pairs
          '(("Mary, I see you."
             ((voc |Mary|)
              (I.pro ((pres see.v) you.pro))))
            ("I don't think I understand, Susan."
             ((I.pro ((pres do.aux-s)
                      not.adv-s
                      (think.v (tht (I.pro ((pres understand.v) {ref}.pro))))))
              (voc |Susan|)))
            ("My ill feelings towards you, Lex, are endless."
             ((My.d (n+preds (ill.a (plur feeling.n))
                             (towards.p-arg you.pro)))
              (voc |Lex|)
              ((pres be.v) endless.a)))
            ("You in the yellow shirt, call 911!"
             ((voc (np+preds you.pro (in.p (the.d (yellow.a shirt.n)))))
              ({you}.pro ((pres call.v) |911|)) !))
            ("John, you rascal, where have you been?"
             ((voc |John|)
              (voc (np+preds you.pro rascal.n))
              (sub (at.p (what.d place.n)) ((pres perf) you.pro (be.v *h))) ?))
            ("You rascal where have you been, John?"
             ((voc (np+preds you.pro rascal.n))
              (sub (at.p (what.d place.n)) ((pres perf) you.pro (be.v *h)))
              (voc |John|) ?))
            ("Why are ye fearful, O ye of little faith?"
             (((Why.adv-s ((pres be.v) ye.pro fearful.a))
               (voc-O (np+preds ye.pro (of.p (little.a faith.n))))) ?))))
        (strclean #'cl-strings:clean))
    (mapcar
      ;; for each pair, generate the ulf2english and compare
      #'(lambda (x)
          (let ((expected (funcall strclean (first x)))
                (ulf (second x))
                generated variants)
            (setf generated (funcall strclean (ulf2english ulf)))
            (setf variants (gute:contraction-possibilities generated))
            (assert-true (member expected variants :test #'equal)
                         generated ulf)))
      sent-ulf-pairs)))


(define-test named-predicates
  "Named predicate error detected during ulf inference experiments."
  (:tag :bugfixes :named-predicates)
  (let ((sent-ulf-pairs
          '(("name Li'l Abner 's favorite Indian drink"
             (({YOU}.PRO
                ((PRES NAME.V) ((|Li'l Abner| 'S) (FAVORITE.A (|Indian.A| DRINK.N)))))
              !))
            ("pollster Robert M. Teeter"
             ({THE}.D (N+PREDS POLLSTER.N (= |Robert M. Teeter|))))))
        (strclean #'identity))
    (mapcar
      ;; for each pair, generate the ulf2english and compare
      #'(lambda (x)
          (let ((expected (funcall strclean (first x)))
                (ulf (second x))
                generated variants)
            (setf generated (funcall strclean (ulf2english ulf
                                                           :add-punct? nil
                                                           :capitalize-front? nil)))
            (setf variants (gute:contraction-possibilities generated))
            (assert-true (member expected variants :test #'equal)
                         generated ulf)))
      sent-ulf-pairs)))


(define-test do-inflection
  "Do inflection error raised by Ben https://github.com/genelkim/ulf2english/issues/37"
  (:tag :bugfixes :do :inflection)
  (let ((ulf '((THE.D (|Starbucks| BLOCK.N))
               ((PRES DO.AUX-S) NOT.ADV-S
                (TOUCH.V (THE.D (|Twitter| BLOCK.N))))))
        (expected "The Starbucks block does not touch the Twitter block."))
    (assert-equal expected
                  (ulf2english ulf)
                  ulf)))

(define-test adverbialization
  "Adveribalizing adjectives until adv-* operators."
  (:tag :bugfixes :adverbialization)
  (assert-equal
    "What block did I recently move?"
    (ulf2english '((sub (what.d block.n)
                        ((past do.aux-s) I.pro
                                         ((adv-e recent.a) (move.v *h))))
                   ?)))
  (assert-equal
    "John ran quickly."
    (ulf2english '(|John| ((past run.v) (adv-a quick.a)))))
  (assert-equal
    "The syrup slowly rolls off the pancakes."
    (ulf2english '((the.d syrup.n)
                   ((adv-a slow.a)
                    (pres roll.v)
                    (adv-a (off.p (the.d (plur pancake.n))))))))
  (assert-equal
    "You fell off your seat mighty clumsily."
    (ulf2english
      '(you.pro ((past fall.v)
                 (adv-a (off.p (your.d seat.n)))
                 (adv-a (mighty.mod-a clumsy.a))))))
  (assert-equal
    "Then you will arrive too early."
    (ulf2english '(then.adv-s
                    (you.pro ((pres will.aux-s)
                              (arrive.v (adv-e (too.mod-a early.a)))))))))

(define-test ka-tenses
  "Tenses not being added correctly for kinds of actions."
  (:tag :bugfixes :tense)
  (assert-equal
    "The next step is putting the Twitter block on the Texaco block ."
    (ulf2english '(((THE.D (NEXT.A STEP.N))
                    ((PRES BE.V)
                     (= (KA (PUT.V (THE.D (|Twitter| BLOCK.N))
                                   (ON.P (THE.D (|Texaco| BLOCK.N)))))))) \.)))
  (assert-equal
    "The next step is putting the Twitter block on it ."
    (ulf2english '(((THE.D (NEXT.A STEP.N))
                    ((PRES BE.V)
                     (= (KA (PUT.V (THE.D (|Twitter| BLOCK.N))
                                   (ON.P IT.PRO)))))) \.)))
  (assert-equal
    "The next step is putting that on it ."
    (ulf2english '(((THE.D (NEXT.A STEP.N))
                    ((PRES BE.V)
                     (= (KA (PUT.V THAT.PRO
                                   (ON.P IT.PRO)))))) \.)))
  (assert-equal
    "The step is putting that on it ."
    (ulf2english '(((THE.D STEP.N)
                    ((PRES BE.V)
                     (= (KA (PUT.V THAT.PRO
                                   (ON.P IT.PRO)))))) \.)))
  (assert-equal
    "It is putting that on it ."
    (ulf2english '((IT.PRO
                    ((PRES BE.V)
                     (= (KA (PUT.V THAT.PRO
                                   (ON.P IT.PRO)))))) \.)))
  (assert-equal
    "It is putting that on it."
    (ulf2english '(IT.PRO
                    ((PRES BE.V)
                     (= (KA (PUT.V THAT.PRO
                                   (ON.P IT.PRO))))))))
  (assert-equal
    "That is running."
    (ulf2english '(THAT.PRO ((pres be.v) (= (ka run.v)))))))

(define-test embedded-tenses
  "Tenses not being handled correctly on embedded tenses."
  (:tag :bugfixes :tenses)
  (assert-equal
    "Where the Target block was before it was on the Starbucks block?"
    (ulf2english '((SUB (AT.P (WHAT.D PLACE.N))
                        ((THE.D (|Target| BLOCK.N))
                         ((PAST BE.V) *H
                                      (ADV-E (BEFORE.P (KE (IT.PRO
                                                             ((PAST BE.V)
                                                              (ON.P (THE.D (|Starbucks| BLOCK.N))))))))))) ?)))
  (assert-equal
    "You do not know where I moved the Texaco block."
    (ulf2english '(YOU.PRO ((PRES DO.AUX-S) NOT
                            (KNOW.V
                             (ANS-TO (SUB WHERE.PQ
                                          (I.PRO ((PAST MOVE.V)
                                                  (THE.D (|Texaco| BLOCK.N)) *H)))))))))
  (assert-equal
    "You do not know where I moved the Texaco block."
    (ulf2english '(YOU.PRO ((PRES DO.AUX-S) NOT
                            (KNOW.V
                             (ANS-TO (SUB WHERE.A
                                          (I.PRO ((PAST MOVE.V)
                                                  (THE.D (|Texaco| BLOCK.N)) (ADV-A *H))))))))))
  (assert-equal
    "You do not know where I moved the Texaco block."
    (ulf2english '(YOU.PRO ((PRES DO.AUX-S) NOT
                            (KNOW.V
                             (ANS-TO (SUB (at-loc.p which.pro)
                                          (I.PRO ((PAST MOVE.V)
                                                  (THE.D (|Texaco| BLOCK.N)) (ADV-A *H))))))))))
  (assert-equal
    "You do not know who I moved."
    (ulf2english '(YOU.PRO ((PRES DO.AUX-S) NOT
                            (KNOW.V
                             (ANS-TO (SUB WHO.PRO
                                          (I.PRO ((PAST MOVE.V) *H))))))))))

(define-test should-be
  "Conjugation on 'should be'"
  (:tag :bugfixes :should-be)
  (assert-equal
    "The Twitter block should be clear."
    (ulf2english '((the.d (|Twitter| block.n))
                   ((pres should.aux-s) (be.v clear.a))))))

(define-test tails
  "Missing plural"
  (:tag :bugfixes :plur)
  (assert-equal
    "All cats have tails."
    (ulf2english '((ALL.D (PLUR CAT.N)) ((PRES HAVE.V) (K (PLUR TAIL.N)))))))

