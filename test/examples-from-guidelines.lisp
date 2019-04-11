;; Gene Louis Kim, 4-1-2019
;; Unit tests based on the ULF guidelines.

(in-package :ulf2english)

(setf *print-failures* t)
(setf *print-errors* t)
(setf *print-summary* t)
(setf *summarize-results* t)

(define-test counterfactuals
  "Examples from the section Counterfactuals & Conditionals"
  (:tag :guidelines :cf)
  (assert-equal "I wish I was rich"
                (ulf2english '(i.pro ((pres wish.v) (tht (i.pro ((cf be.v) rich.a)))))))
  (assert-equal "I wish I believed you"
                (ulf2english '(I.pro ((pres wish.v) (tht (I.pro ((cf believe.v) you.pro)))))))
  (assert-equal "I wish I were rich"
                (ulf2english '(I.pro ((pres wish.v) (tht (I.pro ((cf were.v) rich.a)))))))
  (assert-equal "If I was rich, I would own a boat."
                (ulf2english '((if.ps (I.pro ((cf be.v) rich.a)))
                               (I.pro ((cf will.aux-s) (own.v (a.d boat.n)))))))
  (assert-equal "Were he to leave, the project would collapse"
                (ulf2english '(((cf be-destined.aux-v) he.pro leave.v)
                               ((the.d project.n) ((cf will.aux-s) collapse.v)))))
  (assert-equal "Had I forseen this, I would never have participated"
                (ulf2english '(((cf perf) I.pro (forsee.v this.pro))
                               (I.pro ((cf will.aux-s) never.adv-e (perf participate.v))))))
  (assert-equal "If I had been rich, I would own a boat"
                (ulf2english '((If.ps (I.pro ((cf perf) (be.v rich.a))))
                               (I.pro ((pres would.aux-s) (own.v (a.d boat.n)))))))
  (assert-equal "If I had been rich, then I would have owned a boat"
                (ulf2english '((If.ps (I.pro ((cf perf) (be.v rich.a))))
                               (then.adv-s (I.pro ((cf will.aux-s) (perf (own.v (a.d boat.n))))))))))

(define-test yes-no
  "Examples from the Yes-No subsection"
  (:tag :guidelines :yes-no)
  (let ((strclean (util:compose #'cl-strings:clean
                                #'remove-punctuation)))
    (assert-equal (funcall strclean "Yes")
                  (funcall strclean (ulf2english 'yes.yn)))

    (let* ((expected (funcall strclean "Uh-huh, that's the plan"))
           (generated (funcall strclean
                              (ulf2english '(Uh-huh.yn (that.pro ((pres be.v) (the.d plan.n)))))))
           (variants (util:contraction-possibilities generated)))
      (assert-true (member expected variants :test #'equal)
                   generated))
    (assert-equal (funcall strclean "Definitely yes")
                  (funcall strclean (ulf2english '(Definitely.adv-s yes.yn))))
    (assert-equal (funcall strclean "Yes, definitely")
                  (funcall strclean (ulf2english '(Yes.yn (pu definitely.adv-s)))))
    (assert-equal (funcall strclean "Surprisingly, no")
                  (funcall strclean (ulf2english '(Surprisingly.adv-s no.yn))))))

