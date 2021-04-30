;; Gene Louis Kim, 4-1-2019
;; Unit tests based on the ULF guidelines.

(in-package :ulf2english)

(setf *print-failures* t)
(setf *print-errors* t)
(setf *print-summary* t)
(setf *summarize-results* t)

(defun remove-commas (str)
  (remove #\, str))

(define-test counterfactuals
  "Examples from the section Counterfactuals & Conditionals"
  (:tag :guidelines :cf)
  (let ((sent-ulf-pairs
          '(("I wish I was rich"
             (i.pro ((pres wish.v) (tht (i.pro ((cf be.v) rich.a))))))
            ("I wish I believed you"
             (I.pro ((pres wish.v) (tht (I.pro ((cf believe.v) you.pro))))))
            ("I wish I were rich"
             (I.pro ((pres wish.v) (tht (I.pro ((cf were.v) rich.a))))))
            ("If I was rich, I would own a boat."
             ((if.ps (I.pro ((cf be.v) rich.a)))
              (I.pro ((cf will.aux-s) (own.v (a.d boat.n))))))
            ("Were he to leave, the project would collapse"
             (((cf be-destined.aux-v) he.pro leave.v)
              ((the.d project.n) ((cf will.aux-s) collapse.v))))
            ("Had I forseen this, I would never have participated"
             (((cf perf) I.pro (forsee.v this.pro))
              (I.pro ((cf will.aux-s) never.adv-e (perf participate.v)))))
            ("If I had been rich, I would own a boat"
             ((If.ps (I.pro ((cf perf) (be.v rich.a))))
              (I.pro ((pres would.aux-s) (own.v (a.d boat.n))))))
            ("If I had been rich, then I would have owned a boat"
             ((If.ps (I.pro ((cf perf) (be.v rich.a))))
              (then.adv-s (I.pro ((cf will.aux-s) (perf (own.v (a.d boat.n))))))))))
        ;; We don't care about punctuation, casing or spaces in these tests.
        (strclean (compose
                    #'cl-strings:clean
                    #'remove-punctuation)))
      (mapcar
        ;; For each pair, generate the ulf2english, clean the strings and compare.
        #'(lambda (x)
            (let ((expected (funcall strclean (first x)))
                  (ulf (second x))
                  generated)
              (setf generated (funcall strclean (ulf2english ulf)))
              (assert-equal expected generated
                            expected generated ulf)))
        sent-ulf-pairs)))


(define-test yes-no
  "Examples from the Yes-No subsection"
  (:tag :guidelines :yes-no)
  (let ((strclean (gute:compose #'cl-strings:clean
                                #'remove-punctuation)))
    (assert-equal (funcall strclean "Yes")
                  (funcall strclean (ulf2english 'yes.yn)))

    (let* ((expected (funcall strclean "Uh-huh, that's the plan"))
           (generated (funcall strclean
                              (ulf2english '(Uh-huh.yn (that.pro ((pres be.v) (the.d plan.n)))))))
           (variants (gute:contraction-possibilities generated)))
      (assert-true (member expected variants :test #'equal)
                   generated))
    (assert-equal (funcall strclean "Definitely yes")
                  (funcall strclean (ulf2english '(Definitely.adv-s yes.yn))))
    (assert-equal (funcall strclean "Yes, definitely")
                  (funcall strclean (ulf2english '(Yes.yn (pu definitely.adv-s)))))
    (assert-equal (funcall strclean "Surprisingly, no")
                  (funcall strclean (ulf2english '(Surprisingly.adv-s no.yn))))))

(define-test emphatic-wh
  "Examples from the Exclamatory/Emphatic Wh-words section"
  (:tag :guidelines :emphatic-wh)
  (let ((sent-ulf-pairs
          '(("What a beautiful car that is!"
             (sub (= (What-em.d (= (a.d (beautiful.a car.n)))))
                  (that.pro ((pres be.v) *h))))
            ("What beautiful cars these are"
             (sub (= (What-em.d (beautiful.a (plur car.n))))
                  (these.pro ((pres be.v) *h))))
            ("What a strong person he is"
             (sub (= (What-em.d (= (a.d (strong.a person.n)))))
                  (he.pro ((pres be.v) *h))))
            ("What smart kids you are"
             (sub (= (What-em.d (smart.a (plur kid.n))))
                  (you.pro ((pres be.v) *h))))
            ("What a mess he made!"
             (sub (What-em.d (= (a.d mess.n)))
                  (he.pro ((past make.v) *h))))
            ("What a beautiful car!"
             (sub (= (What-em.d (= (a.d (beautiful.a car.n)))))
                  ({that}.pro ((pres {be}.v) *h))))
            ("What an idea!"
             (sub (= (What-em.d (= (an.d idea.n))))
                  ({that}.pro ((pres {be}.v) *h))))
            ("What a charming actress!"
             (sub (= (What-em.d (= (a.d (charming.a actress.n)))))
                  ({she}.pro ((pres {be}.v) *h))))
            ("What a bunch of beautiful pictures!"
             (sub (= (What-em.d (= (a.d (n+preds bunch.n
                                                 (of.p (k (beautiful.a
                                                            (plur picture.n)))))))))
                  ({those}.pro ((pres {be}.v) *h))))
            ("What a beautiful car you bought!"
             (sub (What-em.d (= (a.d (beautiful.a car.n))))
                  (you.pro ((past buy.v) *h))))
            ("How studious he is!"
             (sub (How-em.mod-a studious.a) (he.pro ((pres be.v) *h))))
            ("How curious they are!"
             (sub (How-em.mod-a curious.a) (they.pro ((pres be.v) *h))))
            ("How strange!"
             (sub (How-em.mod-a strange.a) ({that}.pro ((pres {be}.v) *h))))
            ("How I used to enjoy this!"
             (sub How-em.adv-a (I.pro (((past use.v) (to (enjoy.v this.pro))) *h))))
            ("You should see what beautiful car he bought"
             (You.pro ((pres should.aux-v) (see.v
                                             (ans-to (sub (what.d (beautiful.a car.n))
                                                          (he.pro ((past buy.v) *h))))))))
            ("You should see what a beautiful car he bought"
             (You.pro ((pres should.aux-v) (see.v
                                             (ans-to (sub (What-em.d (= (a.d (beautiful.a car.n))))
                                                          (he.pro ((past buy.v) *h))))))))
            ("You should see what model of car he bought"
             (You.pro ((pres should.aux-v) (see.v
                                             (ans-to (sub (what.d (n+preds (model-of.n (k car.n))))
                                                          (he.pro ((past buy.v) *h))))))))
            ("I know in how deep a financial hole he now is, because of his risky investments"
             (I.pro ((pres know.v) (ans-to
                                     (sub (in.p (sub (how.mod-a deep.a)
                                                     (a.d (*h (financial.a hole.n)))))
                                          (he.pro now.adv-e ((pres be.v) *h)
                                                  (adv-s (because_of.p
                                                           (his.d (risky.a (plur investment.n)))))))))))
            ("In how deep a financial hole he now is, because of his risky investments!"
             (sub (In.p (sub (how-em.mod-a deep.a)
                             (a.d (*h (financial.a hole.n)))))
                  (he.pro now.adv-e ((pres be.v) *h)
                          (adv-s (because_of.p
                                   (his.d (risky.a (plur investment.n))))))))))
        (strclean (compose
                    #'cl-strings:clean
                    #'remove-punctuation)))
    (mapcar
        ;; For each pair, generate the ulf2english, clean the strings and compare.
        #'(lambda (x)
            (let ((expected (funcall strclean (first x)))
                  (ulf (second x))
                  generated)
              (setf generated (funcall strclean (ulf2english ulf)))
              (assert-equal expected generated
                            expected generated ulf)))
        sent-ulf-pairs)))


(define-test imperatives
  "Imperative examples from the guidelines"
  (:tag :guidelines :imperatives)
  (let ((sent-ulf-pairs
          '(("Go home!"
             (({you}.pro ((pres go.v) (k home.n))) !))
            ("John, go home!"
             ((voc |John|) ({you}.pro ((pres go.v) (k home.n))) !))))
        (strclean (compose
                    #'cl-strings:clean
                    #'remove-commas)))
    (mapcar
      ;; For each pair, generate the ulf2english, clean the strings and compare.
      #'(lambda (x)
          (let ((expected (funcall strclean (first x)))
                (ulf (second x))
                generated)
            (setf generated (funcall strclean (ulf2english ulf)))
            (assert-equal expected generated
                          expected generated ulf)))
      sent-ulf-pairs)))

(define-test domain-specific
  "Examples from the domain specific section in the ULF guidelines"
  (:tag :guidelines :ds)
  (let ((sent-ulf-pairs
          '(("555 555-5555" (ds phone-number "555 555-5555"))
            ("(555)555-5555" (ds phone-number "(555)555-5555"))
            ("5555555" (ds phone-number "5555555"))
            ("5:30pm" (ds date-time "5:30pm"))
            ("June 18th 2017" (ds date-time "June 18th 2017"))
            ("quarter after 3" (ds date-time "quarter after 3"))
            ("$50.12" (ds currency "$50.12"))
            ("Fifty dollars and 12 cents" (ds currency "Fifty dollars and 12 cents"))
            ("e30" (ds currency "e30"))
            ("880 Linden Ave" (ds address "880 Linden Ave"))
            ("Rochester NY 14620" (ds address "Rochester NY 14620"))
            ("bonjour monsieur" (ds fws "bonjour monsieur"))
            ("君の名は" (ds fws "君の名は"))
            ("dm-drogerie markt" (ds fws "dm-drogerie markt"))
            ("5 degrees Celsius" (ds temp "5 degrees Celsius"))
            ("5'11¨" (ds length "5'11¨"))
            ("seven meters" (ds length "seven meters"))
            ("80km" (ds length "80km"))
            ("17kph" (ds speed "17kph"))
            ("50mile per hour" (ds speed "50mile per hour"))
            ("8.2 m/s" (ds speed "8.2 m/s"))
            ("whhhatre yooooouuuuse doeeeein" (ds unk "whhhatre yooooouuuuse doeeeein")))))
    (mapcar
      ;; For each pair, generate the ulf2english, clean the strings and compare.
      #'(lambda (x)
          (let ((expected (first x))
                (ulf (second x))
                generated)
            (setf generated (ulf2english ulf :add-punct? nil :capitalize-front? nil))
            (assert-equal expected generated
                          expected generated ulf)))
      sent-ulf-pairs)))

