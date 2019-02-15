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

