;; Unit tests for ulf2english on running from an external package.
;; Uses lisp-unit.

(in-package :cl-user)

(ql:quickload :lisp-unit)
(use-package :lisp-unit)

(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(setq *summarize-results* t)

;;
;; Tests based on blocks world.
;;

;; Macro for defining tests with less writing.
;(defmacro gen-blocks-qa-test (name tags sentence ulf)
(defmacro gen-ext-pkg-test (name tags sentence ulf)
  `(define-test ,name
      ,(format nil "Testing from an external package sentence: '~a'" sentence)
      (:tag :ext-pkg ,@tags)
      (let ((ulf ,ulf)
            (sentence ,sentence))
        (assert-equal sentence (ulf2english:ulf2english ulf)
                      sentence (ulf2english:ulf2english ulf) ulf))))

;;
;; Gene
;;

;; Is the Nvidia block to the left of the Texaco block?
;; Is the McDonalds block on top of the SRI block?
;; Is the Starbucks block near the Toyota block?
;; Is the Toyota block between the Nvidia block and the Target block?
;; Is the SRI block fully on top of any red block?
;; Does the Toyota block face the Nvidia block?
;; Does the tallest stack have a red block on top?
;; Is the Toyota block a part of some stack?
;; Is the Target block a part of some row?
;; Does any row contain a red block?
;; Are any two green blocks touching?
;; Are any two stacks near each other?
;; Does the SRI block have anything on it?
;; Are the Nvidia and the SRI blocks in the same stack?
;; Is the Toyota block below the Texaco block?
;; Is the Starbucks block on top of any row?
;; Are the Nvidia and the McDonalds blocks side by side?
;; Are all the red blocks near the Nvidia block?
;; Is every blue block behind some other block?
;; Is the Texaco block between any two blocks?
;; Is the Target block slightly to the left of some red block?
;; Is there a block between the McDonalds and the SRI blocks?
;; Is there a block near the Target block?
;; Is there a red block near a blue block?
;; Is there a block touching the Nvidia block?
;; Is there anything at the front left corner of the table?
;; Is there a row consisting of three blocks?

(gen-ext-pkg-test extpkg-gk1
  (:gene-extpkg)
  "Is the Nvidia block to the left of the Texaco block?"
  '(((pres be.v) (the.d (| Nvidia| block.n))
     (to.p (the.d (left-of.n (the.d | Texaco| block.n))))) ?))
(gen-ext-pkg-test extpkg-gk2
  (:gene-extpkg)
  "Is the McDonalds block on top of the SRI block?"
  '(((pres be.v) (the.d (| McDonalds| block.n))
     (on.p ({the}.d (top-of.n (the.d (| SRI| block.n)))))) ?))
(gen-ext-pkg-test extpkg-gk3
  (:gene-extpkg)
  "Is the Starbucks block near the Toyota block?"
  '(((pres be.v) (the.d (| Starbucks| block.n))
     (near.p (the.d (| Toyota| block.n)))) ?))
(gen-ext-pkg-test extpkg-gk4
  (:gene-extpkg)
  "Is the Toyota block between the Nvidia block and the Target block?"
  '(((pres be.v) (the.d (| Toyota| block.n))
     (between.p ((the.d (| Nvidia| block.n)) and.cc
                 (the.d (| Target| block.n))))) ?))
(gen-ext-pkg-test extpkg-gk5
  (:gene-extpkg)
  "Is the SRI block fully on top of any red block?"
  '(((pres be.v) (the.d (| SRI| block.n))
     (fully.mod-a (on.p ({the}.d (top-of.n (any.d (red.a block.n))))))) ?))
(gen-ext-pkg-test extpkg-gk6
  (:gene-extpkg)
  "Does the Toyota block face the Nvidia block?"
  '(((pres do.aux-s) (the.d (| Toyota| block.n))
     (face.v (the.d (| Nvidia| block.n)))) ?))
(gen-ext-pkg-test extpkg-gk7
  (:gene-extpkg)
  "Does the tallest stack have a red block on top?"
  '(((pres do.aux-s) (the.d (most-n tall.a stack.n))
     (have.v (a.d (red.a block.n)) (on.p ({the}.d (top-of.n *ref))))) ?))
(gen-ext-pkg-test extpkg-gk8
  (:gene-extpkg)
  "Is the Toyota block a part of some stack?"
  '(((pres be.v) (the.d (| Toyota| block.n))
     (= (a.d (part-of.n (some.d stack.n))))) ?))
(gen-ext-pkg-test extpkg-gk9
  (:gene-extpkg)
  "Is the Target block a part of some row?"
  '(((pres be.v) (the.d (| Target| block.n))
     (= (a.d (part-of.n (some.d row.n))))) ?))
(gen-ext-pkg-test extpkg-gk10
  (:gene-extpkg)
  "Does any row contain a red block?"
  '(((pres do.aux-s) (any.d row.n)
     (contain.v (a.d (red.a block.n)))) ?))
(gen-ext-pkg-test extpkg-gk11
  (:gene-extpkg)
  "Are any two green blocks touching?"
  '(((pres prog) (any.d (two.a (green.a (plur block.n))))
     touch.v) ?))
(gen-ext-pkg-test extpkg-gk12
  (:gene-extpkg)
  "Are any two stacks near each other?"
  '(((pres be.v) (any.d (two.a (plur stack.n)))
     (near.p (each.d (other.n {ref}.n)))) ?)) ;; maybe we can just write each_other.pro?
(gen-ext-pkg-test extpkg-gk13
  (:gene-extpkg)
  "Does the SRI block have anything on it?"
  '(((pres do.aux-s) (the.d (| SRI| block.n))
     (have.v anything.pro (on.p it.pro))) ?))
    ;; TODO: update the formula below once we finalize how to deal with this "blocks"
(gen-ext-pkg-test extpkg-gk14
  (:gene-extpkg)
  "Are the Nvidia and the SRI blocks in the same stack?"
  '(((pres be.v) (the.d ((| Nvidia| and.cc | SRI|) (plur block.n)))
     (in.p (the.d (same.a stack.n)))) ?))
(gen-ext-pkg-test extpkg-gk15
  (:gene-extpkg)
  "Is the Toyota block below the Texaco block?"
  '(((pres be.v) (the.d (| Toyota| block.n))
     (below.p (the.d (| Texaco| block.n)))) ?))
(gen-ext-pkg-test extpkg-gk16
  (:gene-extpkg)
  "Is the Starbucks block on top of any row?"
  '(((pres be.v) (the.d (| Starbucks| block.n))
     (on.p ({the}.d (top-of.n (any.d row.n))))) ?))
    ;; TODO: the formula below doesn't seem right since two separate 'plur'
    ;; operators seems to preclude having one Nvidia and one McDonalds block
(gen-ext-pkg-test extpkg-gk17
  (:gene-extpkg)
  "Are the Nvidia and the McDonalds blocks side by side?"
  '(((pres be.v) ((the.d (| Nvidia| (plur {block}.n))) and.cc
                  (the.d (| McDonalds| (plur block.n))))
     side_by_side.a) ?))
    ;; side_by_side.a
    ;; == (lambda x (all y [y (member-of.n x)]
    ;;            (exists z [[z (member-of.n x)] ^ (not [z = y])]
    ;;              (exists a [a (side-of.n y)]
    ;;                (exists b [b (side-of.n z)]
    ;;                [a by.p b])))))
    ;; Or something like this... Too complicated to break down in ULF.
(gen-ext-pkg-test extpkg-gk18
  (:gene-extpkg)
  "Are all the red blocks near the Nvidia block?"
  '(((pres be.v) (all.d ({of}.p (the.d (red.a (plur block.n)))))
     (near.p (the.d (| Nvidia| block.n)))) ?))
(gen-ext-pkg-test extpkg-gk19
  (:gene-extpkg)
  "Is every blue block behind some other block?"
  '(((pres be.v) (every.d (blue.a block.n))
     (behind.p (some.d (other.a block.n)))) ?))
(gen-ext-pkg-test extpkg-gk20
  (:gene-extpkg)
  "Is the Texaco block between any two blocks?"
  '(((pres be.v) (the.d (| Texaco| block.n))
     (between.p (any.d (two.a (plur block.n))))) ?))
(gen-ext-pkg-test extpkg-gk21
  (:gene-extpkg)
  "Is the Target block slightly to the left of some red block?"
  '(((pres be.v) (the.d (| Target| block.n))
     (slightly.mod-a
      (to.p (the.d (left-of.n (some.d (red.a block.n))))))) ?))
(gen-ext-pkg-test extpkg-gk22
    ;; TODO: update the formula below once we finalize how to deal with this "blocks"
  (:gene-extpkg)
  "Is there a block between the McDonalds and the SRI blocks?"
  '(((pres be.v) there.pro
     (a.d (n+preds block.n
                   (between.p ((the.d (| McDonalds| (plur {block}.n))) and.cc
                               (the.d (| SRI| (plur block.n)))))))) ?))
(gen-ext-pkg-test extpkg-gk23
  (:gene-extpkg)
  "Is there a block near the Target block?"
  '(((pres be.v) there.pro
     (a.d (n+preds block.n
                   (near.p (the.d (| Target| block.n)))))) ?))
(gen-ext-pkg-test extpkg-gk24
  (:gene-extpkg)
  "Is there a red block near a blue block?"
  '(((pres be.v) there.pro
     (a.d (n+preds (red.a block.n)
                   (near.p (a.d (blue.a block.n)))))) ?))
(gen-ext-pkg-test extpkg-gk25
  (:gene-extpkg)
  "Is there a block touching the Nvidia block?"
  '(((pres be.v) there.pro
     (a.d (n+preds block.n
                   (touch.v (the.d (| Nvidia| block.n)))))) ?))
(gen-ext-pkg-test extpkg-gk26
  (:gene-extpkg)
  "Is there anything at the front left corner of the table?"
  '(((pres be.v) there.pro
     (any.d (n+preds thing.n
                     (at.p (the.d (front.a (left.a
                             (corner-of.n (the.d table.n))))))))) ?))
(gen-ext-pkg-test extpkg-gk27
  (:gene-extpkg)
  "Is there a row consisting of three blocks?"
  '(((pres be.v) there.pro
     (a.d (n+preds row.n
                   (consist.v (of.p-arg (three.d (plur block.n))))))) ?))

;; By Gene, taken from the set initially assigned to Tianyi.
(gen-ext-pkg-test extpkg-gk28
  (:gene-extpkg)
  "Which block is on two other blocks?"
  '(((which.d block.n)
     ((pres be.v) (on.p (two.d (other.a (plur block.n)))))) ?))
(gen-ext-pkg-test extpkg-gk29
  (:gene-extpkg)
  "Which block is in the center of the table?"
  '(((which.d block.n)
     ((pres be.v) (in.p (the.d (center-of.n (the.d table.n)))))) ?))
(gen-ext-pkg-test extpkg-gk30
  (:gene-extpkg)
  "What blocks are in the longest row?"
  '(((what.d (plur block.n))
     ((pres be.v) (in.p (the.d (most-n long.a row.n))))) ?))
(gen-ext-pkg-test extpkg-gk31
  (:gene-extpkg)
  "What block is halfway on top of any other block?"
  '(((what.d block.n)
     ((pres be.v)
      ((mod-a halfway.a)
       (on.p ({the}.d (top-of.n (any.d (other.a block.n)))))))) ?))
(gen-ext-pkg-test extpkg-gk32
  (:gene-extpkg)
  "Which block is side by side with the Texaco block?"
  '(((which.d block.n)
     ((pres be.v)
      (side_by_side.a (with.p-arg (the.d (| Texaco| block.n)))))) ?))
(gen-ext-pkg-test extpkg-gk33
  (:gene-extpkg)
  "What is the farthest block from the center of the table?"
  '((what.pro ((pres be.v)
               (the.d (rep ((most (far.a *p)) block.n)
                           (mod-a (from.p (the.d (center-of.n (the.d table.n))))))))) ?))
(gen-ext-pkg-test extpkg-gk34
  (:gene-extpkg)
  "Which block the Nvidia block is on top of?"
  '((sub (which.d block.n)
         ((the.d (| Nvidia| block.n))
          ((pres be.v) (on.p ({the}.d (top-of.n *h)))))) ?))
(gen-ext-pkg-test extpkg-gk35
  (:gene-extpkg)
  "Which block is very close to the front edge of the table?"
  '(((which.d block.n)
     ((pres be.v)
      ((very.mod-a close.a)
       (mod-a (to.p (the.d (front.n (edge-of.n (the.d table.n))))))))) ?))
(gen-ext-pkg-test extpkg-gk36
  (:gene-extpkg)
  "What is in the middle of the table?"
  '((what.pro ((pres be.v)
               (in.p (the.d (middle-of.n (the.d table.n)))))) ?))
(gen-ext-pkg-test extpkg-gk37
  (:gene-extpkg)
  "Which red block is the closest to the Toyota block?"
  '(((which.d (red.a block.n))
     ((pres be.v)
      (the.d (n+preds ({red}.a {block}.n)
                      (most (close.a (mod-a (to.p (the.d (| Toyota| block.n)))))))))) ?))
(gen-ext-pkg-test extpkg-gk38
  (:gene-extpkg)
  "Which red blocks are directly on the table?"
  '(((which.d (red.a (plur block.n)))
     ((pres be.v) directly.adv-a (on.p (the.d table.n)))) ?))
(gen-ext-pkg-test extpkg-gk39
  (:gene-extpkg)
  "Which blue block the Nvidia block is not near to?"
  '((sub (which.d (blue.a block.n))
         ((the.d (| Nvidia| block.n))
          ((pres be.v) not (near.a (mod-a (to.p *h)))))) ?))
(gen-ext-pkg-test extpkg-gk40
  (:gene-extpkg)
  "Where is the Toyota block?"
  '((sub where.a
         ((pres be.v) (the.d (| Toyota| block.n)) *h)) ?))

