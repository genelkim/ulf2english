;; Unit tests for ulf2english on the blocks world QA question set.
;; Uses lisp-unit.

(in-package :ulf2english)

(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(setq *summarize-results* t)

;; Macro for defining tests with less writing.
(defmacro gen-blocks-qa-test (name tags sentence ulf)
  `(define-test ,name
      ,(format nil "Blocks world QA sentence: '~a'" sentence)
      (:tag :blocks-world-qa ,@tags)
      (let ((ulf ,ulf)
            (sentence ,sentence))
        (assert-equal sentence (ulf2english ulf)
                      sentence (ulf2english ulf) ulf))))

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

(gen-blocks-qa-test bwqa-gk1
  (:gene-bwqa)
  "Is the Nvidia block to the left of the Texaco block?"
  '(((pres be.v) (the.d (| Nvidia| block.n))
     (to.p (the.d (left-of.n (the.d | Texaco| block.n))))) ?))
(gen-blocks-qa-test bwqa-gk2
  (:gene-bwqa)
  "Is the McDonalds block on top of the SRI block?"
  '(((pres be.v) (the.d (| McDonalds| block.n))
     (on.p ({the}.d (top-of.n (the.d (| SRI| block.n)))))) ?))
(gen-blocks-qa-test bwqa-gk3
  (:gene-bwqa)
  "Is the Starbucks block near the Toyota block?"
  '(((pres be.v) (the.d (| Starbucks| block.n))
     (near.p (the.d (| Toyota| block.n)))) ?))
(gen-blocks-qa-test bwqa-gk4
  (:gene-bwqa)
  "Is the Toyota block between the Nvidia block and the Target block?"
  '(((pres be.v) (the.d (| Toyota| block.n))
     (between.p ((the.d (| Nvidia| block.n)) and.cc
                 (the.d (| Target| block.n))))) ?))
(gen-blocks-qa-test bwqa-gk5
  (:gene-bwqa)
  "Is the SRI block fully on top of any red block?"
  '(((pres be.v) (the.d (| SRI| block.n))
     (fully.mod-a (on.p ({the}.d (top-of.n (any.d (red.a block.n))))))) ?))
(gen-blocks-qa-test bwqa-gk6
  (:gene-bwqa)
  "Does the Toyota block face the Nvidia block?"
  '(((pres do.aux-s) (the.d (| Toyota| block.n))
     (face.v (the.d (| Nvidia| block.n)))) ?))
(gen-blocks-qa-test bwqa-gk7
  (:gene-bwqa)
  "Does the tallest stack have a red block on top?"
  '(((pres do.aux-s) (the.d (most-n tall.a stack.n))
     (have.v (a.d (red.a block.n)) (on.p ({the}.d (top-of.n *ref))))) ?))
(gen-blocks-qa-test bwqa-gk8
  (:gene-bwqa)
  "Is the Toyota block a part of some stack?"
  '(((pres be.v) (the.d (| Toyota| block.n))
     (= (a.d (part-of.n (some.d stack.n))))) ?))
(gen-blocks-qa-test bwqa-gk9
  (:gene-bwqa)
  "Is the Target block a part of some row?"
  '(((pres be.v) (the.d (| Target| block.n))
     (= (a.d (part-of.n (some.d row.n))))) ?))
(gen-blocks-qa-test bwqa-gk10
  (:gene-bwqa)
  "Does any row contain a red block?"
  '(((pres do.aux-s) (any.d row.n)
     (contain.v (a.d (red.a block.n)))) ?))
(gen-blocks-qa-test bwqa-gk11
  (:gene-bwqa)
  "Are any two green blocks touching?"
  '(((pres prog) (any.d (two.a (green.a (plur block.n))))
     touch.v) ?))
(gen-blocks-qa-test bwqa-gk12
  (:gene-bwqa)
  "Are any two stacks near each other?"
  '(((pres be.v) (any.d (two.a (plur stack.n)))
     (near.p (each.d (other.n {ref}.n)))) ?)) ;; maybe we can just write each_other.pro?
(gen-blocks-qa-test bwqa-gk13
  (:gene-bwqa)
  "Does the SRI block have anything on it?"
  '(((pres do.aux-s) (the.d (| SRI| block.n))
     (have.v anything.pro (on.p it.pro))) ?))
    ;; TODO: update the formula below once we finalize how to deal with this "blocks"
(gen-blocks-qa-test bwqa-gk14
  (:gene-bwqa)
  "Are the Nvidia and the SRI blocks in the same stack?"
  '(((pres be.v) (the.d ((| Nvidia| and.cc | SRI|) (plur block.n)))
     (in.p (the.d (same.a stack.n)))) ?))
(gen-blocks-qa-test bwqa-gk15
  (:gene-bwqa)
  "Is the Toyota block below the Texaco block?"
  '(((pres be.v) (the.d (| Toyota| block.n))
     (below.p (the.d (| Texaco| block.n)))) ?))
(gen-blocks-qa-test bwqa-gk16
  (:gene-bwqa)
  "Is the Starbucks block on top of any row?"
  '(((pres be.v) (the.d (| Starbucks| block.n))
     (on.p ({the}.d (top-of.n (any.d row.n))))) ?))
    ;; TODO: the formula below doesn't seem right since two separate 'plur'
    ;; operators seems to preclude having one Nvidia and one McDonalds block
(gen-blocks-qa-test bwqa-gk17
  (:gene-bwqa)
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
(gen-blocks-qa-test bwqa-gk18
  (:gene-bwqa)
  "Are all the red blocks near the Nvidia block?"
  '(((pres be.v) (all.d ({of}.p (the.d (red.a (plur block.n)))))
     (near.p (the.d (| Nvidia| block.n)))) ?))
(gen-blocks-qa-test bwqa-gk19
  (:gene-bwqa)
  "Is every blue block behind some other block?"
  '(((pres be.v) (every.d (blue.a block.n))
     (behind.p (some.d (other.a block.n)))) ?))
(gen-blocks-qa-test bwqa-gk20
  (:gene-bwqa)
  "Is the Texaco block between any two blocks?"
  '(((pres be.v) (the.d (| Texaco| block.n))
     (between.p (any.d (two.a (plur block.n))))) ?))
(gen-blocks-qa-test bwqa-gk21
  (:gene-bwqa)
  "Is the Target block slightly to the left of some red block?"
  '(((pres be.v) (the.d (| Target| block.n))
     (slightly.mod-a
      (to.p (the.d (left-of.n (some.d (red.a block.n))))))) ?))
(gen-blocks-qa-test bwqa-gk22
    ;; TODO: update the formula below once we finalize how to deal with this "blocks"
  (:gene-bwqa)
  "Is there a block between the McDonalds and the SRI blocks?"
  '(((pres be.v) there.pro
     (a.d (n+preds block.n
                   (between.p ((the.d (| McDonalds| (plur {block}.n))) and.cc
                               (the.d (| SRI| (plur block.n)))))))) ?))
(gen-blocks-qa-test bwqa-gk23
  (:gene-bwqa)
  "Is there a block near the Target block?"
  '(((pres be.v) there.pro
     (a.d (n+preds block.n
                   (near.p (the.d (| Target| block.n)))))) ?))
(gen-blocks-qa-test bwqa-gk24
  (:gene-bwqa)
  "Is there a red block near a blue block?"
  '(((pres be.v) there.pro
     (a.d (n+preds (red.a block.n)
                   (near.p (a.d (blue.a block.n)))))) ?))
(gen-blocks-qa-test bwqa-gk25
  (:gene-bwqa)
  "Is there a block touching the Nvidia block?"
  '(((pres be.v) there.pro
     (a.d (n+preds block.n
                   (touch.v (the.d (| Nvidia| block.n)))))) ?))
(gen-blocks-qa-test bwqa-gk26
  (:gene-bwqa)
  "Is there anything at the front left corner of the table?"
  '(((pres be.v) there.pro
     (any.d (n+preds thing.n
                     (at.p (the.d (front.a (left.a
                             (corner-of.n (the.d table.n))))))))) ?))
(gen-blocks-qa-test bwqa-gk27
  (:gene-bwqa)
  "Is there a row consisting of three blocks?"
  '(((pres be.v) there.pro
     (a.d (n+preds row.n
                   (consist.v (of.p-arg (three.d (plur block.n))))))) ?))

;; By Gene, taken from the set initially assigned to Tianyi.
(gen-blocks-qa-test bwqa-gk28
  (:gene-bwqa)
  "Which block is on two other blocks?"
  '(((which.d block.n)
     ((pres be.v) (on.p (two.d (other.a (plur block.n)))))) ?))
(gen-blocks-qa-test bwqa-gk29
  (:gene-bwqa)
  "Which block is in the center of the table?"
  '(((which.d block.n)
     ((pres be.v) (in.p (the.d (center-of.n (the.d table.n)))))) ?))
(gen-blocks-qa-test bwqa-gk30
  (:gene-bwqa)
  "What blocks are in the longest row?"
  '(((what.d (plur block.n))
     ((pres be.v) (in.p (the.d (most-n long.a row.n))))) ?))
(gen-blocks-qa-test bwqa-gk31
  (:gene-bwqa)
  "What block is halfway on top of any other block?"
  '(((what.d block.n)
     ((pres be.v)
      ((mod-a halfway.a)
       (on.p ({the}.d (top-of.n (any.d (other.a block.n)))))))) ?))
(gen-blocks-qa-test bwqa-gk32
  (:gene-bwqa)
  "Which block is side by side with the Texaco block?"
  '(((which.d block.n)
     ((pres be.v)
      (side_by_side.a (with.p-arg (the.d (| Texaco| block.n)))))) ?))
(gen-blocks-qa-test bwqa-gk33
  (:gene-bwqa)
  "What is the farthest block from the center of the table?"
  '((what.pro ((pres be.v)
               (the.d (rep ((most (far.a *p)) block.n)
                           (mod-a (from.p (the.d (center-of.n (the.d table.n))))))))) ?))
(gen-blocks-qa-test bwqa-gk34
  (:gene-bwqa)
  "Which block the Nvidia block is on top of?"
  '((sub (which.d block.n)
         ((the.d (| Nvidia| block.n))
          ((pres be.v) (on.p ({the}.d (top-of.n *h)))))) ?))
(gen-blocks-qa-test bwqa-gk35
  (:gene-bwqa)
  "Which block is very close to the front edge of the table?"
  '(((which.d block.n)
     ((pres be.v)
      ((very.mod-a close.a)
       (mod-a (to.p (the.d (front.n (edge-of.n (the.d table.n))))))))) ?))
(gen-blocks-qa-test bwqa-gk36
  (:gene-bwqa)
  "What is in the middle of the table?"
  '((what.pro ((pres be.v)
               (in.p (the.d (middle-of.n (the.d table.n)))))) ?))
(gen-blocks-qa-test bwqa-gk37
  (:gene-bwqa)
  "Which red block is the closest to the Toyota block?"
  '(((which.d (red.a block.n))
     ((pres be.v)
      (the.d (n+preds ({red}.a {block}.n)
                      (most (close.a (mod-a (to.p (the.d (| Toyota| block.n)))))))))) ?))
(gen-blocks-qa-test bwqa-gk38
  (:gene-bwqa)
  "Which red blocks are directly on the table?"
  '(((which.d (red.a (plur block.n)))
     ((pres be.v) directly.adv-a (on.p (the.d table.n)))) ?))
(gen-blocks-qa-test bwqa-gk39
  (:gene-bwqa)
  "Which blue block the Nvidia block is not near to?"
  '((sub (which.d (blue.a block.n))
         ((the.d (| Nvidia| block.n))
          ((pres be.v) not (near.a (mod-a (to.p *h)))))) ?))
(gen-blocks-qa-test bwqa-gk40
  (:gene-bwqa)
  "Where is the Toyota block?"
  '((sub where.a
         ((pres be.v) (the.d (| Toyota| block.n)) *h)) ?))
(gen-blocks-qa-test bwqa-gk41
  (:gene-bwqa)
  "Where is some blue block?"
  '((sub where.a
         ((pres be.v) (some.d (blue.a block.n)) *h)) ?))
(gen-blocks-qa-test bwqa-gk42
  (:gene-bwqa)
  "Where is the leftmost red block?"
  '((sub where.a
         ((pres be.v) (the.d (most-n left.a (red.a block.n))) *h)) ?))
(gen-blocks-qa-test bwqa-gk43
  (:gene-bwqa)
  "Where is any clear block?"
  '((sub where.a
         ((pres be.v) (any.d (clear.a block.n)) *h)) ?))
(gen-blocks-qa-test bwqa-gk44
  (:gene-bwqa)
  "Where is the shortest stack?"
  '((sub where.a
         ((pres be.v) (the.d (most-n short.a stack.n)) *h)) ?))
(gen-blocks-qa-test bwqa-gk45
  (:gene-bwqa)
  "Where is the highest block?"
  '((sub where.a
         ((pres be.v) (the.d (most-n high.a block.n)) *h)) ?))

;;
;; Viet
;;

;; Is there a block facing the front right corner of the table?
;; Is there a block facing the front edge of the table?
;; Are there two blocks facing each other?
;; Is there a block in the middle of the table?
;; Is there a stack near the center of the table?
;; Are there two stacks that are side by side?
;; Is there a shortest stack on the table?
;; Is there a block on the left side of the table?
;; Is there a block that is side by side with the Nvidia block?
;; Is there a block that is side by side with any red block?
;; Is there a block that is below two blue blocks?
;; Is there a block on top of any stack?
;; Are there two blocks that are not near each other?
;; Is there a block close to any stack?
;; What color is the block to the left of the Nvidia block?
;; What color is the block that is near the leftmost red block?
;; What color are the blocks near to the Toyota block?
;; What color is the block close to the front edge of the table?
;; What color are the blocks near any edge of the table?
;; What color are the blocks in the rightmost stack?
;; What is the height of the leftmost stack?
;; What is the length of the longest row?
;; What direction is the Toyota block facing?
;; What color is the middle block in the longest row?
;; What colors are the blocks that are under some blue block?
;; What color is the lowest block of the shortest stack?
;; What color is the last block in the leftmost row?

(gen-blocks-qa-test bwqa-v1
  (:viet-bwqa)
  "Is there a block facing the front right corner of the table?"
  '(((pres be.v) there.pro
      (a.d (n+preds block.n
                  (face.v (the.d (front.a (right.a (corner-of.n (the.d table.n))))))))) ?))

(gen-blocks-qa-test bwqa-v2
  (:viet-bwqa)
  "Is there a block facing the front edge of the table?"
  '(((pres be.v) there.pro
      (a.d (n+preds block.n
                  (face.v (the.d (front.a (edge-of.n (the.d table.n)))))))) ?))

(gen-blocks-qa-test bwqa-v3
  (:viet-bwqa)
  "Are there two blocks facing each other?"
  '(((pres be.v) there.pro
     (two.d (n+preds (plur block.n)
                     (face.v (each.d (other.n {ref}.n)))))) ?))

(gen-blocks-qa-test bwqa-v4
  (:viet-bwqa)
  "Is there a block in the middle of the table?"
  '(((pres be.v) there.pro
      (a.d (n+preds block.n
                  (in.p (the.d (middle-of.n (the.d table.n))))))) ?))

(gen-blocks-qa-test bwqa-v5
  (:viet-bwqa)
  "Is there a stack near the center of the table?"
  '(((pres be.v) there.pro
      (a.d (n+preds stack.n
                  (near.p (the.d (center-of.n (the.d table.n))))))) ?))

(gen-blocks-qa-test bwqa-v6
  (:viet-bwqa)
  "Are there two stacks that are side by side?"
  '(((pres be.v) there.pro
     (two.d (n+preds (plur stack.n)
                     (that.rel ((pres be.v) side_by_side.a))))) ?))

(gen-blocks-qa-test bwqa-v7
  (:viet-bwqa)
  "Is there a shortest stack on the table?"
  '(((pres be.v) there.pro
      (a.d (n+preds (most-n short.a stack.n)
                  (on.p (the.d table.n))))) ?))

(gen-blocks-qa-test bwqa-v8
  (:viet-bwqa)
  "Is there a block on the left side of the table?"
  '(((pres be.v) there.pro
      (a.d (n+preds block.n
                  (on.p (the.d (left.a (side-of.n (the.d table.n)))))))) ?))

(gen-blocks-qa-test bwqa-v9
  (:viet-bwqa)
  "Is there a block on the left side of the table?"
  '(((pres be.v) there.pro
      (a.d (n+preds block.n
                  (on.p (the.d (left.a (side-of.n (the.d table.n)))))))) ?))

(gen-blocks-qa-test bwqa-v10
  (:viet-bwqa)
  "Is there a block that is side by side with the Nvidia block?"
  '(((pres be.v) there.pro
     (a.d (n+preds block.n
                  (that.rel ((pres be.v) (side_by_side.a
                                          (with.p-arg (the.d (| Nvidia| block.n))))))))) ?))

(gen-blocks-qa-test bwqa-v11
  (:viet-bwqa)
  "Is there a block that is side by side with any red block?"
  '(((pres be.v) there.pro
     (a.d (n+preds block.n
                  (that.rel ((pres be.v) (side_by_side.a
                                          (with.p-arg (any.d (red.a block.n))))))))) ?))

(gen-blocks-qa-test bwqa-v12
  (:viet-bwqa)
  "Is there a block that is below two blue blocks?"
  '(((pres be.v) there.pro
      (a.d (n+preds block.n
                  (that.rel ((pres be.v) (below.p (two.d (blue.a (plur block.n))))))))) ?))

(gen-blocks-qa-test bwqa-v13
  (:viet-bwqa)
  "Is there a block on top of any stack?"
  '(((pres be.v) there.pro
      (a.d (n+preds block.n
                  (on.p ({the}.d (top-of.n (any.d stack.n))))))) ?))

(gen-blocks-qa-test bwqa-v14
  (:viet-bwqa)
  "Are there two blocks that are not near each other?"
  '(((pres be.v) there.pro
     (two.d (n+preds (plur block.n)
                     (that.rel ((pres be.v) not.adv-s (near.p (each.d (other.n {ref}.n)))))))) ?))

(gen-blocks-qa-test bwqa-v15
  (:viet-bwqa)
  "Is there a block close to any stack?"
  '(((pres be.v) there.pro
      (a.d (n+preds block.n
                  (close.a (to.p-arg (any.d stack.n)))))) ?))

(gen-blocks-qa-test bwqa-v16
  (:viet-bwqa)
  "What color is the block to the left of the Nvidia block?"
  '(sub (what.mod-n color.n)
         (((pres be.v)
           (the.d (n+preds block.n
                           (to.p (the.d (left-of.n (the.d (| Nvidia| block.n)))))))
           *h) ?)))

(gen-blocks-qa-test bwqa-v17
  (:viet-bwqa)
  "What color is the block that is near the leftmost red block?"
  '(sub (what.mod-n color.n)
        (((pres be.v)
          (the.d (n+preds block.n
                          (that.rel ((pres be.v)
                                     (near.p (the.d (most-n left.a (red.a block.n))))))))
          *h) ?)))

(gen-blocks-qa-test bwqa-v18
  (:viet-bwqa)
  "What color are the blocks near to the Toyota block?"
  '(sub (what.mod-n color.n)
        (((pres be.v)
          (the.d (n+preds (plur block.n)
                          (near.a (to.p-arg (the.d (| Toyota| block.n))))))
          *h) ?)))

(gen-blocks-qa-test bwqa-v19
  (:viet-bwqa)
  "What color is the block close to the front edge of the table?"
  '(sub (what.mod-n color.n)
        (((pres be.v)
          (the.d (n+preds block.n
                          (close.a (to.p-arg (the.d (front.a (edge-of.n (the.d table.n))))))))
          *h) ?)))

(gen-blocks-qa-test bwqa-v20
  (:viet-bwqa)
  "What color are the blocks near any edge of the table?"
  '(sub (what.mod-n color.n)
        (((pres be.v)
          (the.d (n+preds (plur block.n)
                          (near.p (any.d (edge-of.n (the.d table.n))))))
          *h) ?)))

(gen-blocks-qa-test bwqa-v21
  (:viet-bwqa)
  "What is the height of the leftmost stack?"
  '((what.pro ((pres be.v) (= (the.d (height-of.n (the.d (most-n left.a stack.n))))))) ?))

(gen-blocks-qa-test bwqa-v22
  (:viet-bwqa)
  "What is the length of the longest row?"
  '((what.pro ((pres be.v) (= (the.d (length-of.n (the.d (most-n long.a row.n))))))) ?))

(gen-blocks-qa-test bwqa-v23
  (:viet-bwqa)
  "What direction is the Toyota block facing?"
  '((sub (what.d direction.n)
         ((pres prog) (the.d (| Toyota| block.n)) (face.v *h))) ?))

(gen-blocks-qa-test bwqa-v24
  (:viet-bwqa)
  "What color is the middle block in the longest row?"
  '(sub (what.mod-n color.n)
        (((pres be.v)
          (the.d (n+preds (middle.a block.n)
                          (in.p (the.d (most-n long.a row.n)))))
          *h) ?)))

(gen-blocks-qa-test bwqa-v25
  (:viet-bwqa)
  "What colors are the blocks that are under some blue block?"
  '(sub (what.mod-n (plur color.n))
        (((pres be.v)
          (the.d (n+preds (plur block.n)
                          (that.rel ((pres be.v)
                                     (under.p (some.d (blue.n block.n)))))))
          *h) ?)))

(gen-blocks-qa-test bwqa-v26
  (:viet-bwqa)
  "What color is the lowest block of the shortest stack?"
  '(sub (what.mod-n color.n)
        (((pres be.v)
          (the.d (n+preds (most-n low.a block.n)
                          (of.p (the.d (most-n short.a stack.n)))))
          *h) ?)))

(gen-blocks-qa-test bwqa-v27
  (:viet-bwqa)
  "What color is the last block in the leftmost row?"
  '(sub (what.mod-n color.n)
        (((pres be.v)
          (the.d (n+preds (last.a block.n)
                          (in.p (the.d (most-n left.a row.n)))))
          *h) ?)))


;;
;; Georgiy
;;

;; What color is the block at the left end of the longest row?
;; How many blocks are above the block that is closest to the Nvidia block?
;; How many red blocks are in some corner of the table?
;; How many blocks are behind any red block?
;; How many blocks are on top of any blue block?
;; How many blocks are between any other blocks?

;; 61 - 91
;; How many blocks are facing any corner of the table?
;; How many stacks are on the table?
;; How many rows are on the table?
;; How many blocks are on the left side of the table?
;; How many blocks are in the tallest stack?
;; How many blocks are in the longest row?
;; How many block are between any two stacks?
;; How many blocks are clear?
;; How many blocks are in some row or stack?
;; How many blocks are not touching the table?
;; What are the blocks on top of the Toyota block?
;; What are the blocks that are not on top of any other block?
;; What is the block closest to the rightmost red block?
;; What is the block between the Toyota and the SRI block?
;; What are the blocks near the corners of the table?
;; What is the block closest to the front edge of the table?
;; What is the block on top of the tallest stack?
;; What is the the blocks behind the Nvidia block?
;; What is the block that is at the rightmost red block?
;; Which block is the highest?
;; Which block is on two other blocks?
;; Which block is in the center of the table?
;; What blocks are in the longest row?
;; What block is halfway on top of any other block?
;; Which block is side by side with the Texaco block?
;; What is the farthest block from the center of the table?
;; Which block the Nvidia block is on top of?
;; Which block is very close to the front edge of the table?
;; What is in the middle of the table?
;; Which red block is the closest to the Toyota block?
;; Which red blocks are directly on the table?

(gen-blocks-qa-test bwqa-gp61
		    (:georgiy-bwqa)
		    "How many blocks are facing any corner of the table?"
		    '(((k ((How.mod-a many.a) (plur block.n)))
		       ((pres prog) (face.v (any.d (n+preds corner.n (of.p (the.d table.n))))))) ?))

(gen-blocks-qa-test bwqa-gp62
		    (:georgiy-bwqa)
		    "How many stacks are on the table?"
		    '(((k ((How.mod-a many.a) (plur stack.n)))
		       ((pres be.v) (on.p (the.d table.n)))) ?))

(gen-blocks-qa-test bwqa-gp63
		    (:georgiy-bwqa)
		    "How many rows are on the table?"
		    '(((k ((How.mod-a many.a) (plur row.n)))
		       ((pres be.v) (on.p (the.d table.n)))) ?))

(gen-blocks-qa-test bwqa-gp64
		    (:georgiy-bwqa)
		    "How many blocks are on the left side of the table?"
		    '(((k ((How.mod-a many.a) (plur block.n)))
		       ((pres be.v) (on.p (the.d (n+preds (left.a side.n) (of.p (the.d table.n))))))) ?))

(gen-blocks-qa-test bwqa-gp65
		    (:georgiy-bwqa)
		    "How many blocks are in the tallest stack?"
		    '(((k ((How.mod-a many.a) (plur block.n)))
		       ((pres be.v) (in.p (the.d (most-n tall.a stack.n))))) ?))

(gen-blocks-qa-test bwqa-gp66
		    (:georgiy-bwqa)
		    "How many blocks are in the longest row?"
		    '(((k ((How.mod-a many.a) (plur block.n)))
		       ((pres be.v) (in.p (the.d (most-n long.a row.n))))) ?))

(gen-blocks-qa-test bwqa-gp67
		    (:georgiy-bwqa)
		    "How many blocks are between any two stacks?"
		    '(((k ((How.mod-a many.a) (plur block.n)))
		       ((pres be.v) (between.p (any.d (two.a (plur stack.n)))))) ?))

(gen-blocks-qa-test bwqa-gp68
		    (:georgiy-bwqa)
		    "How many blocks are clear?"
		    '(((k ((How.mod-a many.a) (plur block.n)))
		       ((pres be.v) clear.a)) ?))

(gen-blocks-qa-test bwqa-gp69
		    (:georgiy-bwqa)
		    "How many blocks are in some row or stack?"
		    '(((k ((How.mod-a many.a) (plur block.n)))
		       ((pres be.v) (in.p (some.d (row.n or.cc stack.n))))) ?))

(gen-blocks-qa-test bwqa-gp70
		    (:georgiy-bwqa)
		    "How many blocks are not touching the table?"
		    '(((k ((How.mod-a many.a) (plur block.n)))
		       ((pres prog) not.adv-s (touch.v (the.d table.n)))) ?))

(gen-blocks-qa-test bwqa-gp71
		    (:georgiy-bwqa)
		    "What are the blocks on top of the Toyota block?"
		    '((What.pro
		       ((pres be.v) (= (the.d (n+preds (plur block.n) (on.p (k (n+preds top.n (of.p (the.d (| Toyota|.n block.n))))))))))) ?))

(gen-blocks-qa-test bwqa-gp72
		    (:georgiy-bwqa)
		    "What are the blocks that are not on top of any other block?"
		    '((What.pro
		       ((pres be.v) (= (the.d (n+preds (plur block.n) (that.rel ((pres be.v) not.adv-s (on.p (k (n+preds top.n (of.p (any.d (other.a block.n))))))))))))) ?))

;;(gen-blocks-qa-test bwqa-gp73
;;		    (:georgiy-bwqa)
;;"What is the block closest to the rightmost red block?"
;;((What.pro ((pres be.v) (= (the.d (n+preds block.n ({that}.rel ((pres be.v) (most-n close.a (n+preds {block}.n (to.p (the.d (most-n right.a (red.a block.n))))))))))))) ?)

(gen-blocks-qa-test bwqa-gp74
		    (:georgiy-bwqa)
		    "What is the block between the Toyota and the SRI block?"
		    '((What.pro
		       ((pres be.v) (= (the.d (n+preds block.n (between.p ((the.d (| Toyota|.n {block}.n)) and.cc (the.d (| SRI|.n block.n))))))))) ?))

(gen-blocks-qa-test bwqa-gp75
		    (:georgiy-bwqa)
		    "What are the blocks near the corners of the table?"
		    '((What.pro
		       ((pres be.v) (= (the.d (plur (n+preds block.n (near.p (the.d (plur (n+preds corner.n (of.p (the.d table.n)))))))))))) ?))

;;(gen-blocks-qa-test bwqa-gp76
;;(:georgiy-bwqa)
;;"What is the block closest to the front edge of the table?"
;;'((What.pro ((pres be.v) (= (the.d (n+preds block.n (closest.a (to.p (the.d (n+preds (plur corner.n) (of.p (the.d table.n))))))))))) ?)

(gen-blocks-qa-test bwqa-gp77
		    (:georgiy-bwqa)
		    "What is the block on top of the tallest stack?"
		    '((What.pro
		       ((pres be.v) (= (the.d (n+preds block.n (on.p (k (n+preds top.n (of.p (the.d (most-n tall.a stack.n))))))))))) ?))

(gen-blocks-qa-test bwqa-gp78
		    (:georgiy-bwqa)
		    "What are the blocks behind the Nvidia block?"
		    '((What.pro
		       ((pres be.v) (= (the.d (n+preds (plur block.n) (behind.p (the.d (| Nvidia|.n block.n)))))))) ?))

(gen-blocks-qa-test bwqa-gp79
		    (:georgiy-bwqa)
		    "What is the block that is at the rightmost red block?"
		    '((What.pro
		       ((pres be.v) (= (the.d (n+preds block.n (that.rel ((pres be.v) (at.p (the.d (most-n right.a (red.a block.n))))))))))) ?))

(gen-blocks-qa-test bwqa-gp80
		    (:georgiy-bwqa)
		    "Which block is the highest?"
		    '(((Which.d block.n)
		       ((pres be.v) (= (the.d (most-n high.a {block}.n))))) ?))

(gen-blocks-qa-test bwqa-gp81
		    (:georgiy-bwqa)
		    "Which block is on two other blocks?"
		    '(((Which.d block.n)
		       ((pres be.v) (on.p (two.d (other.a (plur block.n)))))) ?))

(gen-blocks-qa-test bwqa-gp82
		    (:georgiy-bwqa)
		    "Which block is in the center of the table?"
		    '(((Which.d block.n)
		       ((pres be.v) (in.p (the.d (n+preds center.n (of.p (the.d table.n))))))) ?))

(gen-blocks-qa-test bwqa-gp83
		    (:georgiy-bwqa)
		    "What blocks are in the longest row?"
		    '(((What.d (plur block.n))
		       ((pres be.v) (in.p (the.d (most-n long.a row.n))))) ?))

(gen-blocks-qa-test bwqa-gp84
		    (:georgiy-bwqa)
		    "What block is halfway on top of any other block?"
		    '(((What.d block.n)
		       ((pres be.v) (halfway.mod-a (on.p (k (n+preds top.n (of.p (any.d (other.a block.n))))))))) ?))

(gen-blocks-qa-test bwqa-gp85
		    (:georgiy-bwqa)
		    "Which block is side by side with the Texaco block?"
		    '(((Which.d block.n)
		       ((pres be.v) (side_by_side.a (with.p-arg (the.d (| Texaco|.n block.n)))))) ?))

;;(gen-blocks-qa-test bwqa-gp86
;;  (:georgiy-bwqa)
;;"What is the farthest block from the center of the table?"
;;'((What.pro ((pres be.v) (the.d (most-pc far.a block.n (from.p (the.d (n+preds center.n (of.p (the.d table.n))))))))) ?)

(gen-blocks-qa-test bwqa-gp87
		    (:georgiy-bwqa)
		    "Which block the Nvidia block is on top of?"
		    '((sub (Which.d block.n) ((the.d (| Nvidia|.n block.n))
					      ((pres be.v) (on.p (k (n+preds top.n (of.p *h))))))) ?))

(gen-blocks-qa-test bwqa-gp88
		    (:georgiy-bwqa)
		    "Which block is very close to the front edge of the table?"
		    '(((Which.d block.n)
		       ((pres be.v) ((mod-a (very.mod-a close.a)) (to.p (the.d (front.a (n+preds edge.n (of.p (the.d table.n))))))))) ?))

(gen-blocks-qa-test bwqa-gp89
		    (:georgiy-bwqa)
		    "What is in the middle of the table?"
		    '((What.pro
		       ((pres be.v) (in.p (the.d (n+preds middle.n (of.p (the.d table.n))))))) ?))

;;(gen-blocks-qa-test bwqa-gp90
;;		    (:georgiy-bwqa)
;;"Which red block is the closest to the Toyota block?"
;;'(((Which.d (red.a block.n)) ((pres be.v) (the.d (most-pc close.a (to.p (the.d (| Toyota|.n block.n))))))) ?)

(gen-blocks-qa-test bwqa-gp91
		    (:georgiy-bwqa)
		    "Which red blocks are directly on the table?"
		    '(((Which.d (red.a (plur block.n)))
		       ((pres be.v) (directly.mod-a (on.p (the.d table.n))))) ?))

