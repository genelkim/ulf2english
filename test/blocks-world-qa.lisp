;; Unit tests for ulf2english on the blocks world QA question set.
;; Uses lisp-unit.

(in-package :ulf2english)

(setq *print-failures* t)
(setq *print-summary* t)

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

(gen-blocks-qa-test bwqa-g1
  (:gene-bwqa)
  "Is the Nvidia block to the left of the Texaco block?"
  '(((pres be.v) (the.d (|Nvidia| block.n)) 
     (to.p (the.d (left-of.n (the.d |Texaco| block.n))))) ?))
(gen-blocks-qa-test bwqa-g2
  (:gene-bwqa)
  "Is the McDonalds block on top of the SRI block?"
  '(((pres be.v) (the.d (|McDonalds| block.n))
     (on.p ({the}.d (top-of.n (the.d (|SRI| block.n)))))) ?))
(gen-blocks-qa-test bwqa-g3
  (:gene-bwqa)
  "Is the Starbucks block near the Toyota block?"
  '(((pres be.v) (the.d (|Starbucks| block.n))
     (near.p (the.d (|Toyota| block.n)))) ?))
(gen-blocks-qa-test bwqa-g4
  (:gene-bwqa)
  "Is the Toyota block between the Nvidia block and the Target block?"
  '(((pres be.v) (the.d (|Toyota| block.n))
     (between.p ((the.d (|Nvidia| block.n)) and.cc 
                 (the.d (|Target| block.n))))) ?))
(gen-blocks-qa-test bwqa-g5
  (:gene-bwqa)
  "Is the SRI block fully on top of any red block?"
  '(((pres be.v) (the.d (|SRI| block.n))
     (fully.mod-a (on.p ({the}.d (top-of.n (any.d (red.a block.n))))))) ?))
(gen-blocks-qa-test bwqa-g6
  (:gene-bwqa)
  "Does the Toyota block face the Nvidia block?"
  '(((pres do.aux-s) (the.d (|Toyota| block.n))
     (face.v (the.d (|Nvidia| block.n)))) ?))
(gen-blocks-qa-test bwqa-g7
  (:gene-bwqa)
  "Does the tallest stack have a red block on top?"
  '(((pres do.aux-s) (the.d ((most tall.a) stack.n))
     (have.v (a.d (red.a block.n)) (on.p ({the}.d (top-of.n *ref}))))) ?))
(gen-blocks-qa-test bwqa-g8
  (:gene-bwqa)
  "Is the Toyota block a part of some stack?"
  '(((pres be.v) (the.d (|Toyota| block.n))
     (= (a.d (part-of.n (some.d stack.n))))) ?))
(gen-blocks-qa-test bwqa-g9
  (:gene-bwqa)
  "Is the Target block a part of some row?"
  '(((pres be.v) (the.d (|Target| block.n))
     (= (a.d (part-of.n (some.d row.n))))) ?))
(gen-blocks-qa-test bwqa-g10
  (:gene-bwqa)
  "Does any row contain a red block?"
  '(((pres do.aux-s) (any.d row.n)
     (contain.v (a.d (red.a block.n)))) ?))
(gen-blocks-qa-test bwqa-g11
  (:gene-bwqa)
  "Are any two green blocks touching?"
  '(((pres prog) (any.d (two.a (green.a (plur block.n)))) 
     touch.v) ?))
(gen-blocks-qa-test bwqa-g12
  (:gene-bwqa)
  "Are any two stacks near each other?"
  '(((pres be.v) (any.d (two.a (plur stack.n)))
     (near.p (each.d (other.n {ref}.n)))) ?)) ;; maybe we can just write each_other.pro?
(gen-blocks-qa-test bwqa-g13
  (:gene-bwqa)
  "Does the SRI block have anything on it?"
  '(((pres do.aux-s) (the.d (|SRI| block.n))
     (have.v anything.pro (on.p it.pro))) ?))
    ;; TODO: update the formula below once we finalize how to deal with this "blocks" 
(gen-blocks-qa-test bwqa-g14
  (:gene-bwqa)
  "Are the Nvidia and the SRI blocks in the same stack?"
  '(((pres be.v) (the.d ((|Nvidia| and.cc |SRI|) (plur block.n)))
     (in.p (the.d (same.a stack.n)))) ?))
(gen-blocks-qa-test bwqa-g15
  (:gene-bwqa)
  "Is the Toyota block below the Texaco block?"
  '(((pres be.v) (the.d (|Toyota| block.n))
     (below.p (the.d (|Texaco| block.n)))) ?))
(gen-blocks-qa-test bwqa-g16
  (:gene-bwqa)
  "Is the Starbucks block on top of any row?"
  '(((pres be.v) (the.d (|Starbucks| block.n))
     (on.p ({the}.d (top-of.n (any.d row.n))))) ?))
    ;; TODO: the formula below doesn't seem right since two separate 'plur'
    ;; operators seems to preclude having one Nvidia and one McDonalds block
(gen-blocks-qa-test bwqa-g17
  (:gene-bwqa)
  "Are the Nvidia and the McDonalds blocks side by side?" 
  '(((pres be.v) ((the.d (|Nvidia| (plur {block}.n))) and.cc
                  (the.d (|McDonalds| (plur block.n))))
     side_by_side.a) ?)) 
    ;; side_by_side.a 
    ;; == (lambda x (all y [y (member-of.n x)] 
    ;;            (exists z [[z (member-of.n x)] ^ (not [z = y])]
    ;;              (exists a [a (side-of.n y)]
    ;;                (exists b [b (side-of.n z)]
    ;;                [a by.p b])))))
    ;; Or something like this... Too complicated to break down in ULF.
(gen-blocks-qa-test bwqa-g18
  (:gene-bwqa)
  "Are all the red blocks near the Nvidia block?"
  '(((pres be.v) (all.d ({of}.p (the.d (red.a (plur block.n)))))
     (near.p (the.d (|Nvidia| block.n)))) ?))
(gen-blocks-qa-test bwqa-g19
  (:gene-bwqa)
  "Is every blue block behind some other block?"
  '(((pres be.v) (every.d (blue.a block.n))
     (behind.p (some.d (other.a block.n)))) ?))
(gen-blocks-qa-test bwqa-g20
  (:gene-bwqa)
  "Is the Texaco block between any two blocks?"
  '(((pres be.v) (the.d (|Texaco| block.n))
     (between.p (any.d (two.a (plur block.n))))) ?))
(gen-blocks-qa-test bwqa-g21
  (:gene-bwqa)
  "Is the Target block slightly to the left of some red block?"
  '(((pres be.v) (the.d (|Target| block.n))
     (slightly.mod-a 
      (to.p (the.d (left-of.n (some.d (red.a block.n))))))) ?))
(gen-blocks-qa-test bwqa-g22
    ;; TODO: update the formula below once we finalize how to deal with this "blocks" 
  (:gene-bwqa)
  "Is there a block between the McDonalds and the SRI blocks?"
  '(((pres be.v) there.pro 
     (a.d (n+preds block.n
                   (between.p ((the.d (|McDonalds| (plur {block}.n))) and.cc
                               (the.d (|SRI| (plur block.n)))))))) ?))
(gen-blocks-qa-test bwqa-g23
  (:gene-bwqa)
  "Is there a block near the Target block?"
  '(((pres be.v) there.pro 
     (a.d (n+preds block.n
                   (near.p (the.d (|Target| block.n)))))) ?))
(gen-blocks-qa-test bwqa-g24
  (:gene-bwqa)
  "Is there a red block near a blue block?"
  '(((pres be.v) there.pro
     (a.d (n+preds (red.a block.n)
                   (near.p (a.d (blue.a block.n)))))) ?))
(gen-blocks-qa-test bwqa-g25
  (:gene-bwqa)
  "Is there a block touching the Nvidia block?"
  '(((pres be.v) there.pro
     (a.d (n+preds block.n
                   (touch.v (the.d (|Nvidia| block.n)))))) ?))
(gen-blocks-qa-test bwqa-g26
  (:gene-bwqa)
  "Is there anything at the front left corner of the table?"
  '(((pres be.v) there.pro
     (any.d (n+preds thing.n
                     (at.p (the.d (front.a (left.a 
                             (corner-of.n (the.d table.n))))))))) ?))
(gen-blocks-qa-test bwqa-g27
  (:gene-bwqa)
  "Is there a row consisting of three blocks?"
  '(((pres be.v) there.pro
     (a.d (n+preds row.n
                   (consist.v (of.p-arg (three.d (plur block.n))))))) ?))

;; By Gene, taken from the set initially assigned to Tianyi.
(gen-blocks-qa-test bwqa-g28
  (:gene-bwqa)
  "Which block is on two other blocks?"
  '(((which.d block.n) 
     ((pres be.v) (on.p (two.d (other.a (plur block.n)))))) ?))
(gen-blocks-qa-test bwqa-29
  (:gene-bwqa)
  "Which block is in the center of the table?"
  '(((which.d block.n)
     ((pres be.v) (on.p (two.d (other.a (plur block.n)))))) ?))
(gen-blocks-qa-test bwqa-30
  (:gene-bwqa)
  "What blocks are in the longest row?"
  '(((what.d (plur block.n)) 
     ((pres be.v) (in.p (the.d ((most long.a) row.n))))) ?))
(gen-blocks-qa-test bwqa-31
  (:gene-bwqa)
  "What block is halfway on top of any other block?"
  '(((what.d block.n)
     ((pres be.v) 
      ((mod-a halfway.a) 
       (on.p ({the}.d (top-of.n (any.d (other.a block.n)))))))) ?))
(gen-blocks-qa-test bwqa-32
  (:gene-bwqa)
  "Which block is side by side with the Texaco block?"
  '(((which.d block.n)
     ((pres be.v) 
      (side_by_side.a (with.p-arg (the.d (|Texaco| block.n)))))) ?))
(gen-blocks-qa-test bwqa-33
  (:gene-bwqa)
  "What is the farthest block from the center of the table?"
  '((what.pro ((pres be.v) 
               (the.d (rep (((most far.a) *p) block.n) 
                           (from.p (the.d (center-of.n (the.d table.n)))))))) ?))
(gen-blocks-qa-test bwqa-34
  (:gene-bwqa)
  "Which block the Nvidia block is on top of?"
  '((sub (which.d block.n) 
         ((the.d (|Nvidia| block.n)) 
          ((pres be.v) (on.p ({the}.d (top-of.n *h)))))) ?))
(gen-blocks-qa-test bwqa-35
  (:gene-bwqa)
  "Which block is very close to the front edge of the table?"
  '(((which.d block.n)
     ((pres be.v) 
      ((very.mod-a close.a) 
       (mod-a (to.p (the.d (front.n (edge-of.n (the.d table.n))))))))) ?))
(gen-blocks-qa-test bwqa-36
  (:gene-bwqa)
  "What is in the middle of the table?"
  '((what.pro ((pres be.v)
               (in.p (the.d (middle-of.n (the.d table.n)))))) ?))
(gen-blocks-qa-test bwqa-37
  (:gene-bwqa)
  "Which red block is the closest to the Toyota block?"
  '(((which.d (red.a block.n))
     ((pres be.v) 
      (the.d (n+preds ({red}.a {block}.n) 
                      ((most close.a) (mod-a (to.p (the.d (|Toyota| block.n))))))))) ?))
(gen-blocks-qa-test bwqa-38
  (:gene-bwqa)
  "Which red blocks are directly on the table?"
  '(((which.d (red.a (plur block.n)))
     ((pres be.v) directly.mod-a (on.p (the.d table.n)))) ?))
(gen-blocks-qa-test bwqa-39
  (:gene-bwqa)
  "Which blue block the Nvidia block is not near to?"
  '((sub (which.d (blue.a block.n))
         ((the.d (|Nvidia| block.n)) 
          ((pres be.v) not (near.a (mod-a (to.p *h)))))) ?))
(gen-blocks-qa-test bwqa-40
  (:gene-bwqa)
  "Where is the Toyota block?"
  '((sub where.a
         ((pres be.v) (the.d (|Toyota| block.n)) *h)) ?)) 
(gen-blocks-qa-test bwqa-41
  (:gene-bwqa)
  "Where is some blue block?"
  '((sub where.a
         ((pres be.v) (some.d (blue.a block.n)) *h)) ?))
(gen-blocks-qa-test bwqa-42
  (:gene-bwqa)
  "Where is the leftmost red block?"
  '((sub where.a
         ((pres be.v) (the.d ((most left.a) (red.a block.n))) *h)) ?))
(gen-blocks-qa-test bwqa-43
  (:gene-bwqa)
  "Where is any clear block?"
  '((sub where.a
         ((pres be.v) (any.d (clear.a block.n)) *h)) ?))
(gen-blocks-qa-test bwqa-44
  (:gene-bwqa)
  "Where is the shortest stack?"
  '((sub where.a
         ((pres be.v) (the.d ((most short.a) stack.n)) *h)) ?))
(gen-blocks-qa-test bwqa-45
  (:gene-bwqa)
  "Where is the highest block?"
  '((sub where.a
         ((pres be.v) (the.d ((most high.a) block.n)) *h)) ?))

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


;;
;; Georgiy
;;

;; What color is the block at the left end of the longest row?
;; How many blocks are above the block that is closest to the Nvidia block?
;; How many red blocks are in some corner of the table?
;; How many blocks are behind any red block?
;; How many blocks are on top of any blue block?
;; How many blocks are between any other blocks?
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

;;
;; Tianyi
;;

