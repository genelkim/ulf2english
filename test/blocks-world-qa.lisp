;; Unit tests for ulf2english on the blocks world QA question set.
;; Uses lisp-unit.

(in-package :ulf2english)

(setq *print-failures* t)
(setq *print-summary* t)

;;
;; Gene
;;

;; 1. Is the Nvidia block to the left of the Texaco block?
(define-test bwqa-1
  "Blocks world QA sentence 1."
  (:tag :blocks-world-qa)
  (let ((ulf '(((pres be.v) (the.d (|Nvidia| block.n)) 
                (to.p (the.d ((adv-a left.a) (of.p (the.d |Texaco| block.n)))))) ?))
        (reduced-ulf 
             '(((pres be.v) (the.d (|Nvidia| block.n))
                (to_the_left_of.p (the.d (|Texaco| block.n)))) ?))
        (expected "Is the Nvidia block to the left of the Texaco block?"))
    ;; assert-equal takes the expected value, the actual value, then values to print out on failure.
    (assert-equal expected (ulf2english ulf) 
                  expected (ulf2english ulf) ulf)
    ;; Same test for reduced ULF.
    (assert-equal expected (ulf2english reduced-ulf) 
                  expected (ulf2english reduced-ulf) reduced-ulf)))

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
;; Which blue block the Nvidia block is not near to?
;; Where is the Toyota block?
;; Where is some blue block?
;; Where is the leftmost red block? 
;; Where is any clear block?
;; Where is the shortest stack?
;; Where is the highest block?
