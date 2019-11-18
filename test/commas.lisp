;;; Gene Louis Kim, 11-15-2019
;;; Tests for comma insertions.

(in-package :ulf2english)

(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(setq *summarize-results* t)

(define-test sent-coord
  "Comma insertions for sentential coordinators"
  (:tag :commas :sent-coord)
  (let (ulf expected predicted)
    ;; I left, and the tower fell
    (setf ulf '((i.pro (past leave.v)) and.cc ((the.d tower.n) (past fall.v))))
    (setf expected "I left, and the tower fell.")
    (setf predicted (ulf2english ulf :add-commas t))
    (assert-equal expected predicted
                  expected predicted ulf)
    ;; I left, he turned around, and the tower fell
    (setf ulf '((i.pro (past leave.v)) (he.pro ((past turn.v) around.adv-a)) and.cc ((the.d tower.n) (past fall.v))))
    (setf expected "I left, he turned around, and the tower fell.")
    (setf predicted (ulf2english ulf :add-commas t))
    (assert-equal expected predicted
                  expected predicted ulf)))

; TODO(gene): think of actual examples.
;(define-test noun-coord
;  "Comma insertions for nominal coordinators"
;  (:tag :commas :noun-coord)
;  (let (ulf expected predicted)
;    ;; The 
;    (setf ulf ((i.pro (past leave.v)) and.cc ((the.d tower.n) (past fall.v))))
;    (setf expected "I left, and the tower fell")
;    (setf predicted (ulf2english ulf :add-commas t))
;    (assert-equal expected predicted
;                  expected predicted ulf)
;    ;; I 
;    (setf ulf ((i.pro (past leave.v)) (he.pro ((past turn.v) around.adv-a)) and.cc ((the.d tower.n) (past fall.v))))
;    (setf expected "I left, he turned around, and the tower fell")
;    (setf predicted (ulf2english ulf :add-commas t))
;    (assert-equal expected predicted
;                  expected predicted ulf)))
    
(define-test verb-coord
  "Comma insertions for verbal coordinators"
  (:tag :commas :verb-coord)
  (let (ulf expected predicted)
    ;; I turned around and left
    (setf ulf '(i.pro (((past turn.v) around.adv-a) and.cc (past leave.v))))
    (setf expected "I turned around and left.")
    (setf predicted (ulf2english ulf :add-commas t))
    (assert-equal expected predicted
                  expected predicted ulf)
    ;; I waved, turned around, and left
    (setf ulf '(i.pro ((past wave.v) ((past turn.v) around.adv-a) and.cc (past leave.v))))
    (setf expected "I waved, turned around, and left.")
    (setf predicted (ulf2english ulf :add-commas t))
    (assert-equal expected predicted
                  expected predicted ulf)))
    
(define-test scoped-ps
  "Comma insertions for *.ps where arguments are individually scoped"
  (:tag :commas :scoped-ps)
  (let (ulf expected predicted)
    ;; Mangos are delicious, when they are ripe
    (setf ulf '((k (plur mango.n)) ((pres be.v) delicious.a
                                                (when.ps (they.pro ((pres be.v) ripe.a))))))
    (setf expected "Mangos are delicious, when they are ripe.")
    (setf predicted (ulf2english ulf :add-commas t))
    (assert-equal expected predicted
                  expected predicted ulf)
    ;; When they are ripe, mangos are delicious
    (setf ulf '((when.ps (they.pro ((pres be.v) ripe.a)))
                ((k (plur mango.n)) ((pres be.v) delicious.a))))
    (setf expected "When they are ripe, mangos are delicious.")
    (setf predicted (ulf2english ulf :add-commas t))
    (assert-equal expected predicted
                  expected predicted ulf)
    ;; Legs ache, when they are strained
    (setf ulf '(((k (plur Leg.n)) (pres ache.v)) (when.ps (they.pro (pres (pasv strain.v))))))
    (setf expected "Legs ache, when they are strained.")
    (setf predicted (ulf2english ulf :add-commas t))
    (assert-equal expected predicted
                  expected predicted ulf)))

(define-test flat-ps
  "Comma insertions for *.ps where arguments are flatly scoped"
  (:tag :commas :flat-ps)
  (let (ulf expected predicted)
    ;; When they are ripe, mangos are delicious
    (setf ulf '(when.ps (they.pro ((pres be.v) ripe.a))
                        ((k (plur mango.n)) ((pres be.v) delicious.a))))
    (setf expected "When they are ripe, mangos are delicious.")
    (setf predicted (ulf2english ulf :add-commas t))
    (assert-equal expected predicted
                  expected predicted ulf)
    ;; When they are strained, legs ache
    (setf ulf '(when.ps (they.pro (pres (pasv strain.v)))
                        ((k (plur leg.n)) (pres ache.v))))
    (setf expected "When they are strained, legs ache.")
    (setf predicted (ulf2english ulf :add-commas t))
    (assert-equal expected predicted
                  expected predicted ulf)))

