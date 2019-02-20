;; ULF to English
;; Packaged on 2018-11-24

(in-package :cl-user)

(defpackage :ulf2english
  (:use :cl :ttt :util :ulf-lib :cl-strings :inferior-shell :lisp-unit :drakma :cl-json)
  (:export ulf2english))

