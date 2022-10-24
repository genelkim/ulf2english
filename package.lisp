;; ULF to English
;; Packaged on 2018-11-24

(in-package :cl-user)

(defpackage :ulf2english
  (:use :cl :ttt :cl-strings :cl-json :gute :ulf-lib :inferior-shell :lisp-unit :drakma :py4cl)
  (:shadow :insert)
  (:export ulf2english))

