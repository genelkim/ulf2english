;; ULF to English
;; Packaged on 2018-11-24

(in-package :cl-user)

(defpackage :ulf2english
  (:use :cl :ttt :cl-strings :cl-json :cl-util :ulf-lib :inferior-shell :lisp-unit :drakma)
  (:shadow :insert)
  (:shadowing-import-from :cl-ppcre)
  (:export ulf2english))

