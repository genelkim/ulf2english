;; ULF to English
;; Packaged on 2018-11-24

(asdf:defsystem :ulf2english
  :name "ulf2english"
  :version "0.0.1"
  :author "Gene Louis Kim"
  :depends-on (:ttt :util :ulf-lib :cl-strings :inferior-shell :drakma :cl-json :lisp-unit)
  :components ((:file "package")
               (:file "uppen-morph")
               (:file "pattern-en")
               (:file "ulf2english"))) 

