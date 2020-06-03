;; ULF to English
;; Packaged on 2018-11-24

(asdf:defsystem :ulf2english
  :name "ulf2english"
  :version "0.0.1"
  :author "Gene Louis Kim"
  :depends-on (:ttt :cl-strings :cl-json :cl-util :cl-ppcre :ulf-lib :inferior-shell :drakma :py4cl :lisp-unit)
  :components ((:file "package")
               (:file "pattern-en")
               (:file "resources/wordnet-adverbs")
               (:file "adverbialize")
               (:file "ulf2english"))) 

