(ql:quickload :cl-coveralls)
(coveralls:with-coveralls ()
  (load "runtest"))
