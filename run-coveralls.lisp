(ql:quickload :cl-coveralls)
;(setf uiop:*temporary-directory* *default-pathname-defaults*)
(trace dex:post dex:request coveralls::calc-coverage coveralls::get-coverage coveralls::finalize-coverage coveralls::parse-report-files string-downcase usocket::socket-connect dex::ensure-socks5-connected quri::uri quri::url-encoder-params dex::make-new-connection quri::uri-scheme string=)
(coveralls:with-coveralls ()
  (load "runtest"))
