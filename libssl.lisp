(ql:quickload :cffi)
 (cffi:load-foreign-library "libssl.so.1.0.0")

    (let ((*features* (cons :cl+ssl-foreign-libs-already-loaded
                            *features*)))

      (ql:quickload :a-system-which-depends-on-cl+ssl)

      ;; or just load cl+ssl
      (ql:quickload :cl+ssl))