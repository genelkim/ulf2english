;;; Gene Kim 2-19-2019
;;; File with functions to interact with the Pattern.en package.

(in-package :ulf2english)

(defparameter *python-server-url* "http://localhost:8080")
(defparameter *python-server-username* "g")
(defparameter *python-server-password* "g")

(defvar *python-call-methods* '(socket shell py4cl))

(defun ulf2pen-tense (tense)
  (case (gute:safe-intern tense :ulf2english)
    (pres 'present)
    (past 'past)
    (cf 'past)
    (otherwise (error "~s is not a valid ulf tense" tense))))

;; Takes the function name, list of required arguments, followed by an
;; association list of (key . arg) pairs for keyed arguments and returns
;; the string for the corresponding Python function call.
;;
;; At the moment this function assumes that the required arguments may be
;; strings, whereas keyed arguments are not.
(defun python-call (fnname required-args keyed-args)
  (let ((rarg-str (format nil "~{~s~^,~}" required-args))
        (karg-str (format nil "~{~a~^,~}"
                          (loop for (key . value) in keyed-args
                                collect (intern (format nil "~(~a~)=~a" key value))))))
    (cl-strings:join
      (list (format nil "~(~a~)" fnname) "(" rarg-str "," karg-str ")"))))

(defun string-output-python-call (rawcall)
  (cl-strings:join (list "str(" rawcall ")")))

(defparameter *conjugate-params*
  '((tense . (INFINITIVE PRESENT PAST FUTURE))
    (person . (1 2 3 |None|))
    (number . (SG PL))
    (mood . (INDICATIVE IMPERATIVE CONDITIONAL SUBJUNCTIVE))
    (aspect . (IMPERFECTIVE PERFECTIVE PROGRESSIVE))
    (negated . (|True| |False|))
    (parse . (|True| |False|))))


;; Makes a python call via a socket to a server that interprets it.
(defun python-over-socket (expression cmd-type)
  (assert (member cmd-type '(exec eval)))
  (let (;; Build the parameter string from the given command type and expression.
        (paramstr (json:encode-json-to-string
                    `(("cmd" . ,cmd-type) ("exp" . ,expression))))
        ;; The post-process the request output by decoding into a string followed
        ;; by json, then extracting the result.
        (post-proc-fn
          (gute:compose
            #'(lambda (x) (cdr (assoc :result x)))
            #'json:decode-json-from-string
            #'flexi-streams:octets-to-string))
        raw-request-result)
    (setq raw-request-result
      (drakma:http-request *python-server-url* :method :post
                           :content paramstr
                           :basic-authorization (list *python-server-username*
                                                      *python-server-password*)))
    (if raw-request-result
      (funcall post-proc-fn raw-request-result)
      raw-request-result)))

;; Makes a python call via py4cl.
;; The same behavior as 'python-over-socket'.
(defun python-over-py4cl (expression cmd-type)
  (case cmd-type
    (exec
      (handler-case (py4cl:python-exec expression)
        ; Just try again because of an issue in py4cl which doesn't handle generators correctly in Python >3.6.
        (py4cl:python-error () (py4cl:python-exec expression))))
    (eval
      (handler-case (py4cl:python-eval expression)
        (py4cl:python-error () (py4cl:python-eval expression))))
    (otherwise
      (error "Invalid value for cmd-type argument of python-over-py4cl. cmd-type: ~s~%"
             cmd-type))))

;; Flag for setting up the python environment for pattern.en and the function
;; that performs the setup. The function sets the flag to t so it's not rerun.
;; NB: This flag is shared between the socket and py4cl versions of interfacing
;; with pattern.en. Therefore, each SBCL session should only use one of the two
;; or explicitly set this flag to nil when changing interface modes.
(defparameter *setup-complete* nil)
(defun setup-pattern-en-env (pyfn)
  "Sets up the python environment for pattern.en calls. 'pyfn' is the function
  for interfacing with the Python environment (python-over-socket or
  pyhton-over-py4cl)."
  (funcall pyfn "from pattern.en import *" 'exec)
  ;; For some reason these aren't defined in pattern.en
  (funcall pyfn "IMPERFECTIVE='imperfective'" 'exec)
  (funcall pyfn "PERFECTIVE='perfective'" 'exec)
  (setq *setup-complete* t))

;; Makes a python-call to pattern.en using the provided method.
(defun make-pattern-en-call (python-call python-method)
  ;; socket - makes a python call through a socket (started via
  ;;          python-repl-server.py).
  ;; py4cl - makes a python call through the py4cl library (recommended).
  ;; shell - makes a python call through call-pattern-en-fn.py.
  ; Enforce string value output (vs. say unicode)
  (setf python-call (string-output-python-call python-call))
  (case python-method
    ;; Run the python call over a socket.
    (socket
      (when (not *setup-complete*)
        (setup-pattern-en-env #'python-over-socket))
      (python-over-socket python-call 'eval))
    ;; Run the python call over py4cl.
    (py4cl
      (when (not *setup-complete*)
        (setup-pattern-en-env #'python-over-py4cl))
      (python-over-py4cl python-call 'eval))
    ;; Run the python call through the shell.
    (shell
      (inferior-shell:run/s
        `(python call-pattern-en-fn.py
                 ,python-call)))
    (otherwise
      (error "Invalid 'python-method' value for 'make-pattern-en-call'. python-method: ~s~%"
             python-method))))

;; Takes an input verb string and conjugation parameters and returns a
;; conjugated verb string.
(defun pattern-en-conjugate (verb &key
                                  (tense 'PRESENT)
                                  (person 3)
                                  (number 'SG)
                                  (mood 'INDICATIVE)
                                  (aspect 'IMPERFECTIVE)
                                  (negated '|False|)
                                  (parse '|True|)
                                  (python-method 'py4cl))
  ;; Assert that all the arguments are valid.
  (assert (member tense (cdr (assoc 'tense *conjugate-params*))))
  (assert (member person (cdr (assoc 'person *conjugate-params*))))
  (assert (member number (cdr (assoc 'number *conjugate-params*))))
  (assert (member mood (cdr (assoc 'mood *conjugate-params*))))
  (assert (member aspect (cdr (assoc 'aspect *conjugate-params*))))
  (assert (member negated (cdr (assoc 'negated *conjugate-params*))))
  (assert (member parse (cdr (assoc 'parse *conjugate-params*))))
  (assert (member python-method *python-call-methods*))

  (let* (;; Construct an keyed argument list by getting the value for each
         ;; parameter.
         (arglist `((tense . ,tense)
                    (person . ,person)
                    (number . ,number)
                    (mood . ,mood)
                    (aspect . ,aspect)
                    (negated . ,negated)
                    (parse . ,parse)))
         ;; Construct the Python function call.
         (python-call (python-call 'conjugate (list verb) arglist)))
    ;; Since it's all the same case anyway, make it upper case so it doesn't force
    ;; a case sensitive symbol.
    (string-upcase
      (make-pattern-en-call python-call python-method))))


;; Takes an input noun string and pluralizes it.
;; NB: The pattern.en function has some additional parameters, but they don't
;; seem useful for the ULF project so they're not suppoted currently.
(defun pattern-en-pluralize (noun &key (python-method 'py4cl)
                                       (preserve-case nil))
  (assert (member python-method *python-call-methods*))
  (let* ((python-call (python-call 'pluralize (list noun) nil))
         (rawout (make-pattern-en-call python-call python-method)))
    (if preserve-case rawout (string-upcase rawout))))


;; Takes an input adjective string and converts it to superlative form.
(defun pattern-en-superlative (adj &key (python-method 'py4cl)
                                        (preserve-case nil))
  (assert (member python-method *python-call-methods*))
  (let* ((python-call (python-call 'superlative (list adj) nil))
         (rawout (make-pattern-en-call python-call python-method)))
    (if preserve-case rawout (string-upcase rawout))))


;; Takes an input adjective string and converts it to comparative form.
(defun pattern-en-comparative (adj &key (python-method 'py4cl)
                                        (preserve-case nil))
  (assert (member python-method *python-call-methods*))
  (let* ((python-call (python-call 'comparative (list adj) nil))
         (rawout (make-pattern-en-call python-call python-method)))
    (if preserve-case rawout (string-upcase rawout))))

;; Memoize.
(memoize 'pattern-en-conjugate)
(memoize 'pattern-en-pluralize)
(memoize 'pattern-en-superlative)
(memoize 'pattern-en-comparative)

