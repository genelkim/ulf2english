;;; Gene Kim 2-19-2019
;;; File with functions to interact with the Pattern.en package.

(in-package :ulf2english)

(defparameter *python-server-url* "http://localhost:8080")
(defparameter *python-server-username* "g")
(defparameter *python-server-password* "g")

(defun ulf2pen-tense (tense)
  (case tense
    ('pres 'present)
    ('past 'past)
    ('cf 'past)
    ;; TODO: raise error here
    (otherwise nil)))

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
          (util:compose
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


(defparameter *setup-complete* nil)

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
                                  (python-method 'socket))
  ;; Assert that all the arguments are valid.
  (assert (member tense (cdr (assoc 'tense *conjugate-params*))))
  (assert (member person (cdr (assoc 'person *conjugate-params*))))
  (assert (member number (cdr (assoc 'number *conjugate-params*))))
  (assert (member mood (cdr (assoc 'mood *conjugate-params*))))
  (assert (member aspect (cdr (assoc 'aspect *conjugate-params*))))
  (assert (member negated (cdr (assoc 'negated *conjugate-params*))))
  (assert (member parse (cdr (assoc 'parse *conjugate-params*))))
  (assert (member python-method '(socket shell)))

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

    ;; TODO: add a check that we can connect to the server.  if not, default to
    ;; the shell version.
    (case python-method
      ;; Run the python call over a socket.
      (socket
        (if (not *setup-complete*)
          (progn
            (python-over-socket "from pattern.en import *" 'exec)
            ;; For some reason these aren't defined in pattern.en
            (python-over-socket "IMPERFECTIVE='imperfective'" 'exec)
            (python-over-socket "PERFECTIVE='perfective'" 'exec)
            (setq *setup-complete* t)))
        ;; Finally, the actual call.
        (python-over-socket python-call 'eval))
      ;; Run the python call through the shell.
      (shell
        (inferior-shell:run/s
          `(python call-pattern-en-fn.py
                   ,python-call))))))
