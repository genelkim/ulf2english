;;; Gene Kim 03-20-2020
;;; Functions for converting adjectives to adverbs.

(in-package :ulf2english)

(defparameter *adverbs* nil)

(defun adj2adv (adj)
  "Takes an adjective string and returns the corresponding adverb string."
  (when (null *adverbs*)
    (setf *adverbs* *wordnet-adverbs*))
  ;; Cases
  ;; 1. ends in -ly, no change
  ;; 2. ends in -y, replace -y with -ily
  ;; 3. otherwise, append -ly
  ;; Check if modification is in the adv-file data. If not, return original string.
  (let ((guess
          (cond
            ((cl-strings:ends-with adj "ly" :ignore-case t)
             (string-upcase adj))
            ((cl-strings:ends-with adj "y" :ignore-case t)
             (string-upcase
               (cl-strings:join (list (subseq adj 0 (1- (length adj)))
                                      "ily"))))
            (t (string-upcase (cl-strings:join (list adj "ly")))))))
    (if (member guess *adverbs* :test #'equal) guess adj)))

