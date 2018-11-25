;;; Gene Kim 8-1-2018
;;;
;;; Code for interacting with the Uppen Morphological Data.  It interfaces from
;;; a version of the file that has been formatted into S-expressions.  See
;;; resources/uppen_morph_analysis/.

(in-package :ulf2english)

(defparameter *uppen-morph-data* nil)

;; Loads the Uppen Morphological Data file provided, parses it into an
;; appropriate hashtable format and stores it in *uppen-morph-data*.
;;
;; Data format:
;;  (<token> ((<lemma> <POS> ([list of features])) ...)
;; Where features can be: "3sg", "PPART", "GEN", etc.
;; e.g.
;;  (run ((run N (3sg)) (run V (PPART STR)) (run V (INF))))
;;
;; The stored hashtable will have the structure:
;;  <lemma> -> ((<token> <POS> ([list of features])) ...)
;;
(defun load-uppen-morph (sexp-filepath &optional (limit10000 t))
  (print "Loading Uppen Morph data...")

  ;; Build the data structure as we read the objects so we don't need to 
  ;; allocate extra memory for the intermediate data.
  (let ((in (open sexp-filepath))
        (ht (make-hash-table :test #'equal 
                             :size (if limit10000 6000 80000)))
        topwords-ht)

    (if limit10000
      ;; Set topwords-ht as a hashtable from word in list -> t.
      (setq topwords-ht
            (alexandria:alist-hash-table
              (mapcar #'(lambda (x) (cons x t))
                      (util:read-all-from-file *top10000-word-filepath*)))))

    (when in
      (loop for e = (read in nil)
            while e do
            (let* ((token (car e))
                   (lemmainfos (cadr e)))
              (dolist (lemmainfo lemmainfos)
                (let ((lemma (first lemmainfo))
                      (pos (second lemmainfo))
                      (newentry (cons token (cdr lemmainfo))))
                  (if (and (not (eql pos 'propn)) ; ignore proper nouns
                           (or (not limit10000)   ; not limited or member of limit list
                               (gethash lemma topwords-ht)))
                    (setf (gethash lemma ht)
                          (cons newentry (gethash lemma ht))))))))
      (close in)
      (setq *uppen-morph-data* ht))

    ;; Force garbage collect since this function has a lot of overhead.
    (excl:gc)
    (print "Done!")))


;; Conjugates the given lemma and POS into the construction with the given
;; features.  If none are found, it tries to guess.  If results are found, 
;; the lemma is simple returned.
(defun um-conjugate (lemma pos features)
  (if (not *uppen-morph-data*)
    (error "Please first call 'load-uppen-morph' to set up the data structures."))
  (let ((cnjs (gethash lemma *uppen-morph-data*))
        poscnjs filtered)
    (setq poscnjs (remove-if-not #'(lambda (x) (equal pos (second x))) cnjs))
    (if (not poscnjs) (return-from um-conjugate lemma))
    (setq sorted (sort poscnjs 
                       #'(lambda (x y)
                           (or
                             ;; Select the entry with most overlapping features
                             ;; with query
                             (> (length (intersection (third x) features))
                                (length (intersection (third y) features)))
                             ;; If multiple overlap the same amount, choose the
                             ;; one with fewer features.
                             (and 
                               (= (length (intersection (third x) features))
                                  (length (intersection (third y) features)))
                               (< (length (third x))
                                  (length (third y))))))))
    (caar sorted)))



;; Function used once to make POS to id mapping.
(defun list-poses (sexp-filepath)
  (print "Loading Uppen Morph data...")
  ;; Build the data structure as we read the objects so we don't need to 
  ;; allocate extra memory for the intermediate data.
  (let ((in (open sexp-filepath))
        poses)
    (when in
      (loop for e = (read in nil)
            while e do
            (let* ((token (car e))
                   (lemmainfos (cadr e)))
              (dolist (lemmainfo lemmainfos)
                (let ((pos (second lemmainfo)))
                  (if (not (member pos poses))
                    (setq poses (cons pos poses)))))))
      (close in))
    (print "Done!")
    poses))

;; Function used once to make Feature to id mapping.
(defun list-features (sexp-filepath)
  (print "Loading Uppen Morph data...")
  ;; Build the data structure as we read the objects so we don't need to 
  ;; allocate extra memory for the intermediate data.
  (let ((in (open sexp-filepath))
        featurelst)
    (when in
      (loop for e = (read in nil)
            while e do
            (let* ((token (car e))
                   (lemmainfos (cadr e)))
              (dolist (lemmainfo lemmainfos)
                (let ((features (third lemmainfo)))
                  (dolist (f features)
                    (if (not (member f featurelst))
                      (setq featurelst (cons f featurelst))))))))
            (close in))
      (print "Done!")
      featurelst))

