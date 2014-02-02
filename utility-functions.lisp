;;;; utility-functions.lisp

(in-package #:trading-core)

;;; utility-functions methods

(defun avg-list (list &optional (key-fn #'identity))
  "Calculate the average value in a list in an efficient manner."
  (let ((sum 0)
        (len 0))
    (dolist (x list)
      (incf sum (funcall key-fn x))
      (incf len 1))
    (if (> len 0)
      (/ sum len)
      0)))

(defun sub-list (list start-index end-index)
  "Get subset of list without throwing errors if indexes are bad. START-INDEX and END-INDEX are inclusive."
  (if (< end-index start-index)
    nil
    (let ((result nil)
      (loop for i = 0 then (1+ i)
            for v in list
            when (<= start-index i end-index)
              collecting v)))))

(defmacro case-equal (exp &body clauses)
  "Case macro with test EQUAL instead of standard EQ test."
  (let ((temp (gensym)))
    `(let ((,temp ,exp))
       (cond ,@(mapcar (lambda (clause)
                         (destructuring-bind (keys . clause-forms) clause
                           (cond ((eq keys 'otherwise)
                                  `(t ,@clause-forms))
                                 (t (if (atom keys)
                                      (setq keys (list keys)))
                                    `((member ,temp `,keys
                                              :test #'equal
                                              ,@clause-forms))))))
                       clauses)))))

(defun date-components (YYYYMMDD)
  "Break combined date value (YYYYMMDD) into component parts."
  (multiple-value-bind (YYYY MMDD)
      (truncate YYYYMMDD 10000)
    (multiple-value-bind (MM DD)
        (truncate MMDD 100)
      (values YYYY MM DD))))

(let ((days-in-months '(31 28 31 30 31 30 31 31 30 31 30 31)))
  (defun julian-day (YYYYMMDD)
    "Julian date calculator (days from 01-Jan-1900)"
    (multiple-value-bind (YYYY MM DD)
        (date-components YYYYMMDD)
      (let ((num-year-since-1900 (- YYYY 1900)))
        (multiple-value-bind (division-int division-rest)
          (truncate num-year-since-1900 4)
          (let* ((this-year-leap-p (zerop division-rest))
                 (num-leap-years-since-1900 (if this-year-leap-p
                                              division-int
                                              (1+ division-int)))
                 (num-days-since-year-began (+ DD (reduce #'+ (subseq days-in-months 0 (1- MM)))
                                               (if (and this-year-leap-p (> MM 2)) 1 0))))
            (values (+ num-days-since-year-began
                       num-leap-years-since-1900
                       (* 365 num-year-since-1900))
                    num-days-since-year-began
                    num-leap-years-since-1900
                    num-year-since-1900)))))))

(defun f-h-m (HHNN)
  "Fraction of day since midnight"
  (multiple-value-bind (HH NN)
      (truncate HHNN 100)
      (/ (+ (* HH 60) NN) (* 24 60))))

(defun u-d-h-m (YYYYMMDDHHNN)
  "Universal timestamp using Julian date (01-Jan-1900 epoch) and fraction of day after midnight."
  (multiple-value-bind (YYYYMMDD HHNN)
      (truncate YYYYMMDDHHNN 10000)
    (coerce (+ (julian-day YYYYMMDD) (f-h-m HHNN)) 'double-float)))

(defun julian-to-unix-timestamp (julian-timestamp)
  "Convert julian timestamp (days since 01-Jan-1990 plus fraction of day since midnight)
to the corresponding unix timestamp (seconds since 01-Jan-1970)."
  (let ((seconds-per-day 86400))
    (multiple-value-bind (days fraction-after-midnight)
        (truncate (- julian-timestamp 25569.0D0) 1)
      (+ (* days seconds-per-day)
         (truncate (* fraction-after-midnight seconds-per-day) 1)))))

;; SAFE-READ-FROM-STRING taken from "Let Over Lambda" by Doug Hoyte
;; Prevent code injection when reading in historical price data
(defvar safe-read-from-string-blacklist
  '(#\# #\: #\|))

(let ((rt (copy-readtable nil)))
  (defun safe-reader-error (stream closech)
    (declare (ignore stream closech))
    (error "safe-read-from-string failure"))

  (dolist (c safe-read-from-string-blacklist)
    (set-macro-character
      c #'safe-reader-error nil rt))

  (defun safe-read-from-string (s &optional fail)
    (if (stringp s)
      (let ((*readtable* rt) *read-eval*)
        (handler-bind
          ((error (lambda (condition)
                    (declare (ignore condition))
                    (return-from
                      safe-read-from-string fail))))
          (read-from-string s)))
      fail)))

;;EOF
