;;;; simulate.lisp

(in-package #:trading-core)

(defparameter *historical-data-path*
  (merge-pathnames "trading-core/examples/example-data/"
                   (first ql:*local-project-directories*))
  "Location of the historical data used in the simulation.")

;; TODO : Move this code to an example file.  At the point this is being executed
;; the agents probabably haven't been added to the *agents* parameter yet.
;(push (make-instance 'aggregate-agent
;                     :name "MyAggregateAgent"
;                     :members (list *agents*))
;      *aggregate-agents*)

(defparameter *csv-pattern-regex* (cl-ppcre:create-scanner "([^,]+),([0-9., /-]+)([A-Z]+)?"))
(defparameter *date-regex* (cl-ppcre:create-scanner "(\\d+)/(\\d+)/(\\d+)( (\\d+)(:\\d+:\\d+) (AM|PM))?"))

(defun reformat-datestring (date-string)
  (cl-ppcre:register-groups-bind (month day year nil hour min-sec am-pm)
      (*date-regex* date-string :sharedp t)
    (when (and am-pm (string-equal am-pm "PM"))
      (setf hour (+ (parse-integer hour) 12)))
    (format nil "~A-~A-~A~@[T~A~A~]"
            year month day hour min-sec)))

(defun create-sexpr (match &rest registers)
  (declare (ignorable match))
  (format nil "(\"~A\" ~A~@[ \"~A\"~])"
          (reformat-datestring (first registers))
          (map 'string (lambda (c) (if (char= c #\,) #\Space c)) (second registers))
          (third registers)))

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

(defun load-event-data (data-name &key (data-format '(:time-bar 1 :day))
                                       (data-dir *historical-data-path*)
                                       (start-date nil)
                                       (end-date nil))
  "Read a comma-separated data file of historical price data and create appropriate
market-update price classes for each data record. Optionally filter out prices
based on a desired date range.

DATA-FORMAT should be either a standalone MARKET-EVENT subtype (:prc :book :delta) or
a list containing a MARKET-EVENT subtype, the event duration and units, if applicable.
(ex. '(:tick-bar 200) or '(:time-bar 1 :day).  It defaults to daily time bars.

START-DATE and END-DATE can be used to extract the exact date range you wish to use
from the set of records."
  (assert (or (member data-format '(:prc :book :delta))
              (and (listp data-format)
                   (member (first data-format) '(:tick-bar :time-bar)))))
  (unless (listp data-format)
    (setf data-format (list data-format)))
  (let ((data-path (make-pathname
                     :defaults data-dir
                     :name data-name
                     :type "txt"))
        (security (intern data-name "KEYWORD"))
        (start-date (local-time:parse-timestring start-date :allow-missing-elements t))
        (end-date (local-time:parse-timestring end-date :allow-missing-elements t))
        (events '()))
    (flet ((make-event (record)
             (let ((date (local-time:parse-timestring (first record) :allow-missing-elements t)))
               (when (and (or (not start-date) (local-time:timestamp>= date start-date))
                          (or (not end-date) (local-time:timestamp<= date end-date)))
                 (let ((event (make-instance
                                (ccase (first data-format)
                                  (:prc 'prc)
                                  (:book 'book)
                                  (:delta 'delta)
                                  (:tick-bar 'tick-bar)
                                  (:time-bar 'time-bar))
                                :security security
                                :timestamp date
                                :value (rest record))))
                   ;; add additional metadata for BAR subtypes.
                   (ccase (first data-format)
                          (:tick-bar (setf (num-ticks event) (second data-format)))
                          (:time-bar (setf (num-time-units event) (second data-format)
                                           (time-unit event) (third data-format))))
                   (push event events))))))
      (if (probe-file data-path)
        (progn
          (let ((price-data
                  (format nil "(~A)"
                          (cl-ppcre:regex-replace-all
                            *csv-pattern-regex*
                            (file-io:slurp-file data-path)
                            #'create-sexpr
                            :simple-calls t)))
                data-time-series)
            (setf data-time-series (safe-read-from-string price-data :data-error))
            (when (eq data-time-series :data-error)
              (error "Unable to import data for ~A security. Code injection point found." data-name))
            (mapc #'make-event data-time-series))
          (nreverse events))
        (error "Price data file not found: [~A]" data-path)))))

(defun compute-future-data (historical-events)
  "Compute a set of \"future\" data based on a set of historical events."
  (cond ((typep (first historical-events) 'prc)
         (compute-future-prc-data historical-events))
        ((typep (first historical-events) 'bar)
         (compute-future-bar-data historical-events))
        (t (error "Forward casting of type ~A events is not supported."
                  (type-of (first historical-events))))))

(defun classify-price-change (price-change ranges)
  "Determine the correct range for a given price-change."
  (labels ((check-range (range)
             (and (>= price-change (second range))
                  (< price-change (third range)))))
    (loop for range in ranges
          for range-found = (check-range range)
          when range-found return range)))

(defun compute-future-prc-data (historical-events)
  "Compute a set of \"future\" data based on a set of historical events."
  (multiple-value-bind (historical-changes max-positive-change max-negative-change)
      (loop for e = (first historical-events) then next-e
            for next-e in (rest historical-events)
            while (not (null next-e))
            for percent-change = (/ (- (last-price next-e) (last-price e)) (last-price e))
            collecting percent-change into percent-changes
            maximizing (max 0 percent-change) into max-pos-change
            minimizing (min 0 percent-change) into max-neg-change
            finally (return (values percent-changes max-pos-change max-neg-change)))
    (multiple-value-bind (positive-numeric-predicates positive-bins)
        (interval-division-predicates-bins 0 (+ max-positive-change +epsilon+) 3
                                           :hard-lower-bound t :hard-upper-bound t)
        (declare (ignorable positive-numeric-predicates))
      (multiple-value-bind (negative-numeric-predicates negative-bins)
        (interval-division-predicates-bins max-negative-change 0 3
                                           :hard-lower-bound t :hard-upper-bound t)
        (declare (ignorable negative-numeric-predicates))
        (let* ((ranges (pairlis '(:neg-tail :neg-mid :neg-body :pos-body :pos-mid :pos-tail)
                                (union negative-bins positive-bins)))
               (historical-ranges (loop for change in historical-changes
                                        collecting (classify-price-change change ranges))))
          (loop for future-prices = (list (random 1000))
                for (nil min-change max-change) in historical-ranges
                do (push (* (first future-prices) (+ min-change (random (- max-change min-change))))
                         future-prices)
                finally (return (nreverse future-prices))))))))

;; TODO : Bar future price calculations not yet implemented
(defun compute-future-bar-data (historical-events)
  "Compute a set of \"future\" data based on a set of historical events."
  (declare (ignore historical-events))
  nil)

(defun run-simulation (events &optional (agents *agents*) (aggregate-agents *aggregate-agents*))
  "Method used to simulate trading by on a set of events by the agents."
  (dolist (a agents)
    (initialize a))
  (setf *events-queue* events)
  (loop for e = (pop *events-queue*)
        while e
        do (progn
             (dolist (a agents)
               (consume a e))
             (dolist (aa aggregate-agents)
               (aggregate-trades aa)))))

;; EOF
