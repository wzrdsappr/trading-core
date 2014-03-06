;;;; simulate.lisp

(in-package #:trading-core)

(defparameter *historical-data-path* "C:/Worden/TeleChart/Export/SP500_Components/"
  "Location of the historical data used in the simulation.")

;; TODO : Move this code to an example file.  At the point this is being executed
;; the agents probabably haven't been added to the *agents* parameter yet.
(push (make-instance 'aggregate-agent
                     :name "MyAggregateAgent"
                     :members (list *agents*))
      *aggregate-agents*)

(defparameter *csv-pattern-regex* (cl-ppcre:create-scanner "([0-9.,]+)([A-Z]+)"))

(defun create-sexpr (match &rest registers)
  (format nil "(~A~{\"~A\"~^ ~})"
          (map 'string (lambda (c) (if (char= c #\,) #\Space c)) (first registers))
          (rest registers)))

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

(defun load-event-data (data-name &key (data-format :prc)
                                       (data-dir *historical-data-path*)
                                       (start-date nil)
                                       (end-date nil))
  "Read a comma-separated data file of historical price data and create appropriate
market-update price classes for each data record. Optionally filter out prices
based on a desired date range."
  (let ((data-path (make-pathname
                     :defaults data-dir
                     :name data-name
                     :type "txt"))
        (security (intern data-name "KEYWORD"))
        (events '()))
    (if (probe-file data-path)
      (progn
        (let ((price-data (concatenate 'string
                                       "("
                                       (cl-ppcre:regex-replace-all *csv-pattern-regex*
                                                                   (file-io:slurp-file data-path)
                                                                   #'create-sexpr
                                                                   :simple-calls t)
                                       ")"))
              data-time-series)
          (setf data-time-series (safe-read-from-string price-data :data-error))
          (when (eq data-time-series :data-error)
            (error "Unable to import data for ~A security. Code injection point found." data-name))
          (mapc (lambda (r)
                  (let ((date (if (< (first r) 100000000)
                                (* (first r) 10000)
                                (first r))))
                    (when (and (or (not start-date) (>= date start-date))
                               (or (not end-date) (<= date end-date)))
                      (push (make-instance
                              (ccase data-format
                                     (:prc 'prc)
                                     (:book 'book)
                                     (:delta 'delta)
                                     (:bar 'bar))
                              :security security
                              :timestamp (u-d-h-m date)
                              :value (rest r))
                            events))))
                data-time-series))
        (nreverse events))
      (error "Price data file not found: [~A]" data-path))))

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
        (interval-division-predicates-bins 0 (+ max-positive-change *epsilon*) 3
                                           :hard-lower-bound t :hard-upper-bound t)
      (multiple-value-bind (negative-numeric-predicates negative-bins)
        (interval-division-predicates-bins max-negative-change 0 3
                                           :hard-lower-bound t :hard-upper-bound t)
        (let* ((ranges (pairlis '(:neg-tail :neg-mid :neg-body :pos-body :pos-mid :pos-tail)
                                (union negative-bins positive-bins)))
               (historical-ranges (loop for change in historical-changes
                                        collecting (classify-price-change change ranges))))
          (loop for future-prices = (list (random 1000))
                for (range min-change max-change) in historical-ranges
                do (push (* (first future-prices) (+ min-change (random (- max-change min-change))))
                         future-prices)
                finally (return (nreverse future-prices))))))))

;; TODO : Bar future price calculations not yet implemented
(defun compute-future-bar-data (historical-events)
  "Compute a set of \"future\" data based on a set of historical events."
  (let* ((percent-innovations ()))
    nil))

(defun run-simulation (events)
  "Method used to simulate trading by on a set of events by the agents in the *AGENTS* list."
  (dolist (a *agents*)
    (initialize a))
  (setf *events-queue* events)
  (loop for e = (pop *events-queue*)
        while e do (dolist (a *agents*)
                     (consume a e))))

;; EOF
