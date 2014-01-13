;;;; simulate.lisp

(in-package #:trading-core)

;; Location of the historical data used in the simulation.
(defparameter *data-dir-string* "C:/Worden/TeleChart/Export/SP500_Components/")

;; List of timeseries price events
(defparameter *data-time-series* nil)

(defparameter *agag*
  (make-instance 'aggregate-agent
                 :name "MyAggregateAgent"
                 :members (list *agents*)))

(push *agag* *aggregate-agents*)

(defparameter *csv-pattern-regex* (cl-ppcre:create-scanner "([0-9.,]+)([A-Z]+)" ;; :multi-line-mode t
                                                           ))

(defun create-sexpr (match &rest registers)
  (format nil "(~A~{\"~A\"~^ ~})"
          (map 'string (lambda (c) (if (char= c #\,) #\Space c)) (first registers))
          (rest registers)))

(defun load-event-data (data-name &key
                                  (data-format :prc)
                                  (data-dir *data-dir-string*)
                                  (start-date nil)
                                  (end-date nil))
  "Read a comma-separated data file of historical price data and create
appropriate market-update price classes for each data record. Optionally
filter out prices based on a desired date range."
  (let ((data-path (make-pathname
                     :defaults data-dir
                     :name data-name
                     :type "txt"))
        (security (intern data-name "KEYWORD"))
        events)
    (when (probe-file data-path)
      (let ((price-data (concatenate 'string
                                     "("
                                     (cl-ppcre:regex-replace-all *csv-pattern-regex*
                                                                 (file-io:slurp-file data-path)
                                                                 #'create-sexpr
                                                                 :simple-calls t)
                                     ")")))
        (setf *data-time-series* (safe-read-from-string price-data :data-error))
        (when (eq *data-time-series* :data-error)
          (error "Unable to import data for ~A security. Code injection point found." data-name)))
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
            *data-time-series*)
      events)))

(defun compute-future-data (historical-events)
  "Compute a set of \"future\" data based on a set of historical events."
  (cond ((typep (first historical-events) 'prc)
         (compute-future-prc-data historical-events))
        ((typep (first historical-events) 'bar)
         (compute-future-bar-data historical-events))
        (t (error "Forward casting of type ~A events is not supported."
                  (type-of (first historical-events))))))

(defun classify-price-change (price-change ranges)
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
  (dolist (a *agents*)
    (initialize a))
  (setf *events-queue* events)
  (loop for e = (pop *events-queue*)
        while e do (dolist (a *agents*)
                     (consume a e))))

(defun interval-division-predicates-bins (min-lvl max-lvl num-bins
                                          &key (hard-lower-bound nil) (hard-upper-bound nil))
  "Generate the set of predicates needed to group a set of agents into a specific
number of bins. The HARD-LOWER-BOUND and HARD-UPPER-BOUND options allow strict limitations
of the covered bin ranges."
  (let ((predicates-list nil)
        (bins-list nil)
        (subdivision (/ (- max-lvl min-lvl) num-bins))
        (first-pred (lambda (x) (< x min-lvl)))
        (first-bin (list :min_inf min-lvl))
        (last-pred (lambda (x) (>= x max-lvl)))
        (last-bin (list max-lvl :plus_inf)))
    (unless hard-lower-bound
      (push first-pred predicates-list)
      (push first-bin bins-list))
    (dotimes (i num-bins)
      (let* ((bin-left (+ min-level (* i subdivision)))
             (bin-right (+ bin-left subdivision)))
        (push (lambda (x)
                (and (>= x bin-left)
                     (< x bin-right)))
              predicates-list)
        (push (list bin-left bin-right) bins-list)))
    (unless hard-upper-bound
      (push last-pred predicates-list)
      (push last-bin bins-list))
    (values (nreverse predicates-list) (nreverse bins-list))))

(defun classify (objects-list predicates-list)
  "Iterate of a set of predicates and a set of objects and create a set of bins
containing the objects specified by the predicates."
  (let ((bins-list nil))
    (dolist (pred predicates-list)
      (let ((p-bin nil))
        (dolist (obj objects-list)
          (when (funcall pred obj)
            (push obj p-bin)))
        (push (nreverse p-bin) bins-list)))
    (values (nreverse bins-list))))

(defun cluster-agents (agents what num-bins)
  "Cluster a set of agents into a specified number of bins based on a trade statistic."
  (labels ((getstat (a)
             (let ((tradestat (first (tradestats a))))
               (case what
                 (:tpl (trade-stat-tot-pl tradestat))
                 (:lrt (trade-stat-average-logret tradestat))
                 (:wtl (trade-stat-win-to-loss tradestat))
                 (:pcp (trade-stat-percent-profitable tradestat))
                 (:pff (trade-stat-profit-factor tradestat))))))
    (let (stats min-stats max-stats)
      (loop for a in agents
            for stat = (getstat a)
            collecting stat into stats
            minimize stat into min-stats
            maximize stat into max-stats)
      (multiple-value-bind (numeric-predicates bins)
          (interval-division-predicates-bins min-stats max-stats num-bins)
        (let* ((agent-predicates (mapcar (lambda (p)
                                           (lambda (a)
                                             (funcall p (getstat a))))
                                         numeric-predicates))
               (agent-clusters (classify agents agent-predicates)))
          (values bins agent-clusters))))))

(defun analyze (agents)
  "NOT IMPLEMENTED. Function to create web pages that display the results of a trading simulation."
  nil)

;;; Example backtesting simulation
#|

;; load historical data
(defparameter *MSFT-events* (load-event-data "MSFT" :data-format :bar))
(defparameter *AAPL-events* (load-event-data "AAPL" :data-format :bar))

(push (make-instance 'adaptive-moving-avg-trend-following
                           :n-min 10
                           :n-max 55
                           :width-factor 1.5
                           :snr-factor .5
                           :security :msft)
      *agents*)
(push (make-instance 'adaptive-moving-avg-trend-following
                           :n-min 10
                           :n-max 55
                           :width-factor 1.5
                           :snr-factor .5
                           :security :aapl)
      *agents*)

(setf *events* (sort (union *MSFT-events* *AAPL-events*)
                     (lambda (x y)
                       (< (timestamp x) (timestamp y)))))

(run-simulation *events*)

|#

;;; Example forwardtesting simulation
#|

;; calculate a set of "future" event data using existing historical data
(defparameter *MSFT-events* (compute-future-data (load-event-data "MSFT" :data-format :bar)))
(defparameter *AAPL-events* (compute-future-data (load-event-data "AAPL" :data-format :bar)))

(setf *agents*
      (list (make-instance 'adaptive-moving-avg-trend-following
                           :n-min 10
                           :n-max 55
                           :width-factor 1.5
                           :snr-factor .5
                           :security :msft)
            (make-instance 'adaptive-moving-avg-trend-following
                           :n-min 10
                           :n-max 55
                           :width-factor 1.5
                           :snr-factor .5
                           :security :aapl)
            (make-instance 'swing-mean-reversion
                           :max-allowed-breakout 1.25
                           :expected-width 5.0
                           :event-count 34
                           :security :msft)
            (make-instance 'swing-mean-reversion
                           :max-allowed-breakout 1.25
                           :expected-width 5.0
                           :event-count 34
                           :security :aapl)))


(setf *events* (sort (union *MSFT-events* *AAPL-events*)
                     (lambda (x y)
                       (< (timestamp x) (timestamp y)))))

(run-simulation *events*)

|#

;;; Parameter Search and Optimization
#|

;; initialize a set of agents spanning the range of parameters desired
(dotimes (x 100)
  (push (make-instance 'simple-model
                       :L (+ i 10))
        *agents*))

;; run the simulation
(run-simulation (load-event-data "SPY"))

;; compute the trade statistics for each agent
(let ((results nil))
  (dolist (a *agents*)
    (push (list (L a)
                (trade-stat-tot-pl (first (tradestats a))))
          results))
  results)

;; find the top performing subset of agents for a particular trade statistic
(cluster-agents *agents* :tpl 10) ; total profit-loss

|#

;; EOF
