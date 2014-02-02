;;;; backtesting-simulation.lisp

(in-package #:trading-core)

(setf logv:*log-output* nil)

;; Set the location of the historical data used in the simulation if different from the default.
;(setf *historical-data-path* "C:/Worden/TeleChart/Export/SP500_Components/")

;; Set the location where the analysis result template is located if different from default
;(setf *ui-template-path* #P"C:/Users/Jonathan/Lisp/projects/trading-core/trading-ui/templates/")

;; Set the location where the analysis results will be placed if different from default
;(setf *analysis-results-path* #P"C:/Users/Jonathan/Lisp/projects/trading-core/trading-ui/")

;; load historical data
(defparameter *security-data* `((:msft . ,(load-event-data "MSFT" :data-format :bar 
                                                           :start-date 199201010000
                                                           :end-date 201201010000))
                                (:aapl . ,(load-event-data "AAPL" :data-format :bar 
                                                           :start-date 199201010000
                                                           :end-date 201201010000))))

;; create the trading agents that will process the historical data
(setf *agents*
      (list (make-instance 'adaptive-moving-avg-trend-following
                           :n-min 3 :n-max 21 :width-factor 1.5
                           :snr-factor .3 :security :msft)
            (make-instance 'adaptive-moving-avg-trend-following
                           :n-min 3 :n-max 21 :width-factor 1.5
                           :snr-factor .1 :security :aapl)
            (make-instance 'adaptive-moving-avg-trend-following
                           :n-min 3 :n-max 21 :width-factor 1.5
                           :snr-factor .3 :security :aapl)
            (make-instance 'adaptive-moving-avg-trend-following
                           :n-min 3 :n-max 21 :width-factor 1.2
                           :snr-factor .3 :security :aapl)))

;; create a list of all events in datetime order for the simulation engine
(defparameter *events* (sort (copy-list (union (cdr (assoc :msft *security-data*)) (cdr (assoc :aapl *security-data*))))
                             (lambda (x y)
                               (< (timestamp x) (timestamp y)))))

(run-simulation *events*)

(analyze *agents* *security-data*)

;; EOF
