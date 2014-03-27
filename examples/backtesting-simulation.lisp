;;;; backtesting-simulation.lisp
;;; An example of backtesting multiple agents on historical data and generating analysis reports.

(in-package #:trading-core)

(setf logv:*log-output* nil)

;; Set the location of the historical data used in the simulation if different from the default.
(setf *historical-data-path* "C:/Worden/TeleChart/Export/SP500_Components/")

;; Set the location where the analysis result template is located if different from default
;(setf *ui-template-path* #P"C:/Users/Jonathan/Lisp/projects/trading-core/trading-ui/templates/")

;; Set the location where the analysis results will be placed if different from default
;(setf *analysis-results-path* #P"C:/Users/Jonathan/Lisp/projects/trading-core/trading-ui/")

;; load historical data
(defparameter *security-data*
  `((:msft . ,(load-event-data "MSFT"
                               :start-date "1992-01-01" :end-date "2010-01-01"))
    (:aapl . ,(load-event-data "AAPL"
                               :start-date "1992-01-01" :end-date "2010-01-01"))))

;; create the trading agents that will process the historical data
(setf *agents*
      (list (make-instance 'adaptive-moving-avg-trend-following
                           :min-period 5 :max-period 21 :width-factor 2.5
                           :snr-factor .9 :security :msft)
            (make-instance 'adaptive-moving-avg-trend-following
                           :min-period 59 :max-period 126 :width-factor 3.2
                           :snr-factor 0.5 :security :msft)
            (make-instance 'adaptive-moving-avg-trend-following
                           :min-period 62 :max-period 153 :width-factor 3.0
                           :snr-factor 0.6 :security :msft)
            (make-instance 'adaptive-moving-avg-trend-following
                           :min-period 5 :max-period 21 :width-factor 2.5
                           :snr-factor .9 :security :aapl)
            (make-instance 'adaptive-moving-avg-trend-following
                           :min-period 59 :max-period 126 :width-factor 3.2
                           :snr-factor 0.5 :security :aapl)
            (make-instance 'adaptive-moving-avg-trend-following
                           :min-period 62 :max-period 153 :width-factor 3.0
                           :snr-factor 0.6 :security :aapl)))

;; create a list of all events in datetime order for the simulation engine
(defparameter *events* (sort (copy-list (union (cdr (assoc :msft *security-data*))
                                               (cdr (assoc :aapl *security-data*))))
                             (lambda (x y)
                               (local-time:timestamp< (timestamp x) (timestamp y)))))

(run-simulation *events*)

(analyze *agents* *security-data*)

;; EOF
