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

(defparameter *agent-specs*
  `((simple-model :L 89)
    (channel-breakout-trend-following :fast-period 89 :slow-period 211)
    (envelope-moving-avg-trend-following :N 89 :width 3.2)
    (adaptive-moving-avg-trend-following
     :min-period 59 :max-period 126 :width-factor 3.2 :snr-factor 0.5)
    (fractal-ama-trend-following :max-period 200 :min-period 1 :fractal-length 126)
    (opening-range-breakout :volatility-limit 1.5 :N 21)
    (range-projection-mean-reversion :N 34 :projection-interval :week)
    (swing-breakout :event-count 21 :expected-width 1.5 :price-extension 2.0)
    (swing-mean-reversion :event-count 21 :expected-width 2.0 :max-allowed-breakout 1.5)))

(defmacro instantiate-agent (agent-specs security)
  (destructuring-bind (agent-type &rest init-args)
      agent-specs
    `(make-instance ',agent-type ,@init-args :security ,security)))

;; create the trading agents that will process the historical data
(setf *agents*
      (loop for security in '(:msft :aapl)
            appending (loop for agent-specs in *agent-specs*
                            collecting (instantiate-agent agent-specs security))))

;; create a list of all events in datetime order for the simulation engine
(defparameter *events* (sort (copy-list (union (cdr (assoc :msft *security-data*))
                                               (cdr (assoc :aapl *security-data*))))
                             (lambda (x y)
                               (local-time:timestamp< (timestamp x) (timestamp y)))))

(run-simulation *events*)

(analyze *agents* *security-data*)

;; EOF
