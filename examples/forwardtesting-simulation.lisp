;;;; forwardtesting-simulation.lisp

(in-package #:trading-core)

(log:config :warn)

;; Set the location of the historical data used in the simulation if different from the default.
;(setf *historical-data-path*
;  (merge-pathnames "trading-core/examples/example-data/"
;                   (first ql:*local-project-directories*)))

;; Set the location where the analysis result template is located if different from default
;(setf *ui-template-path*
;  (merge-pathnames "trading-core/trading-ui/templates/"
;                   (first ql:*local-project-directories*)))

;; Set the location where the analysis results will be placed if different from default
;(setf *analysis-results-path
;  (merge-pathnames "trading-core/trading-ui/"
;                   (first ql:*local-project-directories*)))

;; Load historical data and use it to compute "future" price data.
(defparameter *security-data* `((:msft-future . ,(compute-future-data (load-event-data "MSFT" :output-event-type :prc)))
                                (:aapl-future . ,(compute-future-data (load-event-data "AAPL" :output-event-type :prc)))))

(setf *agents*
      (list (make-instance 'adaptive-moving-avg-trend-following
                           :min-period 10 :max-period 55 :width-factor 1.5
                           :snr-factor .5 :security :msft-future)
            (make-instance 'adaptive-moving-avg-trend-following
                           :min-period 10 :max-period 55 :width-factor 1.5
                           :snr-factor .5 :security :aapl-future)
            (make-instance 'swing-mean-reversion
                           :max-allowed-breakout 1.25 :expected-width 5.0
                           :event-count 34 :security :msft-future)
            (make-instance 'swing-mean-reversion
                           :max-allowed-breakout 1.25 :expected-width 5.0
                           :event-count 34 :security :aapl-future)))


(defparameter *events* (sort (copy-list (union (cdr (assoc :msft-future *security-data*))
                                               (cdr (assoc :aapl-future *security-data*))))
                             (lambda (x y)
                               (local-time:timestamp< (timestamp x) (timestamp y)))))

(run-simulation *events*)

(analyze *agents* *security-data*)

;; EOF
