;;;; backtesting-simulation.lisp
;;; An example of backtesting multiple agents on historical data and generating analysis reports.

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

;; load historical data
(defparameter *security-data*
  `((:msft . ,(load-event-data "MSFT"
                               :start-date "1996-01-01" :end-date "2010-01-01"))
    (:aapl . ,(load-event-data "AAPL"
                               :start-date "1996-01-01" :end-date "2010-01-01"))))

;; create the trading agents that will process the historical data
(progn
  (setf *agents*
      (list (make-instance 'fractal-ama-trend-following
                           :max-period 300 :fractal-length 126
                           :security :msft)
            (make-instance 'fractal-ama-trend-following
                           :max-period 200 :fractal-length 50
                           :security :msft)
            (make-instance 'fractal-ama-trend-following
                           :max-period 200 :fractal-length 50
                           :security :msft)
            (make-instance 'fractal-ama-trend-following
                           :max-period 300 :min-period 50 :fractal-length 126
                           :security :msft)
            (make-instance 'fractal-ama-trend-following
                           :max-period 200 :min-period 1 :fractal-length 126
                           :security :msft)
            (make-instance 'fractal-ama-trend-following
                           :max-period 300 :min-period 50 :fractal-length 22
                           :security :msft)
            (make-instance 'fractal-ama-trend-following
                           :max-period 200 :min-period 1 :fractal-length 22
                           :security :msft)
            (make-instance 'channel-breakout-trend-following
                           :fast-period 110 :slow-period 211
                           :security :msft)
            (make-instance 'channel-breakout-trend-following
                           :fast-period 26 :slow-period 211
                           :security :msft)
            (make-instance 'fractal-ama-trend-following
                           :max-period 300 :fractal-length 126
                           :security :aapl)
            (make-instance 'channel-breakout-trend-following
                           :fast-period 110 :slow-period 211
                           :security :aapl)
            (make-instance 'channel-breakout-trend-following
                           :fast-period 26 :slow-period 211
                           :security :aapl)))
#||
  (push (make-instance 'market-direction-filter
                       :period 100 :security :aapl
                       :recipients-list (remove-if-not (lambda (a)
                                                         (eql (security a) :aapl))
                                                       *agents*))
        *agents*)
  (push (make-instance 'market-direction-filter
                       :period 100 :security :msft
                       :recipients-list (remove-if-not (lambda (a)
                                                         (eql (security a) :msft))
                                                       *agents*))
        *agents*)
  ||#
  )

;; create a list of all events in datetime order for the simulation engine
(defparameter *events* (sort (copy-list (union (cdr (assoc :msft *security-data*))
                                               (cdr (assoc :aapl *security-data*))))
                             (lambda (x y)
                               (local-time:timestamp< (timestamp x) (timestamp y)))))

(run-simulation *events*)

(analyze *agents* *security-data*)

;; EOF
