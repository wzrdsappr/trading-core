;;;; parameters-search-and-optimization.lisp

;;;; !!! WARNING !!!
;;;; Loading this file will result in extreme memory use and long running times while the
;;;; simulation is running.  This could result in your computer running out of memory and
;;;; execution slowing to a crawl as virtual memory is swapped out. Ways to mitigate this
;;;; to some extent by eliminating unnecessary calculations and the storage of values that
;;;; not used are under way but are not included in this source code yet.

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

(format t "Loading event data...")

;; load historical data
(defparameter *security-data* `((:spy . ,(load-event-data "SPY"
                                                          :start-date "1996-01-01"
                                                          :end-date   "2004-01-01"))))

;; initialize a set of agents spanning the range of parameters desired
(setf *agents* nil)

(format t "~&Instantiating trading agents...")

(loop for snr-factor from .5 upto 3.1 by .3
      do (loop for width-factor from .5 below 4.5 by .3
               do (loop for max-period = 21 then (+ max-period (max 3 (floor (* max-period .3))))
                        until (> max-period 155)
                        do (loop for min-period = 5 then (+ min-period (max 1 (floor (/ max-period 7))))
                                 until (>= min-period (* max-period 3/4))
                                 do (push (make-instance 'adaptive-moving-avg-trend-following
                                                         :snr-factor snr-factor :security :spy
                                                         :min-period min-period :max-period max-period
                                                         :width-factor width-factor)
                                          *agents*)))))

(format t "~&Sorting event data...")

;; create a list of all events in datetime order for the simulation engine
(defparameter *events* (sort (copy-seq (cdr (assoc :spy *security-data*)))
                             (lambda (x y)
                               (local-time:timestamp< (timestamp x) (timestamp y)))))

(format t "~&Running simulation...")

(time (run-simulation *events*))

(format t "~&Creating comparison page...")

;; find the top performing subset of agents for a particular trade statistic
;; and output the result
(time (multiple-value-bind (bins agent-clusters)
        (cluster-agents *agents* 20 :tpl) ; total profit-loss
        (let ((clusters (pairlis bins agent-clusters)))
          (compare-results (rest (reduce (lambda (a b)
                                           (if (> (caar a) (caar b)) a b))
                                         clusters))
                           "Adaptive-Moving-Avg-Trend-Following Parameter Search"
                           *security-data*))))

;; EOF
