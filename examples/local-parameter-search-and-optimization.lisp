;;;; parameters-search-and-optimization.lisp

;;;; !!! WARNING !!!
;;;; Loading this file will result in extreme memory use and long running times while the
;;;; simulation is running.  This could result in your computer running out of memory and
;;;; execution slowing to a crawl as virtual memory is swapped out. Ways to mitigate this
;;;; to some extent by eliminating unnecessary calculations and the storage of values that
;;;; not used are under way but are not included in this source code yet.

(in-package #:trading-core)

(setf logv:*log-output* nil)

;; Set the location of the historical data used in the simulation if different from the default.
(setf *historical-data-path* "C:/Worden/TeleChart/Export/SP500_Components/"
      ;; "C:/Worden/TeleChart/Export/Copernicus_Mostly_USA/"
      )

;; Set the location where the analysis result template is located if different from default
;(setf *ui-template-path* #P"C:/Users/Jonathan/Lisp/projects/trading-core/trading-ui/templates/")

;; Set the location where the analysis results will be placed if different from default
;(setf *analysis-results-path* #P"C:/Users/Jonathan/Lisp/projects/trading-core/trading-ui/")

(format t "Loading event data...")

;; load historical data
(defparameter *security-data* `((:aapl . ,(load-event-data "AAPL"
                                                           :start-date "1996-01-01"
                                                           :end-date   "2005-01-01"))))

;; initialize a set of agents spanning the range of parameters desired
(setf *agents* nil)

(format t "~&Instantiating trading agents...")

(loop for slow-channel = 21 then (+ slow-channel (max 3 (floor (* slow-channel .3))))
      until (> slow-channel 300)
      do (loop for fast-channel = 5 then (+ fast-channel (max 1 (floor (/ slow-channel 10))))
               until (>= fast-channel (* slow-channel 3/4))
               do (push (make-instance 'channel-breakout-trend-following
                                       :fast-period fast-channel :slow-period slow-channel
                                       :security :aapl)
                        *agents*)))

(format t "~&Sorting event data...")

;; create a list of all events in datetime order for the simulation engine
(defparameter *events* (sort (copy-seq (cdr (assoc :aapl *security-data*)))
                             (lambda (x y)
                               (local-time:timestamp< (timestamp x) (timestamp y)))))

(format t "~&Running simulation...")

(time (run-simulation *events*))

(format t "~&Creating comparison page...")

;; find the top performing subset of agents for a particular trade statistic
;; and output the result
(time (multiple-value-bind (bins agent-clusters)
        (cluster-agents *agents* (ceiling (length *agents*) 30) :tpl) ; total profit-loss
        (let ((clusters (pairlis bins agent-clusters)))
          (compare-results (rest (reduce (lambda (a b)
                                           (if (> (caar a) (caar b)) a b))
                                         clusters))
                           "CBTF Parameter Search"
                           *security-data*))))

;; EOF
