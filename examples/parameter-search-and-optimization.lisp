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
(setf *historical-data-path* "C:/Worden/TeleChart/Export/Copernicus_Mostly_USA/")

;; Set the location where the analysis result template is located if different from default
;(setf *ui-template-path* #P"C:/Users/Jonathan/Lisp/projects/trading-core/trading-ui/templates/")

;; Set the location where the analysis results will be placed if different from default
;(setf *analysis-results-path* #P"C:/Users/Jonathan/Lisp/projects/trading-core/trading-ui/")

;; load historical data
(defparameter *security-data* `((:spy . ,(load-event-data "SPY" :data-format :bar
                                                          :start-date 19950101
                                                          :end-date 20050101))))

;; initialize a set of agents spanning the range of parameters desired
(setf *agents* nil)
(loop for snr-factor from .5 upto 1.1 by .2
      do (loop for width-factor from .5 below 1.5 by .2
               do (loop for n-max = 10 then (+ n-max (floor (* n-max .4)))
                        until (> n-max 20)
                        do (loop for n-min = 2 then (+ n-min (floor (/ n-max 5)))
                                 until (>= n-min (* n-max 3/4))
                                 do (push (make-instance 'adaptive-moving-avg-trend-following
                                                         :snr-factor snr-factor :security :spy
                                                         :n-min n-min :n-max n-max
                                                         :width-factor width-factor)
                                          *agents*)))))

;; create a list of all events in datetime order for the simulation engine
(defparameter *events* (sort (copy-seq (cdr (assoc :spy *security-data*)))
                             (lambda (x y)
                               (< (timestamp x) (timestamp y)))))

(run-simulation *events*)

;; find the top performing subset of agents for a particular trade statistic
;; and output the result
(multiple-value-bind (bins agent-clusters)
  (cluster-agents *agents* 10 :tpl) ; total profit-loss
  (let ((clusters (pairlis bins agent-clusters)))
    (analyze (cdr (reduce (lambda (ca cb)
                            (if (> (caar ca) (caar cb)) ca cb))
                          clusters))
             *security-data*)))

;; EOF
