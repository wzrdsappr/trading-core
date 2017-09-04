;;;; agent-communication-pair-trading.lisp
;;; A simple example of cooperative communication between agents.

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
  `((:nue . ,(load-event-data "NUE"
                              :start-date "2005-01-01" :end-date "2012-01-01"))
    (:x . ,(load-event-data "X"
                            :start-date "2005-01-01" :end-date "2012-01-01"))))

;; create the trading agents that will process the historical data
(let ((a1 (make-instance 'simple-model-comm
                           :L 89 :security :x))
      (a2 (make-instance 'simple-model-comm
                           :L 89 :security :nue)))
  (setf (recipients-list a1) (list a2)
        (recipients-list a2) (list a1)
        *agents* (list a1 a2)))

;; create a list of all events in datetime order for the simulation engine
(defparameter *events* (sort (copy-list (union (cdr (assoc :nue *security-data*))
                                               (cdr (assoc :x *security-data*))))
                             (lambda (x y)
                               (local-time:timestamp< (timestamp x) (timestamp y)))))

(run-simulation *events*)

(analyze *agents* *security-data*)

;; EOF
