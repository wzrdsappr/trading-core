trading-core
============

Description
-----------
Backend core needed for automated trading

This project implements the trading platform discussed in the book:
Professional Automated Trading: Theory and Practice by Eugene A. Durenard.

Loading the project
-------------------
To load this project, use QuickLisp.

```lisp
(ql:quickload "trading-core")
```

### Examples
A simple back testing example is listed below.  More example scripts can be
found in the *examples* folder

```lisp
;; load historical data
(defparameter *security-data*
  `((:msft . ,(load-event-data "MSFT" :data-format :bar 
                               :start-date 199201010000 :end-date 201201010000))
    (:aapl . ,(load-event-data "AAPL" :data-format :bar 
                               :start-date 199201010000 :end-date 201201010000))))

;; create the trading agents that will process the historical data
(setf *agents*
      (list (make-instance 'adaptive-moving-avg-trend-following
                           :n-min 11 :n-max 21 :width-factor 1.5
                           :snr-factor .9 :security :msft)
            (make-instance 'adaptive-moving-avg-trend-following
                           :n-min 11 :n-max 21 :width-factor 1.2
                           :snr-factor .9 :security :msft)
            (make-instance 'adaptive-moving-avg-trend-following
                           :n-min 11 :n-max 21 :width-factor 1.5
                           :snr-factor .9 :security :aapl)
            (make-instance 'adaptive-moving-avg-trend-following
                           :n-min 11 :n-max 21 :width-factor 1.2
                           :snr-factor .9 :security :aapl)))

;; create a list of all events in datetime order for the simulation engine
(defparameter *events* (sort (copy-list (union (cdr (assoc :msft *security-data*))
                                               (cdr (assoc :aapl *security-data*))))
                             (lambda (x y)
                               (< (timestamp x) (timestamp y)))))

(run-simulation *events*)

(analyze *agents* *security-data*)
```

License
-------
MIT. See "LICENSE".

