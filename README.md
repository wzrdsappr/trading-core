trading-core
============

Description
-----------
Backend core needed for automated trading

This project implements the trading platform discussed in the book:
Professional Automated Trading: Theory and Practice by Eugene A. Durenard.

Loading the project
-------------------
To load this project, use QuickLisp.  First, download or clone this repo into the quicklisp local-projects folder.
Alternately, if you have cloned it into a different location, add a symbolic link to its location in the local-projects
folder.

```lisp
(ql:quickload "trading-core")
```

### Examples
A simple back testing example is listed below.  More example scripts can be
found in the *examples* folder

```lisp

(in-package :trading-core)

;; load historical data
(defparameter *security-data*
  `((:msft . ,(load-event-data "MSFT"
                               :start-date "1992-01-01" :end-date "2012-01-01"))
    (:aapl . ,(load-event-data "AAPL"
                               :start-date "1992-01-01" :end-date "2012-01-01"))))

;; create the trading agents that will process the historical data
(setf *agents*
      (list (make-instance 'adaptive-moving-avg-trend-following
                           :min-period 11 :max-period 21 :width-factor 1.5
                           :snr-factor .9 :security :msft)
            (make-instance 'adaptive-moving-avg-trend-following
                           :min-period 11 :max-period 21 :width-factor 1.2
                           :snr-factor .9 :security :msft)
            (make-instance 'adaptive-moving-avg-trend-following
                           :min-period 11 :max-period 21 :width-factor 1.5
                           :snr-factor .9 :security :aapl)
            (make-instance 'adaptive-moving-avg-trend-following
                           :min-period 11 :max-period 21 :width-factor 1.2
                           :snr-factor .9 :security :aapl)))

;; create a list of all events in datetime order for the simulation engine
(defparameter *events* (sort (copy-list (union (cdr (assoc :msft *security-data*))
                                               (cdr (assoc :aapl *security-data*))))
                             (lambda (x y)
                               (local-time:timestamp< (timestamp x) (timestamp y)))))

;; unless you wish to see all of the logging/debugging messages, turn off logging
(log:config :warn)

;; run the simulation
(run-simulation *events*)

(analyze *agents* *security-data*)
```

To using the included example files, load it.

```lisp
;; Use of an example system
(load (merge-pathnames "trading-core/examples/backtesting-simulation.lisp"
                       (first ql:*local-project-directories*)))

```

License
-------
MIT. See "LICENSE".

The generated analysis web pages use Highstocks, which has [its own, separate licensing.](http://shop.highsoft.com/highstock.html)

