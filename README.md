trading-core
============

Description
-----------
Backend core needed for automated trading

This project implements the trading platform discussed in the book:
Professional Automated Trading: Theory and Practice by Eugene A. Durenard.

Change Log
----------
<table>
<tr><td>2013-11-09</td><td>Jonathan Lee</td><td>Created project.</td></tr>
<tr><td>2014-01-01</td><td>Jonathan Lee</td><td>Improved documentation. Added markdown README file for Github repository
default page.</td></tr>
</table>

Loading the project
-------------------
To load this project, use QuickLisp.

```lisp
(ql:quickload "trading-core")
```

### Example simulation

```lisp
(push (make-instance 'adaptive-moving-avg-trend-following
                     :n-min 10 :n-max 60 :width-factor 1.5 :snr-factor 0.2)
      *agents*)
(push (make-instance 'swing-mean-reversion
                     :event-count 10 :expected-width 1.5 :max-allowed-breakout 0.11)
      *agents*)

(defparameter *MSFT-events* (load-event-data "MSFT" :data-format :bar))
(defparameter *AAPL-events* (load-event-data "AAPL" :data-format :bar))

(setf *events* (sort (union *MSFT-events* *AAPL-events*)
                     (lambda (x y)
                       (< (timestamp x) (timestamp y)))))

(run-simulation *events*)

;(analyze *agents*)
;(analyze *aggregate-agents*)
```

### Parameter Search and Optimization

```lisp
(dotimes (x 100)
  (push (make-instance 'simple-model
                       :L (+ i 10))
        *agents*))

(run-simulation (load-event-data "SPY" :data-format :bar))

(let ((results nil))
  (dolist (a *agents*)
    (push (list (L a)
                (trade-stat-tot-pl (first (tradestats a))))
          results))
  results)
```

License
-------
MIT. See "LICENSE".

