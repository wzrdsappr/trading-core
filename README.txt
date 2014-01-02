This project implements the trading platform discussed in the book:
Professional Automated Trading: Theory and Practice by Eugene A. Durenard.

Copyright (c) 2013, Jonathan J. Lee
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met:

    Redistributions of source code must retain the above copyright notice, this list of conditions and
    the following disclaimer. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation and/or other materials
    provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Example simulation

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

;; Parameter Search and Optimization
(dotimes (x 100)
  (push (make-instance 'simple-model
                       :L (+ i 10))
        *agents*))

(run-simulation (load-event-data "SPY"))

(let ((results nil))
  (dolist (a *agents*)
    (push (list (L a)
                (trade-stat-tot-pl (first (tradestats a))))
          results))
  results)

