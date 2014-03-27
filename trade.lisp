;;;; trade.lisp

(in-package #:trading-core)

(defstruct trade
  timestamp
  price
  quantity
  description)

;;; Trade calculations

(defun aggregate-trades (trades-list)
  (let* ((trades-list-sorted (sort trades-list (lambda (x y)
                                                 (local-time:timestamp>=
                                                   (trade-timestamp x)
                                                   (trade-timestamp y)))))
         (agg-timestamp (trade-timestamp (first trades-list-sorted))))
    (loop with agg-description = "AGG_"
          for i from 0
          for trade in trades-list-sorted
          for quantity = (trade-quantity trade)
          summing quantity into agg-quantity
          summing (* (trade-price trade) quantity) into weighted-sum
          do (setf agg-description (concatenate agg-description (trade-description trade)))
          finally (return (make-trade
                            :timestamp   agg-timestamp
                            :price       (/ weighted-sum agg-quantity)
                            :quantity    agg-quantity
                            :description agg-description)))))

;; EOF
