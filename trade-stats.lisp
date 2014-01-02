;;;; trade.lisp

(in-package #:trading-core)

(defstruct trade-stats
  percent-profitable
  win-to-loss
  average-logret
  total-pl
  average-duration
  pos-pl
  neg-pl
  profit-factor
  rolling-pl
  trades)

;;; Trade statistics calculations

(defun rc-integrate (rc-vector)
  "Calcalate the reverse chronological integration of the RC-VECTOR."
  (loop for v in (reverse rc-vector)
        summing v into integral
        collecting integral))

(defun compute-tradestats (trades-list &key (reverse-chron t))
  "Compute the trade statistics for the specified list of trades."
  (let* ((trades (if reverse-chron (reverse trades-list) trades-list))
         (num-trades-groups 0)                ;; multiple times so the end can be detected.
         (trades-groups-list '())
         (trades-groups-stats-list '())
         (trade-stats nil))
    (loop with current-group = nil
          with new-position = 0
          for old-position = 0 then new-position
          for new-trade in trades
          for dummy-trade-1 = nil
          for dummy-trade-2 = nil
          do (progn
               (setf new-position (+ old-position (trade-quantity new-trade)))
               (if (zerop new-position)
                 (progn
                   (push new-trade current-group)
                   (push (reverse current-group) trades-groups-list))
                 (setf current-group nil))
               (if (< (* old-position new-position) 0)
                 (progn
                   (setf dummy-trade-1 (make-trade
                                         :timestamp (trade-timestamp new-trade)
                                         :quantity (- old-position)
                                         :price (trade-price new-trade)
                                         :description "Dummy1")
                         dummy-trade-2 (make-trade
                                         :timestamp (trade-timestamp new-trade)
                                         :quantity new-position
                                         :price (trade-price new-trade)
                                         :description "Dummy2"))
                   (push dummy-trade-1 current-group)
                   (push (reverse current-group) trades-groups-list)
                   (setf current-group (list dummy-trade-2)))
                 (push new-trade current-group)))
          finally (progn
                    (setf dummy-trade-2 (make-trade
                                          :timestamp (trade-timestamp new-trade)
                                          :quantity (- new-position)
                                          :price (trade-price new-trade)
                                          :description "Dummy2"))
                    (push dummy-trade-2 current-group)
                    (push (reverse current-group) trades-groups-list)
                    (setf trades-groups-list (reverse trades-groups-list))))
    (setf num-trades-groups (length trades-groups-list))
    (loop for trades-group in trades-groups-list
          for i from 0
          for buys = (remove-if (lambda (x) (< (trade-quantity x) 0)) trades-group)
          for sells = (remove-if (lambda (x) (> (trade-quantity x) 0)) trades-group)
          for avg-buy-price = (avg-list buys #'trade-price)
          for avg-sell-price = (avg-list sells #'trade-price)
          for avg-buy-index = (avg-list buys #'trade-timestamp)
          for avg-sell-index = (avg-list sells #'trade-timestamp)
          for trade-length = (abs (- avg-buy-index avg-sell-index))
          for trade-logret = (log (if (or (<= avg-buy-price *epsilon*) (<= avg-sell-price *epsilon*))
                                    1
                                    (/ avg-sell-price avg-buy-price)))
          for trade-pl = (reduce #'+ trades-group :key (lambda (x)
                                                         (- (* (trade-price x) (trade-quantity x)))))
          do (progn
               (format t "TRADES GROUP ~A : ~A ~%" i trades-group)
               (push (list trade-length trade-logret trade-pl) trades-groups-stats-list)
               (format t "  ~A ~A ~A~%" trade-length trade-logret trade-pl))
          finally (setf trades-groups-stats-list (reverse trades-groups-stats-list)))
      (loop for (trade-length trade-logret trade-pl) in trades-groups-stats-list
            for trade-count = 1 then (1+ trade-count)
            counting (>= trade-pl 0) into profitable-count
            summing (max trade-pl 0) into pos-pl
            summing (max (- trade-pl) 0) into neg-pl
            summing trade-pl into total-pl
            summing trade-logret into total-logret
            summing trade-length into total-length
            finally (setf trade-stats
                          (make-trade-stats
                            :percent-profitable (/ profitable-count trade-count)
                            :win-to-loss        (if (<= neg-pl *epsilon*) 100 (/ pos-pl neg-pl))
                            :average-logret     (/ total-logret trade-count)
                            :total-pl           total-pl
                            :average-duration   (/ total-length trade-count)
                            :pos-pl             pos-pl
                            :neg-pl             neg-pl
                            :profit-factor      (if (<= (+ pos-pl neg-pl) *epsilon*)
                                                  0
                                                  (/ (- pos-pl neg-pl) (+ pos-pl neg-pl)))
                            :trades             trades-groups-list
                            :rolling-pl         nil ;; TODO - Calculate the rolling annual PL loss value
                            )))
      (format t "new trade-stats ~S~%" trade-stats)
      trade-stats))

(defun trade-stats-plot (trade-stats)
  "Generate a web page that displays the trading results."
  ;; TODO - Use HighCharts/HighStock JS charting software to display PL charts
  nil)

(defmethod graph-stats ((a agent) what)
  (trade-stats-plot (timestamps a)
                    (case what
                      (:cpl (rc-integrate (trunc (pls a)))) ; cumulative profit/loss
                      (:fit (trunc (fitnesses a)))
                      (:prc (revalprices a))
                      (:pls (trunc (pls a)))
                      (:pos (positions a)))))

(defmethod graph-trade-stats ((a agent) what)
  (with-slots (trades trade-stats) a
    (trade-stats-plot (mapcar #'trade-timestamp trades)
                    (case what
                      (:tpl (mapcar #'trade-stat-tot-pl trade-stats))
                      (:lrt (mapcar #'trade-stat-average-logret trade-stats))
                      (:prc (mapcar #'trade-price trades))
                      (:qnt (mapcar #'trade-quantity trades))))))

;; EOF
