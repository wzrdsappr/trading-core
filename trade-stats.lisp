;;;; trade-stats.lisp

(in-package #:trading-core)

;; TODO : Add RAR% (Regressed Annual Return) and R-Cubed (robust risk/reward ratio RRRR)
;;        and Robust Sharpe Ratio stats as defined by Curtis M. Faith in "Way of the Turtle".

(defstruct trade-group
  trades
  entry-timestamp
  exit-timestamp
  duration
  pl
  logret)

(defstruct trade-stats
  timestamp
  percent-profitable
  win-to-loss
  average-logret
  total-pl
  average-duration
  pos-pl
  neg-pl
  profit-factor)

;;; Trade statistics calculations

(defun compute-trade-group (trades)
  "Create a trade-group instance, computing the relevant stats."
  (loop with avg-buy-price = 0
        with avg-sell-price = 0
        with trade-length = 0
        with trade-logret = 0.0D0
        with trade-group = (make-trade-group :trades trades)
        for trade in trades
        for trade-start = (trade-timestamp trade)
          then (local-time:timestamp-minimum (trade-timestamp trade) trade-start)
        for trade-end = (trade-timestamp trade)
          then (local-time:timestamp-maximum (trade-timestamp trade) trade-end)
        counting trade into trade-count
        when (> (trade-quantity trade) 0)
          summing (trade-price trade) into buy-price-sums
          and counting 1 into buy-trades
        when (< (trade-quantity trade) 0)
          summing (trade-price trade) into sell-price-sums
          and counting 1 into sell-trades
        summing (* (trade-price trade) (- (trade-quantity trade))) into trade-pl
        finally (progn
                  (when (> trade-count 0)
                    (setf avg-buy-price (/ buy-price-sums buy-trades)
                          avg-sell-price (/ sell-price-sums sell-trades)
                          trade-length (/ (local-time:timestamp-difference trade-end trade-start)
                                          local-time:+seconds-per-day+)
                          trade-logret (log (if (or (<= avg-buy-price +epsilon+)
                                                    (<= avg-sell-price +epsilon+))
                                              1
                                              (/ avg-sell-price avg-buy-price)))))
                  (setf trade-group
                         (make-trade-group
                           :trades (reverse trades)     ;; store trades in reverse chronological order
                           :entry-timestamp trade-start
                           :exit-timestamp trade-end
                           :duration trade-length
                           :pl trade-pl
                           :logret trade-logret))
                  (log:debug "TRADES GROUP : ~A ~%" trade-group)
                  (return trade-group))))

(defun partition-trades (unprocessed-trades last-timestamp last-price)
  "Group trades by market position. Takes a list of trades in reverse-
chronological order. Returns a reverse-chronological list of trade
groups with relevant stats pre-computed for each group."
  (loop with trade-groups = nil
        with current-group = nil
        with new-position = 0
        for old-position = 0 then new-position
        for new-trade in (reverse unprocessed-trades)
        do (progn
             (setf new-position (+ old-position (trade-quantity new-trade)))
             (if (zerop new-position)
               (progn
                 (push new-trade current-group)
                 (push (compute-trade-group current-group) trade-groups)
                 (setf current-group nil))
               (if (< (* old-position new-position) 0)
                 (progn
                   (push (make-trade
                           :timestamp (trade-timestamp new-trade)
                           :quantity (- old-position)
                           :price (trade-price new-trade)
                           :description "SplitExit")
                         current-group)
                   (push (compute-trade-group current-group) trade-groups)
                   (setf current-group (list (make-trade
                                               :timestamp (trade-timestamp new-trade)
                                               :quantity new-position
                                               :price (trade-price new-trade)
                                               :description "SplitEntry"))))
                 (push new-trade current-group))))
        finally (progn
                  ;; Insert dummy trade to close the current open position, if needed
                  (when (/= new-position 0)
                    (push (make-trade
                            :timestamp last-timestamp
                            :quantity (- new-position)
                            :price last-price
                            :description "DummyExit")
                          current-group)
                    (push (compute-trade-group current-group) trade-groups))
                  (return trade-groups))))

(defmethod compute-trade-stats ((agent agent))
  "Compute the trade statistics for the given agent."
  (with-slots (unprocessed-trades trade-groups-cache) agent
    (let* ((trade-stats (make-trade-stats          ;; blank trade stats for agent without trades.
                          :timestamp          (first (timestamps agent))
                          :percent-profitable 0
                          :win-to-loss        0
                          :average-logret     0
                          :total-pl           0
                          :average-duration   0
                          :pos-pl             0
                          :neg-pl             0
                          :profit-factor      0))
           (trade-groups (trade-groups agent)))
      ;; Partition the trades into trade groups, from entry to exit from the market. Inserts
      ;; dummy trades to close existing positions if the trading position was reversed in one
      ;; trade or a trade is still ongoing.
      (when trade-groups
        (loop for trade-group in trade-groups
              for trade-count from 1
              counting (>= (trade-group-pl trade-group) 0) into profitable-count
              summing (max (trade-group-pl trade-group) 0) into pos-pl
              summing (max (- (trade-group-pl trade-group)) 0) into neg-pl
              summing (trade-group-pl trade-group) into total-pl
              summing (trade-group-logret trade-group) into total-logret
              summing (trade-group-duration trade-group) into total-length
              finally (when trade-count
                        (setf trade-stats
                              (make-trade-stats
                                :timestamp          (first (timestamps agent)) 
                                :percent-profitable (/ profitable-count trade-count)
                                :win-to-loss        (if (<= neg-pl +epsilon+) 100 (/ pos-pl neg-pl))
                                :average-logret     (/ total-logret trade-count)
                                :total-pl           total-pl
                                :average-duration   (/ total-length trade-count)
                                :pos-pl             pos-pl
                                :neg-pl             neg-pl
                                :profit-factor      (if (<= (+ pos-pl neg-pl) +epsilon+)
                                                      0
                                                      (/ (- pos-pl neg-pl) (+ pos-pl neg-pl))))))))
      (log:debug "new trade-stats ~S~%" trade-stats)
      trade-stats)))

(defun rc-integrate (rc-vector)
  "Calcalate the reverse chronological integration of the RC-VECTOR."
  (loop for v in (reverse rc-vector)
        summing v into integral
        collecting integral))

;; EOF
