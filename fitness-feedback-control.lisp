;;;; fitness-feedback-control.lisp

(in-package #:trading-core)

(defclass fitness-feedback-control ()
  ((fitness-measure :accessor fitness-measure :initarg :fitness-measure
                     :type (member :rolling-trade-nav-fitness :rolling-profit-loss-fitness
                                   :rolling-profit-factor-fitness :nav-path-length-fitness :price-path-length-fitness
                                   :relative-path-length-fitness)
                     :initform :rolling-profit-loss-fitness)
   (ffc-window-length :accessor ffc-window-length :initarg :ffc-window-length :initform 20)
   (ffc-smoothing-period :accessor ffc-smoothing-period :initarg :ffc-smoothing-period :initform 200)
   (ffc-threshold-percent :accessor ffc-threshold-percent :initarg :ffc-threshold-percent :initform -0.04
     :documentation "The positive-offset from the fitness average used to rebase the fitness value.")
   (ffc-live-threshold :accessor ffc-live-threshold :initarg :ffc-live-threshold :initform 0
     :documentation "The fitness level above which the FFC controller switches an agent live.")
   (ffc-offline-threshold :accessor ffc-offline-threshold :initarg :ffc-offline-threshold :initform 0
     :documentation "The fitness level at or below which the FFC controller switches an agent off-line.")
   (ffc-state :accessor ffc-state :type (member :live :offline) :initform :offline)
   ;; Internal slots
   (nav-ema :accessor nav-ema :type 'exponential-moving-average)
   (pl-ema :accessor pl-ema :type 'exponential-moving-average)
   (gross-profit-ema :accessor gross-profit-ema :type 'exponential-moving-average)
   (gross-loss-ema :accessor gross-loss-ema :type 'exponential-moving-average)
   (initialized :initform nil))
  (:documentation "Class to allow trading control of an agent based on the agent's equity curve.
                   The fitness may be calculated by a variety of methods, as appropriate to the
                   type of trading strategy (trend following => rolling-profit-loss-fitness,
                                             mean reversion => rolling-trade-nav-fitness, etc.)."))

(defmethod initialize-instance :after ((ffc fitness-feedback-control) &key)
  "Initialize the Fitness Feedback Control specific parameters/calculations."
  (with-slots (ffc-live-threshold ffc-offline-threshold ffc-smoothing-period
               nav-ema pl-ema gross-profit-ema gross-loss-ema) ffc
    (assert (>= ffc-live-threshold ffc-offline-threshold))
    (setf nav-ema (make-instance 'exponential-moving-average
                                 :period ffc-smoothing-period)
          pl-ema (make-instance 'exponential-moving-average
                                :period ffc-smoothing-period)
          gross-profit-ema (make-instance 'exponential-moving-average
                                          :period ffc-smoothing-period)
          gross-loss-ema (make-instance 'exponential-moving-average
                                        :period ffc-smoothing-period))))

(defmethod compute-fitness-feedback ((a agent) (ffc fitness-feedback-control))
  "Determine if the specified strategy agent should be traded, based on the equity curve
of the underlying strategy."
  (with-slots (nav-ema pl-ema gross-profit-ema gross-loss-ema initialized fitness-measure
               ffc-live-threshold ffc-offline-threshold ffc-state) ffc
    (update-indicator nav-ema (first (navs a)))
    (update-indicator pl-ema (first (revalprices a)))
    (loop for trade-group in (trade-groups a)
        when (> (trade-group-pl trade-group) 0) summing (trade-group-pl trade-group) into gross-profit
          else summing (trade-group-pl trade-group) into gross-loss
        finally (progn
                  (update-indicator gross-profit-ema gross-profit)
                  (update-indicator gross-loss-ema gross-loss)))
    (if (initialized ffc)
      (let ((fitness-level (ecase fitness-measure
                             (:rolling-trade-nav-fitness (rolling-trade-net-asset-value-fitness a ffc))
                             (:rolling-profit-loss-fitness (rolling-profit-loss-fitness a ffc))
                             (:rolling-profit-factor-fitness (rolling-profit-factor-fitness ffc))
                             (:nav-path-length-fitness
                               (path-length-fitness a :nav (ffc-window-length ffc)))
                             (:price-path-length-fitness
                               (path-length-fitness a :price (ffc-window-length ffc)))
                             (:relative-path-length-fitness
                               (relative-path-length-fitness a (ffc-window-length ffc))))))
        (setf ffc-state
              (cond ((and (eql ffc-state :offline)
                          (> fitness-level ffc-live-threshold)) :live)
                    ((and (eql ffc-state :live)
                          (<= fitness-level ffc-offline-threshold)) :offline)
                    (t :off-line))))
      (progn
        (setf initialized (and (initialized nav-ema) (initialized pl-ema)
                               (initialized gross-profit-ema) (initialized gross-loss-ema)))
        :offline))))

(defun rolling-trade-net-asset-value-fitness (a ffc)
  "Calculation used to judge the fitness of a means-regression type trading strategy. The
threshold value bumps the moving average up to force the fitness negative when the NAV
is losing momentum."
  (let ((current-nav (first (navs a))))
    (- current-nav (* (1+ (ffc-threshold-percent ffc)) (value (nav-ema ffc))))))

(defun rolling-profit-loss-fitness (a ffc)
  "Calculation used to judge the fitness of a trend-following or longer term strategy. The
THRESHOLD-PERCENT band above the moving average to force the fitness negative when the profit/loss
is losing momentum."
  (let ((current-pl (first (pls a))))
    (- current-pl (* (1+ (ffc-threshold-percent ffc)) (value (pl-ema ffc))))))

(defun rolling-profit-factor-fitness (ffc)
  "Calculate the fitness of a trading agent using a rolling window of the ratio of the system gross profit and loss."
  (/ (gross-profit-ema ffc)
     (gross-loss-ema ffc)))

(defun path-length-fitness (a path-type window-length)
  "Measures the deviation of the NAV from a straight line benchmark with the same endpoints
but also taking account of the average return over the period so low or negative return is
penalized."
  (assert (member path-type '(:nav :price)))
  (let* ((data-series (if (eql path-type :nav) (navs a) (revalprices a)))
         (current-nav (first data-series))
         (initial-nav (or (nth (1- window-length) data-series)
                          (last data-series))))
    (* (expt (- (/ current-nav initial-nav) 1) (/ window-length))
       (/ (sqrt (+ (expt window-length 2) (expt (- current-nav initial-nav) 2)))
          (path-length data-series window-length)))))

(defun relative-path-length-fitness (a window-length)
  "Measures the performance of a strategy relative to a long-only or short-only benchmark (index)."
  (- (path-length-fitness a :nav window-length))
     (abs (path-length-fitness a :price window-length)))

(defun path-length (series window-length)
  "Calculate the path length of the values in the trading window."
  (loop repeat window-length
        for (value prev-value) on series
        summing (abs (- value (or prev-value value)))))

;; EOF
