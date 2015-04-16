;;; indicators.lisp
;;;
;;; Standalone indicators that can be used in any trading system simply by
;;; creating an instance in the agent's INITIALIZE method and storing it in
;;; a slot in the agent class.  The indicator values are updated by calling
;;; the UPDATE-VALUE method for each indicator in the agent's PREPROCESS method.
;;; When the indicator contains enough data to make the required calculations,
;;; the INITIALIZED boolean is set. The indicator value is accessed by the VALUE
;;; reader.

(in-package #:trading-core)
(named-readtables:in-readtable rutils-readtable)


(defclass indicator ()
  ((value :reader value :initform nil)
   (initialized :reader initialized :initform 'nil))
  (:documentation
    "Base class for indicator that can be used in any trading system simply by
creating an instance in the agent's INITIALIZE method and storing it in
a slot in the agent class.  The indicator values are updated by calling
the UPDATE-VALUE method for each indicator in the agent's PREPROCESS method.
When the indicator contains enough data to make the required calculations,
the INITIALIZED boolean is set. The indicator value is accessed by the VALUE
reader."))

(defclass simple-moving-average (indicator)
  ((period :reader period :initarg :period :type (integer 0))
   (prices :type circbuf:circular-buffer))
  (:default-initargs :period 20))

(defclass exponential-moving-average (indicator)
  ((period :accessor period :initarg :period :type (integer 0))
   (initializing-periods :type (integer 0) :initform 0)
   (factor :type double-float :initform 0.0D0)))

(defclass adaptive-moving-average (indicator)
  ((min-period :initarg :min-period :type (integer 0)
     :documentation "Minimum number of averaging periods of the adaptive moving average.")
   (max-period :initarg :max-period :type (integer 0)
     :documentation "Maximum number of averaging periods of the adaptive moving average.")
   (width-factor :initarg :width-factor
     :documentation "Factor used to calculate the channel width.")
   (snr-factor :initarg :snr-factor
     :documentation "Adjustment factor used to adjust the speed the adaptive moving average swings
between the minimum and maximum averaging periods.")
   (initializing-periods :type (integer 0) :initform 0)
   (min-factor :initform 0.0
     :documentation "Exponential factor equivalent to the specified minimum periods.")
   (max-factor :initform 1.0
     :documentation "Exponential factor equivalent to the specified maximum periods.")
   (factor :initform 0.5
     :documentation "Adjustable exponential factor used to calculate the moving average")
   (deviation+ :reader deviation+ :initform 0
     :documentation "Average positive price deviations")
   (deviation- :reader deviation- :initform 0
     :documentation "Average negative price deviations")
   (upper-band :accessor upper-band :initform nil
     :documentation "Upper channel value, used internally to adjust the AMA but also
available for trading decisions.")
   (lower-band :accessor lower-band :initform nil
     :documentation "Lower channel value, used internally to adjust the AMA but also
available for trading decisions.")
   (snr :initform 0
     :documentation "Signal-to-noise ratio value used to adjust the factor")
   (prev-price :initform nil))
  (:documentation "Adaptive moving average from Professional Automated Trading by Eugene
A. Durenard."))

(defclass fractal-adaptive-moving-average (indicator)
  ((min-period :initarg :min-period :initform 4
     :documentation "Maximum averaging period.")
   (max-period :initarg :max-period :initform 300
     :documentation "Maximum averaging period.")
   (fractal-period :initarg :fractal-length :initform 126
     :documentation "Price periods over which to calculate the fractal dimension.")
   (max-factor
     :documentation "Exponential factor equivalent to the specified maximum periods.")
   (log-max-factor
     :documentation "Log of the MAX-FACTOR.")
   (prices :type circbuf:circular-buffer))
  (:documentation "Fractal Adaptive Moving Average (FRAMA) invented by John F Ehlers.  Modified
to allow user selection of the maximum averaging period instead of the fixed value
equivalent to 198 periods built into the original equation.  Default values are based
on \"optimal\" settings found after testing by ETF HQ (http://etfhq.com/blog/2010/10/
09/frama-is-it-effective/), although they report that the original values were also
very effective."))

(defclass moving-linear-regression (indicator)
  ((period :accessor period :initarg :period :type (integer 0))
   (factors :accessor factors :initarg :factors :initform nil)
   (factor-divisor :accessor factor-divisor :initarg :factor-divisor :initform nil)
   (prices :type circbuf:circular-buffer))
  (:documentation "Linear regression curve defined by the rolling end-point values
of PERIOD length linear regressions."))

(defclass channel (indicator)
  ((center-indicator :type indicator :initarg :center-indicator)
   (channel-width-indicator :type indicator :initarg :channel-width-indicator)
   (width-multiplier :initarg :width-multiplier :initform 1.0)
   (upper-band :reader upper-band :initform nil)
   (lower-band :reader lower-band :initform nil))
  (:documentation "Center and width-based channel. Upper and lower bands are offset
from the center value by 1/2 of the calculated width."))

(defclass centerless-channel (indicator)
  ((upper-band-indicator :type function :initarg upper-band-indicator)
   (lower-band-indicator :type function :initarg lower-band-indicator)
   (upper-band :reader upper-band :initform nil)
   (lower-band :reader lower-band :initform nil))
  (:documentation "Channel where the upper and lower bands are calculated directly, rather
than as widths. Center value is available as the value 1/2 way between the two bands."))

(defclass asymmetric-channel (indicator)
  ((center-indicator :type indicator :initarg :center-indicator)
   (upper-band-width-indicator :type function :initarg upper-band-width-indicator)
   (lower-band-width-indicator :type function :initarg lower-band-width-indicator)
   (upper-band :reader upper-band :initform nil)
   (lower-band :reader lower-band :initform nil))
  (:documentation "Width-based asymmetric channel. Upper and lower band widths are calculated
independently."))

(defclass donchian-channel (indicator)
  ((period :accessor period :initarg :period)
   (offset :accessor offset :initarg :offset :initform 1)
   (prices :type circbuf:circular-buffer)
   (upper-band :reader upper-band :initform nil)
   (lower-band :reader lower-band :initform nil))
  (:documentation "Channel indicator developed by Richard Donchian. It is formed by taking the
highest-high and lowest-low of the last n periods."))

(defclass average-true-range (indicator)
  ((period :reader period :initarg :period :type (integer 0))
   (smoothing-type :initarg :smoothing-type :type (member :ema :sma))
   (value-type :initarg :value-type :type (member :absolute :percent))
   (factor :type double-float :initform 0.0D0)
   (tr-buffer :type circbuf:circular-buffer)
   (prev-price :initform nil))
  (:default-initargs :smoothing-type :ema :value-type :absolute))

(defclass parabolic-sar (indicator)
  ((af-step :initarg :af-step
     :documentation "Acceleration Factor step size.")
   (af-step-max :initarg :af-step-max
     :documentation "Maximum acceleration factor step size.")
   (state :type (member :init :long :short) :initform :init)
   (prices :type circbuf:circular-buffer)
   (acceleration-factor :initform nil)
   extreme)
  (:documentation "Parabolic Stop-and-Reverse indicator developed by Welles Wilder.")
  (:default-initargs :af-step 0.02 :af-step-max 0.20))

;; indicator initialization methods

(defmethod initialize-instance :after ((i simple-moving-average) &key)
  (with-slots (period prices) i
    (setf prices (make-instance 'circbuf:circular-buffer
                                :size period))))

(defmethod initialize-instance :after ((i exponential-moving-average) &key)
  (with-slots (period factor) i
    (setf factor (coerce (/ 2 (1+ period)) 'double-float))))

(defmethod initialize-instance :after ((i fractal-adaptive-moving-average) &key)
  (with-slots (min-period max-period fractal-period max-factor log-max-factor
               value prices) i
    (assert (< min-period max-period))
    (assert (evenp fractal-period))
    (setf max-factor (/ 2 (1+ max-period))
          log-max-factor (log max-factor)
          value 0.0
          prices (make-instance 'circbuf:circular-buffer
                                :size fractal-period))))

(defmethod initialize-instance :after ((i adaptive-moving-average) &key)
  (with-slots (min-period max-period min-factor max-factor factor) i
    (assert (< min-period max-period))
    (setf min-factor (/ 2 (1+ min-period))
          max-factor (/ 2 (1+ max-period))
          factor (+ min-factor (/ (- max-factor min-factor) 2)))))

(defmethod initialize-instance :after ((i moving-linear-regression) &key)
  (with-slots (period prices factors factor-divisor) i
    (setf prices (make-instance 'circbuf:circular-buffer
                                :size period))
    ;; Generate the factors needed to calculate the final value of the linear regression
    (loop with starting-factor = (/ (- period 1) 2)
          for factor = starting-factor then (- factor 1.0)
          until (< factor (- starting-factor))
          collecting factor into factor-list
          summing (expt factor 2) into factor-square-sum
          finally (setf factors (coerce factor-list 'vector)
                        factor-divisor factor-square-sum))))

(defmethod initialize-instance :after ((i donchian-channel) &key)
  (with-slots (period offset prices) i
    (setf prices (make-instance 'circbuf:circular-buffer
                                :size (+ period offset)))))

(defmethod initialize-instance :after ((i average-true-range) &key)
  (with-slots (period factor prev-price tr-buffer) i
    (setf factor (coerce (/ 2 (1+ period)) 'double-float)
          tr-buffer (make-instance 'circbuf:circular-buffer
                                   :size period))))

(defmethod initialize-instance :after ((i parabolic-sar) &key)
  (with-slots (prices) i
    (setf prices (make-instance 'circbuf:circular-buffer
                                :size 2 :granularity 4))))

;; indicator update methods

(defmethod update-indicator ((i simple-moving-average) price)
  (with-slots (value initialized prices period) i
    (circbuf:cb-push price prices)
    (when (and (not initialized)
               (= (circbuf:cb-size prices) period))
        (setf initialized t))
    (if initialized
      (let ((price-sum 0))
        (circbuf:do-circular-buffer (v prices)
          (incf price-sum v))
        (setf value (/ price-sum period)))
      (setf value price))))

(defmethod update-indicator ((i exponential-moving-average) price)
  (with-slots (initialized initializing-periods period factor value) i
    (when (and (not initialized)
               (= (incf initializing-periods) period))
        (setf initialized t))
    (setf value (+ (* factor price) (* (- 1 factor) (or value price))))))

(defmethod update-indicator ((i fractal-adaptive-moving-average) price)
  (let ((price-bar (make-instance 'bar
                                  :timestamp 0.0D0
                                  :security :frama
                                  :value (list price price price price))))
    (update-indicator i price-bar)))

(defmethod update-indicator ((i fractal-adaptive-moving-average) (e bar))
  (with-slots (initialized min-period max-period fractal-period prices
               max-factor log-max-factor value) i
    (circbuf:cb-push e prices)
    (when (and (not initialized)
               (= (circbuf:cb-size prices) fractal-period))
      (setf initialized t))
    (let ((half-fractal-period (/ fractal-period 2))
          (i 0)
          (high1 -999999) (low1 999999)
          (high2 -999999) (low2 999999)
          (high3 -999999) (low3 999999)
          (n1 0) (n2 0) (n3 0)        ;; slopes across three overlapping time ranges
          dimension                   ;; Fractal dimension of the prices (ranges from 1-2).
          unscaled-factor
          unscaled-period             ;; Adaptive EMA periods with range [1, MAX-PERIOD], before scaling
          ;; to account for the desired range of [MIN-PERIOD, MAX-PERIOD].
          (factor max-factor))                     ;; Final, scaled EMA factor
      (when initialized
        (circbuf:do-circular-buffer (p prices)
                                    (when (< i half-fractal-period)
                                      (when (> (h p) high1) (setf high1 (h p)))
                                      (when (< (l p) low1) (setf low1 (l p))))
                                    (when (>= i half-fractal-period)
                                      (when (> (h p) high2) (setf high2 (h p)))
                                      (when (< (l p) low2) (setf low2 (l p))))
                                    (when (> (h p) high3) (setf high3 (h p)))
                                    (when (< (l p) low3) (setf low3 (l p)))
                                    (incf i))
        (setf n1 (/ (- high1 low1) half-fractal-period)
              n2 (/ (- high2 low2) half-fractal-period)
              n3 (/ (- high3 low3) fractal-period)
              dimension (max (min (realpart (/ (- (log (+ n1 n2)) (log n3)) (log 2))) 2) 1)
              unscaled-factor (exp (* log-max-factor (- dimension 1)))
              unscaled-period (/ (- 2 unscaled-factor) unscaled-factor)
              factor (min (max (/ 2 (1+ (+ (* (- max-period min-period)
                                              (/ (1- unscaled-period) (1- max-period)))
                                           min-period)))
                               max-factor)
                          1)))
      (setf value (+ (* factor (price e)) (* (- 1 factor) value))))))

(defmethod update-indicator ((i adaptive-moving-average) price)
  (with-slots (initialized initializing-periods width-factor max-period max-factor min-factor
               factor deviation+ deviation- upper-band lower-band snr-factor prev-price value) i
    (when (and (not initialized)
               (= (incf initializing-periods) max-period))
      (setf initialized t))
    (when (null prev-price)
      (setf value price
            upper-band price
            lower-band price
            prev-price price))
    (let ((prev-value (or value price))
          (prev-d+ deviation+)
          (prev-d- deviation-)
          scale-factor+
          scale-factor-
          (pi/2 1.5707963267948966D0)
          snr                  ;; price signal-to-noise ratio
          normalized-atan)     ;; atan normalized to range of [0, 1] instead of [-1.5, 1.5]
      (setf deviation+ (+ (* factor (max (/ (- price prev-price) prev-price) 0))
                          (* (- 1 factor) deviation+))
            deviation- (+ (* factor -1 (min (/ (- price prev-price) prev-price) 0))
                          (* (- 1 factor) deviation-))
            value (+ (* factor price) (* (- 1 factor) value))
            scale-factor- (* width-factor prev-d-)
            scale-factor+ (* width-factor prev-d+)
            upper-band (* (+ 1 scale-factor-) value)
            lower-band (* (- 1 scale-factor+) value)
            snr (cond ((and (> price upper-band) (/= scale-factor- 0))
                       (/ (- price prev-value) (* prev-value scale-factor-)))
                      ((and (< price lower-band) (/= scale-factor+ 0))
                       (/ (* -1 (- price prev-value)) (* prev-value scale-factor+)))
                      (t 0))
            normalized-atan (/ (+ (atan (* snr-factor snr)) pi/2) (* 2 pi/2))
            factor (+ max-factor (* (- min-factor max-factor)
                                    normalized-atan))
            prev-price price))))

(defmethod update-indicator ((i moving-linear-regression) price)
  (with-slots (value initialized prices period factors factor-divisor) i
    (circbuf:cb-push price prices)
    (when (and (not initialized)
               (= (circbuf:cb-size prices) period))
      (setf initialized t))
    (let ((idx 0)
          (price-sum 0)
          (weighted-sum 0))
      (circbuf:do-circular-buffer (p prices)
        (incf weighted-sum (* (aref factors idx) p))
        (incf price-sum p)
        (incf idx))
      (setf value (+ (/ price-sum period)
                     (/ (* (aref factors 0) weighted-sum)
                        factor-divisor))))))

(defmethod update-indicator ((i channel) price)
  (with-slots (initialized center-indicator channel-width-indicator
               width-multiplier value upper-band lower-band) i
    (update-indicator center-indicator price)
    (update-indicator channel-width-indicator price)
    (when (and (not initialized)
               (initialized center-indicator)
               (initialized channel-width-indicator))
      (setf initialized t))
    (let ((half-width (/ (* (value channel-width-indicator) width-multiplier) 2)))
      (setf value (value center-indicator)
            upper-band (+ (value center-indicator) half-width)
            lower-band (- (value center-indicator) half-width)))))

(defmethod update-indicator ((i centerless-channel) price)
  (with-slots (initialized upper-band-indicator
               lower-band-indicator value upper-band lower-band) i
    (update-indicator upper-band-indicator price)
    (update-indicator lower-band-indicator price)
    (when (and (not initialized)
               (initialized upper-band-indicator)
               (initialized lower-band-indicator))
      (setf initialized t))
    (setf value (/ (+ (value upper-band-indicator) (value lower-band-indicator)) 2)
          upper-band (value upper-band-indicator)
          lower-band (value lower-band-indicator))))

(defmethod update-indicator ((i asymmetric-channel) price)
  (with-slots (initialized center-indicator upper-band-width-indicator
               lower-band-width-indicator value upper-band lower-band) i
    (update-indicator center-indicator price)
    (update-indicator upper-band-width-indicator price)
    (update-indicator lower-band-width-indicator price)
    (when (and (not initialized)
               (initialized center-indicator)
               (initialized upper-band-width-indicator)
               (initialized lower-band-width-indicator))
      (setf initialized t))
    (setf value (value center-indicator)
          upper-band (+ (value center-indicator) (value upper-band-width-indicator))
          lower-band (- (value center-indicator) (value lower-band-width-indicator)))))

(defmethod update-indicator ((i donchian-channel) price)
  (with-slots (initialized prices period offset value upper-band lower-band) i
    (circbuf:cb-push price prices)
    (let ((offset-period (+ period offset))
          (price-bar-p (typep price 'bar)))
      (when (and (not initialized)
                 (= (circbuf:cb-size prices) offset-period))
        (setf initialized t))
      (loop for i from offset below (if initialized
                                      offset-period
                                      (1- (circbuf:cb-size prices)))
            for p = (circbuf:cb-elt i prices)
            for high = (if price-bar-p (h p) p)
            for low = (if price-bar-p (l p) p)
            maximizing high into highest-high
            minimizing low into lowest-low
            finally (setf value (/ (+ highest-high lowest-low) 2)
                          upper-band highest-high
                          lower-band lowest-low)))))

(defun %update-indicator-atr (indicator true-range price)
  "Helper method to update the average-true-range indicator."
  (with-slots (initialized tr-buffer prev-price value
               period factor smoothing-type value-type) indicator
    (when (and (eql value-type :percent)
               (not (null prev-price)))
      (setf true-range (/ true-range (price prev-price))))
    (circbuf:cb-push true-range tr-buffer)
    (when (and (not initialized)
               (= (circbuf:cb-size tr-buffer) period))
      (setf initialized t))
    (setf prev-price price)
    (if (eql smoothing-type :ema)
      (setf value (+ (* factor true-range) (* (- 1 factor) (or value true-range)))) ;; exponential smoothing
      (let ((tr-sum 0))                                                             ;; simple smoothing
        (circbuf:do-circular-buffer (tr tr-buffer)
                                    (incf tr-sum tr))
        (setf value (/ tr-sum period))))))

(defmethod update-indicator ((i average-true-range) price)
  (with-slots (prev-price) i
    (let ((true-range (if (not (null prev-price))
                        (abs (- prev-price price))
                        0)))
      (%update-indicator-atr i true-range price))))

(defmethod update-indicator ((i average-true-range) (price prc))
  (with-slots (prev-price) i
    (let ((true-range (if (not (null prev-price))
                        (abs (- (price prev-price) (price price)))
                        0)))
      (%update-indicator-atr i true-range price))))

(defmethod update-indicator ((i average-true-range) (price bar))
  (with-slots (prev-price) i
    (let ((true-range (if (not (null prev-price))
                        (max (- (h price) (l price))
                             (abs (- (h price) (c prev-price)))
                             (abs (- (c prev-price) (l price))))
                        (- (h price) (l price)))))
      (%update-indicator-atr i true-range price))))

(defmethod update-indicator ((i parabolic-sar) (price bar))
  (with-slots (initialized state extreme acceleration-factor af-step af-step-max
               prices value) i
    (circbuf:cb-push price prices)
    (cond ((eql state :init)
           (setf initialized t
                 state :long
                 acceleration-factor af-step
                 extreme (h price)
                 value (l price)))
          ((eql state :short)
           (if (< value (h price))
             (setf state :long
                   acceleration-factor af-step
                   value extreme
                   extreme (h price))
             (progn
               (when (< (l price) extreme)
                 (setf acceleration-factor (min (+ acceleration-factor af-step) af-step-max)
                       extreme (l price)))
               (setf value (max (max (h (circbuf:cb-elt 0 prices))
                                     (h (circbuf:cb-elt 1 prices)))
                                (+ value (* acceleration-factor (- extreme value))))))))
          ((eql state :long)
           (if (> value (l price))
             (setf state :short
                   acceleration-factor af-step
                   value extreme
                   extreme (l price))
             (progn
               (when (> (h price) extreme)
                 (setf acceleration-factor (min (+ acceleration-factor af-step) af-step-max)
                       extreme (h price)))
               (setf value (min (min (l (circbuf:cb-elt 0 prices))
                                     (l (circbuf:cb-elt 1 prices)))
                                (+ value (* acceleration-factor (- extreme value))))))))
          (t (error "Invalid state.  Expected (member :init :long :short).")))))

;; EOF
