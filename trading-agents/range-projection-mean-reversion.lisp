;;;; range-projection-mean-reversion.lisp

(in-package #:trading-core)

;;; In order for this trading strategy to work correctly, the event stream must be at a faster
;;; frequency than the projection interval since the projected range is based on the opening value
;;; of the projection interval.  Additionally, the smoothed trading range limits must be re-calculated
;;; only at the projection interval, so an instance of the TIME-BAR-GENERATOR that will generate bars
;;; at the projection interval must be created to supply market updates to the strategy as well.

(defclass range-projection-mean-reversion (fsm-agent)
  ((N :accessor N :initarg :N)                                     ; Number of bars used to smooth price range
   (projection-interval :reader projection-interval :initarg :projection-interval
    :type (member :week :day :hour) :initform :day)
   (pivot-ma :accessor pivot-ma :type simple-moving-average)       ; Pivot moving average
   (pivot-ma-history :accessor pivot-ma-history :type circbuf:circular-buffer)
   (tr-ma :accessor tr-ma :initform 0)                             ; Trading range moving average
   (market-on-close :initform t)
   (L :accessor L :initform nil)       ; Lower channel (go long)
   (S :accessor S :initform nil)       ; Upper channel (go short)
   (SFL :accessor SFL :initform nil)   ; Stop from long value
   (SFS :accessor SFS :initform nil))) ; Stop from short value

;;; range-projection-mean-reversion methods

;; Prevent the use of this strategy for event durations below the fidelity required
;; by the PROJECTION-INTERVAL. i.e. For a projection duration of day, only events with
;; a time unit of hour or less are valid since the opening value for the day is required
;; in time to place orders for the day.
(defmethod observe ((a range-projection-mean-reversion) (e time-bar))
  (and (call-next-method)
       (let ((time-units '(:month :week :day :hour)))
         (< (position (projection-interval a) time-units)
            (position (time-unit e) time-units)))))

(defmethod initialize ((a range-projection-mean-reversion))
  (with-slots (N projection-interval pivot-ma pivot-ma-history tr-ma
               market-on-close range-duration initialized L S SFL SFS states
               name long-size short-size positions transitions) a
    (when (null states)
      (setf pivot-ma (make-instance 'simple-moving-average
                                    :period N)
            pivot-ma-history (make-instance 'circbuf:circular-buffer
                                            :size N)
            tr-ma (make-instance 'simple-moving-average
                                 :period N))
      (push :init states)
      (setf transitions
            `((:init . (,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :init
                           :sensor #'price
                           :predicate (lambda (p)
                                        (or (not initialized)
                                            market-on-close
                                            (< L p S)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and initialized
                                             (not market-on-close)
                                             (<= p L)
                                             (> p SFL)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push long-size positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :stop-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and initialized
                                             (not market-on-close)
                                             (<= p SFL)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and initialized
                                             (not market-on-close)
                                             (>= p S)
                                             (< p SFS)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push short-size positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :stop-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and initialized
                                             (not market-on-close)
                                             (>= p SFS)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))))
              (:long . (,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :init
                           :sensor #'price
                           :predicate (lambda (p)
                                        (declare (ignore p))
                                        market-on-close)
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> p SFL) (< p S)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push long-size positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :stop-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (<= p SFL))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (>= p S) (< p SFS)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push short-size positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :stop-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (>= p SFS))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))))
              (:stop-from-long . (,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :init
                                     :sensor #'price
                                     :predicate #1=(lambda (p)
                                                     (declare (ignore p))
                                                     nil)
                                     :actuator #1#)
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :long
                                     :sensor #'price
                                     :predicate #1#
                                     :actuator #1#)
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :stop-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (< p S))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push 0 positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (>= p S) (< p SFS)))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push short-size positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :stop-from-short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (>= p SFS))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push 0 positions)))))
              (:short . (,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :init
                            :sensor #'price
                            :predicate (lambda (p)
                                         (declare (ignore p))
                                         market-on-close)
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push 0 positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and (> p SFL) (<= p L)))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push long-size positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :stop-from-long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (<= p SFL))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push 0 positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and (> p L) (< p SFS)))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push short-size positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :stop-from-short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (> p SFS))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push 0 positions)))))
              (:stop-from-short . (,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :init
                                      :sensor #'price
                                      :predicate #1#
                                      :actuator #1#)
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :long
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (and (> p SFL) (<= p L)))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push long-size positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :stop-from-long
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (<= p SFL))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push 0 positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :short
                                      :sensor #'price
                                      :predicate #1#
                                      :actuator #1#)
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :stop-from-short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (> p L))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push 0 positions))))))))))

(defmethod preprocess ((a range-projection-mean-reversion) (e comm))
  "Update range calculations once per PROJECTION-INTERVAL rather than at the shorter
MARKET-UPDATE interval."
  (with-slots (N projection-interval pivot-ma pivot-ma-history tr-ma initialized
               market-hours market-on-close L S SFL SFS) a
    (let ((e (value e)))
      (when (and (typep e 'time-bar) (eql (time-unit e) projection-interval))
        (let ((trading-range (* 2 (- (h e) (l e)) (+ (h e) (l e))))
              (channel-center (+ (o e) (/ (- pivot-ma (circbuf:cb-elt pivot-ma-history (1- N))) N)))
              (prev-tr-ma (value tr-ma)))
          (update-indicator pivot-ma (pivot e))
          (update-indicator tr-ma trading-range)
          (circbuf:cb-push (value pivot-ma) pivot-ma-history)
          (when (and (not initialized)
                     (initialized pivot-ma)
                     (= (circbuf:cb-size pivot-ma-history) N)
                     (initialized tr-ma))
            (setf initialized t))
          (setf L (/ channel-center (1+ (* prev-tr-ma 1/2)))
                S (* channel-center (1+ (* prev-tr-ma 1/2)))
                SFL (/ channel-center (1+ (* prev-tr-ma 2/3)))
                SFS (* channel-center (1+ (* prev-tr-ma 2/3)))))))))

(defmethod preprocess ((a range-projection-mean-reversion) (e market-update))
  (with-slots (projection-interval market-hours market-on-close L S SFL SFS indicators) a
    (let ((weekday (local-time:timestamp-day-of-week (timestamp e)))
          (outside-market-hours (market-closed-p a (timestamp e))))
      (setf market-on-close
            (or (and (eql projection-interval :day)
                     outside-market-hours)
                (and (eql projection-interval :week) ; Allow holding a position for a week
                     (or (member weekday '(0 6))     ; Sunday or Saturday
                         (and (= weekday 1)          ; Monday before market open
                              (< (local-time:sec-of (timestamp e))
                                 (local-time:sec-of (first market-hours))))
                         (and (= weekday 5)          ; Friday after market close
                              (>= (local-time:sec-of (timestamp e))
                                  (local-time:sec-of (second market-hours))))))
                (and (eql projection-interval :hour)
                     (or outside-market-hours        ; Close out positions each hour
                         (>= (rem (local-time:sec-of (timestamp e)) local-time:+seconds-per-hour+)
                             (* local-time:+seconds-per-hour+ 55/60)))))))
    (push (list L S SFL SFS) indicators)))

(defmethod postprocess ((a range-projection-mean-reversion) (e comm))
  (call-next-method)
  (with-slots (L S SFL SFS states positions pls) a
    (log:debug "Output: L= ~S S= ~S SFL= ~S SFS= ~S State= ~S ~
               Position= ~S PL= ~S~%" L S SFL SFS (first states)
               (first positions) (first pls))))

(defmethod postprocess ((a range-projection-mean-reversion) (e bar))
  (call-next-method)
  (with-slots (L S SFL SFS states positions pls) a
    (log:debug "Output: L= ~S S= ~S SFL= ~S SFS= ~S State= ~S ~
               Position= ~S PL= ~S~%" L S SFL SFS (first states)
               (first positions) (first pls))))

(defmethod extract-context-data ((a range-projection-mean-reversion))
  "Returns the indicators that should be displayed on the price chart as context data for analysis output."
  `(,@(call-next-method)          ;; Get the generic agent context data relevant to any agent
    (:indicators . ,(extract-indicators a ("L" "S" "SFL" "SFS")))))

;;EOF
