;;;; adaptive-moving-avg-trend-following.lisp

(in-package #:trading-core)

(defclass adaptive-moving-avg-trend-following (fsm-agent)
  ((min-period :accessor min-period :initarg :min-period)       ; minimum length of moving average
   (max-period :accessor max-period :initarg :max-period)       ; maximum length of moving average
   (width-factor :accessor width-factor :initarg :width-factor) ; scaling factor for channel width
   (snr-factor :accessor snr-factor :initarg :snr-factor)       ; scaling factor for signal-to-noise ratio
   (ama :type adaptive-moving-average)
   (atr :type average-true-range)
   (SFL :accessor SFL :initform nil)     ; Stop from long
   (SFS :accessor SFS :initform nil)     ; Stop from short
   (PFL :accessor PFL :initform nil)     ; Profit from long value, 2R above stop
   (PFS :accessor PFS :initform nil)))   ; Profit from short value, 2R below stop

;;; adaptive-moving-avg-trend-following methods

(defmethod initialize ((a adaptive-moving-avg-trend-following))
  (with-slots (min-period max-period width-factor snr-factor PFL PFS SFL SFS
               states name long-size short-size positions transitions ama atr initialized) a
    (assert (< min-period max-period))
    (setf ama (make-instance 'adaptive-moving-average
                                       :min-period min-period
                                       :max-period max-period
                                       :width-factor width-factor
                                       :snr-factor snr-factor)
          atr (make-instance 'average-true-range
                             :period 20))
    (when (null states)
      (push :init states)
      (setf name (format nil "AMATF_~A_~A_~3,2F_~3,2A"
                         min-period max-period width-factor snr-factor))
      (setf transitions
            `((:init . (,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :init
                           :sensor #'price
                           :predicate (lambda (p)
                                        (or (not initialized)
                                            (< (lower-band ama) p (upper-band ama))))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and initialized (>= p (upper-band ama)) (< p PFL)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push long-size positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :stop-from-long
                           :sensor #'price
                           :predicate #1=(lambda (p)
                                           (declare (ignore p))
                                           nil)
                           :actuator #1#)
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :profit-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and initialized PFL (>= p PFL)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push long-size positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and initialized (lower-band ama) PFS (<= p (lower-band ama)) (> p PFS)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push short-size positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :stop-from-short
                           :sensor #'price
                           :predicate #1#
                           :actuator #1#)
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :profit-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and initialized PFS (<= p PFS)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push short-size positions)))))
              (:long . (,(make-instance
                                     'transition
                                     :initial-state :long
                                     :final-state   :init
                                     :sensor #'price
                                     :predicate #1#
                                     :actuator #1#)
                                  ,(make-instance
                                     'transition
                                     :initial-state :long
                                     :final-state   :long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (> p SFL) (< p PFL)))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push long-size positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :long
                                     :final-state   :stop-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (> p (lower-band ama)) (<= p SFL)))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push 0 positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :long
                                     :final-state   :profit-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (>= p PFL))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push (* 2 long-size) positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :long
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (<= p (lower-band ama)) (> p PFS)))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push short-size positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :long
                                     :final-state   :stop-from-short
                                     :sensor #'price
                                     :predicate #1#
                                     :actuator #1#)
                                  ,(make-instance
                                     'transition
                                     :initial-state :long
                                     :final-state   :profit-from-short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (<= p PFS))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push 0 positions)))))
              (:stop-from-long . (,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :init
                                     :sensor #'price
                                     :predicate #1#
                                     :actuator #1#)
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (>= p (upper-band ama)) (< p PFL)))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push long-size positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :stop-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (< (lower-band ama) p (upper-band ama)))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push 0 positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :profit-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (>= p PFL))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push long-size positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (<= p (lower-band ama)) (> p PFS)))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push short-size positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :stop-from-short
                                     :sensor #'price
                                     :predicate #1#
                                     :actuator #1#)
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :profit-from-short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (<= p PFS))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push short-size positions)))))
              (:profit-from-long . (,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :init
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :long
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :stop-from-long
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :profit-from-long
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (> p (lower-band ama)))
                                       :actuator (lambda (p)
                                                   (declare (ignore p))
                                                   (push (first positions) positions)))
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :short
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (and (<= p (lower-band ama)) (> p PFS)))
                                       :actuator (lambda (p)
                                                   (declare (ignore p))
                                                   (push short-size positions)))
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :stop-from-short
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :profit-from-short
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (<= p PFS))
                                       :actuator (lambda (p)
                                                   (declare (ignore p))
                                                   (push short-size positions)))))
              (:short . (,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :init
                            :sensor #'price
                            :predicate #1#
                            :actuator #1#)
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and (>= p (upper-band ama)) (< p PFL)))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push long-size positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :stop-from-long
                            :sensor #'price
                            :predicate #1#
                            :actuator #1#)
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :profit-from-long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (>= p PFL))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push long-size positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and (> p PFS) (< p SFS)))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push short-size positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :stop-from-short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and (>= p SFS) (< p (upper-band ama))))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push 0 positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :profit-from-short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (<= p PFS))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push (* 2 short-size) positions)))))
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
                                                   (and (>= p (upper-band ama)) (< p PFL)))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push long-size positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :stop-from-long
                                      :sensor #'price
                                      :predicate #1#
                                      :actuator #1#)
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :profit-from-long
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (>= p PFL))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push long-size positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (and (<= p (lower-band ama)) (> p PFS)))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push short-size positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :stop-from-short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (< (lower-band ama) p (upper-band ama)))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push 0 positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :profit-from-short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (<= p PFS))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push short-size positions)))))
              (:profit-from-short . (,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :init
                                        :sensor #'price
                                        :predicate #1#
                                        :actuator #1#)
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :long
                                        :sensor #'price
                                        :predicate (lambda (p)
                                                     (and (>= p (upper-band ama)) (< p PFL)))
                                        :actuator (lambda (p)
                                                    (declare (ignore p))
                                                    (push long-size positions)))
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :stop-from-long
                                        :sensor #'price
                                        :predicate #1#
                                        :actuator #1#)
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :profit-from-long
                                        :sensor #'price
                                        :predicate (lambda (p)
                                                     (>= p PFL))
                                        :actuator (lambda (p)
                                                    (declare (ignore p))
                                                    (push long-size positions)))
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :short
                                        :sensor #'price
                                        :predicate #1#
                                        :actuator #1#)
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :stop-from-short
                                        :sensor #'price
                                        :predicate #1#
                                        :actuator #1#)
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :profit-from-short
                                        :sensor #'price
                                        :predicate (lambda (p)
                                                     (< p (upper-band ama)))
                                        :actuator (lambda (p)
                                                    (declare (ignore p))
                                                    (push (first positions) positions))))))))))

(defmethod preprocess ((a adaptive-moving-avg-trend-following) (e market-update))
  (with-slots (initialized width-factor SFL SFS PFL PFS states ama atr revalprices indicators) a
    (update-indicator ama (price e))
    (update-indicator atr e)
    (when (and (not initialized) (initialized ama) (initialized atr))
      (setf initialized t))
    (when (null SFL)
      (setf SFL (price e)
            SFS (price e)
            PFL (price e)
            PFS (price e)))
    (let ((prev-p (or (second revalprices) (price e))))
      (setf SFL (if (< prev-p SFL)
                  (- (value ama) (* (value atr) width-factor))
                  (max SFL (- (value ama) (* (value atr) width-factor))))
            SFS (if (> prev-p SFS)
                  (+ (value ama) (* (value atr) width-factor))
                  (min SFS (+ (value ama) (* (value atr) width-factor)))))
      (unless (eql (first states) :long)
        (setf PFL (+ (upper-band ama) (* 1.2 (* (value atr) width-factor)))))
      (unless (eql (first states) :short)
        (setf PFS (- (lower-band ama) (* 1.2 (* (value atr) width-factor))))))
    (push (list (value ama) (upper-band ama) (lower-band ama) SFL SFS PFL PFS)
          indicators)))

(defmethod postprocess ((a adaptive-moving-avg-trend-following) (e market-update))
  (call-next-method)
  (with-slots (ama PFL PFS SFL SFS states positions pls) a
    (logv:format-log "Output: AMA= ~S UB= ~S LB= ~S PFL= ~S PFS= ~S SFL= ~S SFS= ~S State= ~S Position= ~S PL= ~S~%"
                     (value ama) (upper-band ama) PFL PFS SFL SFS
                     (first states) (first positions) (first pls))))

(defmethod extract-context-data ((a adaptive-moving-avg-trend-following))
  "Returns the indicators that should be displayed on the price chart as context data for analysis output."
  `(,@(call-next-method)                       ;; Get the generic agent context data relevant to any agent
    (:indicators . ,(extract-indicators a ("AMA" "UB" "LB" "SFL" "SFS" "PFL" "PFS")))))

;;EOF
