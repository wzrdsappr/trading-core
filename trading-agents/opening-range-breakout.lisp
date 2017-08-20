;;;; opening-range-breakout.lisp

(in-package #:trading-core)

(defclass opening-range-breakout (fsm-agent)
  ((volatility-limit :initarg :volatility-limit)
   (N :initarg :N)
   (market-on-close :initform t)
   (counter :initform 0)
   (volatility :type average-true-range)
   (K1 :initarg K1 :initform 3/2)     ;; volatility multiple to set entry point
   (K2 :initarg K2 :initform 3)       ;; volatility multiple to set profit point
   (R1 :initform 0)
   (R2 :initform 0)
   (S1 :initform 0)
   (S2 :initform 0)))

(defmethod initialize ((a opening-range-breakout))
  (with-slots (name market-on-close volatility-limit volatility counter N
               R1 R2 S1 S2 long-size short-size positions states transitions) a
    (when (null states)
      (setf volatility (make-instance 'average-true-range
                                      :period N
                                      :value-type :percent))
      (push :init states)
      (setf transitions
            `((:init . (,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :init
                           :sensor #'price
                           :predicate (lambda (p)
                                        (or market-on-close
                                            (<= counter N)
                                            (< S1 p R1)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (not market-on-close)
                                             (> counter N)
                                             (< R1 p R2)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push long-size positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :flat-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (not market-on-close)
                                             (> counter N)
                                             (> p R2)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (not market-on-close)
                                             (> counter N)
                                             (< S2 p S1)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push short-size positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :flat-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (not market-on-close)
                                             (> counter N)
                                             (< p S2)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push short-size positions)))))
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
                                        (> S1 p R2))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push long-size positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :flat-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (>= p R2))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (< S2 p S1))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push short-size positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :flat-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (>= p S2))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))))
              (:flat-from-long . (,(make-instance
                                     'transition
                                     :initial-state :flat-from-long
                                     :final-state   :init
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (<= S1 p R1))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push 0 positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :flat-from-long
                                     :final-state   :long
                                     :sensor #'price
                                     :predicate #1=(lambda (p)
                                                     (declare (ignore p))
                                                     nil)
                                     :actuator #1#)
                                  ,(make-instance
                                     'transition
                                     :initial-state :flat-from-long
                                     :final-state   :flat-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (> p R1))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push 0 positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :flat-from-long
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (< S2 p S1))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push short-size positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :flat-from-long
                                     :final-state   :flat-from-short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (>= p S2))
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
                                         (< R1 p R2))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push long-size positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :flat-from-long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (>= p R2))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push 0 positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (<= S2 p R1))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push short-size positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :flat-from-short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (<= p S2))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push 0 positions)))))
              (:flat-from-short . (,(make-instance
                                      'transition
                                      :initial-state :flat-from-short
                                      :final-state   :init
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (<= S1 p R1))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push 0 positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :flat-from-short
                                      :final-state   :long
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (< R1 p R2))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push long-size positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :flat-from-short
                                      :final-state   :flat-from-long
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (>= p R2))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push 0 positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :flat-from-short
                                      :final-state   :short
                                      :sensor #'price
                                      :predicate #1#
                                      :actuator #1#)
                                   ,(make-instance
                                      'transition
                                      :initial-state :flat-from-short
                                      :final-state   :flat-from-short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (<= p S1))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push 0 positions))))))))))

(defmethod preprocess ((a opening-range-breakout) (e market-update))
  (with-slots (counter volatility-limit N volatility market-on-close
               market-hours K1 K2 R1 R2 S1 S2 indicators) a
    (let ((prev-volatility (value volatility))) 
      (setf market-on-close (market-closed-p a (timestamp e))
            counter (if market-on-close 0 (1+ counter))) 
      (update-indicator volatility e)
      (when (and (not market-on-close)
                 (< prev-volatility volatility-limit)
                 (>= (value volatility) volatility-limit))
        (setf R1 (* (price e) (1+ (* (value volatility) K1)))
              R2 (* (price e) (1+ (* (value volatility) K2)))
              S1 (/ (price e) (1+ (* (value volatility) K1)))
              S2 (/ (price e) (1+ (* (value volatility) K2))))))
    (push (list R1 R2 S1 S2) indicators)))

(defmethod postprocess ((a opening-range-breakout) (e market-update))
  (call-next-method)
  (with-slots (counter volatility R1 R2 S1 S2 states positions pls) a
    (logv:format-log "Output: counter= ~S volatility= ~S R1= ~S R2= ~S S1= ~S S2= ~S
               State= ~S Position= ~S PL= ~S~%" counter volatility R1 R2 S1 S2
    (first states) (first positions) (first pls))))

(defmethod extract-context-data ((a opening-range-breakout))
  "Returns the indicators that should be displayed on the price chart as context data for analysis output."
  `(,@(call-next-method)          ;; Get the generic agent context data relevant to any agent
    (:indicators . ,(extract-indicators a ("R1" "R2" "S1" "S2")))))

;;EOF
