;;;; market-direction-filter.lisp

(in-package #:trading-core)

(defclass market-direction-filter (fsm-agent)
  ((period :accessor period :initarg :period :initform 100)
   (direction-indicator :type indicator)
   (envelope :type channel)))

;;; market-direction-filter methods

(defmethod initialize ((a market-direction-filter))
  (with-slots (name period direction-indicator envelope
               initialized states positions transitions) a
    (setf direction-indicator (make-instance 'moving-linear-regression
                                             :period period)
          envelope (make-instance 'channel
                                  :center-indicator
                                  (make-instance 'simple-moving-average
                                                 :period 7)
                                  :channel-width-indicator
                                  (make-instance 'simple-moving-average
                                                 :period 7)
                                  :width-multiplier 0.02))
    (when (null states)
      (push :range states)
      (emit a :range 'market-direction-comm)
      (setf transitions
            `((:range . (,(make-instance
                            'transition
                            :initial-state :range
                            :final-state   :range
                            :sensor #'price
                            :predicate #1=(lambda (p)
                                            (declare (ignore p))
                                            (or (not initialized)
                                                (<= (lower-band envelope)
                                                    (value direction-indicator)
                                                    (upper-band envelope))))
                            :actuator #7=(lambda (p)
                                           (declare (ignore p))
                                           nil))
                         ,(make-instance
                            'transition
                            :initial-state :range
                            :final-state   :long
                            :sensor #'price
                            :predicate #2=(lambda (p)
                                            (declare (ignore p))
                                            (and initialized
                                                 (> (value direction-indicator)
                                                    (upper-band envelope))))
                            :actuator #3=(lambda (p)
                                           (declare (ignore p))
                                           (emit a :long 'market-direction-comm)))
                         ,(make-instance
                            'transition
                            :initial-state :range
                            :final-state   :short
                            :sensor #'price
                            :predicate #4=(lambda (p)
                                            (declare (ignore p))
                                            (and initialized
                                                 (< (value direction-indicator)
                                                    (lower-band envelope))))
                            :actuator #5=(lambda (p)
                                           (declare (ignore p))
                                           (emit a :short 'market-direction-comm)))))
            (:long . (,(make-instance
                         'transition
                         :initial-state :long
                         :final-state   :range
                         :sensor #'price
                         :predicate #1#
                         :actuator #6=(lambda (p)
                                         (declare (ignore p))
                                         (emit a :range 'market-direction-comm)))
                      ,(make-instance
                         'transition
                         :initial-state :long
                         :final-state   :long
                         :sensor #'price
                         :predicate #2#
                         :actuator #7#)
                      ,(make-instance
                         'transition
                         :initial-state :long
                         :final-state   :short
                         :sensor #'price
                         :predicate #4#
                         :actuator #5#)))
            (:short . (,(make-instance
                          'transition
                          :initial-state :short
                          :final-state   :range
                          :sensor #'price
                          :predicate #1#
                          :actuator #6#)
                       ,(make-instance
                          'transition
                          :initial-state :short
                          :final-state   :long
                          :sensor #'price
                          :predicate #2#
                          :actuator #3#)
                       ,(make-instance
                          'transition
                          :initial-state :short
                          :final-state   :short
                          :sensor #'price
                          :predicate #4#
                          :actuator #7#))))))))

(defmethod preprocess ((a market-direction-filter) (e market-update))
  (with-slots (positions direction-indicator envelope initialized emitted indicators) a
    (update-indicator direction-indicator (price e))
    (update-indicator envelope (value direction-indicator))
    (when (and (not initialized) (initialized direction-indicator) (initialized envelope))
      (setf initialized t))
    (push (list (value direction-indicator) (upper-band envelope) (lower-band envelope)) indicators)
    (push 0 positions)))

(defmethod postprocess ((a market-direction-filter) (e comm))
  (with-slots (current-state) a
    (log:debug "Output: CURRENT-STATE= ~S~%" current-state)))

(defmethod extract-context-data ((a market-direction-filter))
  "Returns the indicators that should be displayed on the price chart as context data for analysis output."
  `(,@(call-next-method)          ;; Get the generic agent context data relevant to any agent
    (:indicators . ,(extract-indicators a ("MDF" "MD-UB" "MD-LB")))))

;;EOF
