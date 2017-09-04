;;;; simple-model.lisp

(in-package #:trading-core)

(defclass simple-model (fsm-agent)
  ((L :accessor L :initarg :L)
   (sma :type simple-moving-average)))

(defmethod initialize ((a simple-model))
  (with-slots (L sma initialized states name long-size short-size transitions positions) a
    (assert (> L 0))
    (setf sma (make-instance 'simple-moving-average :period L))
    (when (null states)
      (push :init states)
      (setf transitions
            `((:init . (,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :init
                           :sensor #'price
                           :predicate (lambda (p)
                                        (declare (ignore p))
                                        (not initialized))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))
                         ,(make-instance
                            'transition
                            :initial-state :init
                            :final-state   :long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and initialized (> p (value sma))))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push long-size positions)))
                         ,(make-instance
                            'transition
                            :initial-state :init
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and initialized (<= p (value sma))))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push short-size positions)))))
              (:long . (,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :init
                           :sensor #'price
                           :predicate #1=(lambda (p)
                                           (declare (ignore p))
                                           nil)
                           :actuator #1#)
                         ,(make-instance
                            'transition
                            :initial-state :long
                            :final-state   :long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and initialized (> p (value sma))))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push long-size positions)))
                         ,(make-instance
                            'transition
                            :initial-state :long
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and initialized (<= p (value sma))))
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
                                          (> p (value sma)))
                             :actuator (lambda (p)
                                         (declare (ignore p))
                                         (push long-size positions)))
                          ,(make-instance
                             'transition
                             :initial-state :short
                             :final-state   :short
                             :sensor #'price
                             :predicate (lambda (p)
                                          (<= p (value sma)))
                             :actuator (lambda (p)
                                         (declare (ignore p))
                                         (push short-size positions))))))))))

(defmethod preprocess ((a simple-model) (e market-update))
  (with-slots (sma initialized indicators) a
    (update-indicator sma (price e))
    (when (and (not initialized) (initialized sma))
      (setf initialized t))
    (push (list (value sma)) indicators)))

(defmethod postprocess ((a simple-model) (e market-update))
  (call-next-method)
  (with-slots (L sma states positions pls) a
    (log:debug "Output: L= ~S SMA= ~S State= ~S Position= ~S PL= ~S~%"
                     L (value sma) (first states) (first positions) (first pls))))

(defmethod extract-context-data ((a simple-model))
  "Returns the indicators that should be displayed on the price chart as context data for analysis output."
  `(,@(call-next-method)          ;; Get the generic agent context data relevant to any agent
    (:indicators . ,(extract-indicators a ("SMA")))))

;;EOF
