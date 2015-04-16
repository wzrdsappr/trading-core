;;;; frama-trend-following.lisp

(in-package #:trading-core)

(defclass fractal-ama-trend-following (fsm-agent)
  ((max-period :accessor max-period :initarg :max-period)
   (min-period :accessor min-period :initarg :min-period :initform 4)
   (fractal-length :accessor fractal-length :initarg :fractal-length :initform 126)
   (frama :type fractal-adaptive-moving-average)))

(defmethod initialize ((a fractal-ama-trend-following))
  (with-slots (max-period min-period fractal-length frama initialized states
               name long-size short-size transitions positions) a
    (assert (and (> max-period 0) (> fractal-length 0) (evenp fractal-length)))
    (setf frama (make-instance 'fractal-adaptive-moving-average
                               :max-period max-period
                               :min-period min-period
                               :fractal-length fractal-length))
    (when (null states)
      (push :init states)
      (setf name (format nil "FRAMATF_~A_~A_~A" min-period max-period fractal-length))
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
                                         (and initialized (> p (value frama))))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push long-size positions)))
                         ,(make-instance
                            'transition
                            :initial-state :init
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and initialized (<= p (value frama))))
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
                                         (and initialized (> p (value frama))))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push long-size positions)))
                         ,(make-instance
                            'transition
                            :initial-state :long
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and initialized (<= p (value frama))))
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
                                          (> p (value frama)))
                             :actuator (lambda (p)
                                         (declare (ignore p))
                                         (push long-size positions)))
                          ,(make-instance
                             'transition
                             :initial-state :short
                             :final-state   :short
                             :sensor #'price
                             :predicate (lambda (p)
                                          (<= p (value frama)))
                             :actuator (lambda (p)
                                         (declare (ignore p))
                                         (push short-size positions))))))))))

(defmethod preprocess ((a fractal-ama-trend-following) (e market-update))
  (with-slots (frama initialized indicators) a
    (update-indicator frama (price e))
    (when (and (not initialized) (initialized frama))
      (setf initialized t))
    (push (list (value frama)) indicators)))

(defmethod postprocess ((a fractal-ama-trend-following) (e market-update))
  (call-next-method)
  (with-slots (frama states positions pls) a
    (logv:format-log "Output: FRAMA= ~S State= ~S Position= ~S PL= ~S~%"
                     (value frama) (first states) (first positions) (first pls))))

(defmethod extract-context-data ((a fractal-ama-trend-following))
  "Returns the indicators that should be displayed on the price chart as context data for analysis output."
  `(,@(call-next-method)          ;; Get the generic agent context data relevant to any agent
    (:indicators . ,(extract-indicators a ("FRAMA")))))

;;EOF
