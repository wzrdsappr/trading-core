;;;; simple-model.lisp

(in-package #:trading-core)

(defclass simple-model (fsm-agent)
  ((L :accessor L :initarg :L)
   (sma :type simple-moving-average)))

(defmethod initialize ((a simple-model))
  (with-slots (L sma initialized states name transitions positions) a
    (assert (> L 0))
    (setf sma (make-instance 'simple-moving-average :period L))
    (when (null states)
      (push :init states)
      (setf name (format nil "SIMPLE-MODEL_~A" L))
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
                                        (push 1 positions)))
                         ,(make-instance
                            'transition
                            :initial-state :init
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and initialized (<= p (value sma))))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push -1 positions)))))
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
                                        (push 1 positions)))
                         ,(make-instance
                            'transition
                            :initial-state :long
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and initialized (<= p (value sma))))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push -1 positions)))))
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
                                         (push 1 positions)))
                          ,(make-instance
                             'transition
                             :initial-state :short
                             :final-state   :short
                             :sensor #'price
                             :predicate (lambda (p)
                                          (<= p (value sma)))
                             :actuator (lambda (p)
                                         (declare (ignore p))
                                         (push -1 positions))))))))))

(defmethod preprocess ((a simple-model) (e market-update))
  (with-slots (sma initialized indicators) a
    (update-indicator sma (price e))
    (when (and (not initialized) (initialized sma))
      (setf initialized t))
    (push (value sma) indicators)))

(defmethod postprocess ((a simple-model) (e market-update))
  (call-next-method)
  (with-slots (sma states positions pls) a
    (logv:format-log "Output: MA= ~S State= ~S Position= ~S PL= ~S~%"
                     (value sma) (first states) (first positions) (first pls))))

(defmethod extract-context-data ((a simple-model))
  "Returns the indicators that should be displayed on the price chart as context data for analysis output."
  `(,@(call-next-method)          ;; Get the generic agent context data relevant to any agent
    (:indicators . ,(loop with is-first = t
                          for ts in (reverse (timestamps a))
                          and ma in (reverse (indicators a))
                          for utc-date = (* 1000 (local-time:timestamp-to-unix ts))
                          when ma collect `((:is-first . ,is-first) (:utc-date . ,utc-date)
                                            (:value . ,(format nil "~,8f" ma))) into ma-list
                          when is-first do (setf is-first nil)
                          finally (return `#(((:indicator-name . "MA")
                                              (:indicator-data . #(,@ma-list)))))))))

;;EOF
