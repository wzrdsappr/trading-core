;;;; tick-bar-generator.lisp

(in-package #:trading-core)

(defclass tick-bar-generator (fsm-agent)
  ((mkt :accessor mkt :initarg :mkt)
   (n :accessor n :initarg :n)
   (counter :accessor counter :initform 0)
   (buffer :accessor buffer :initarg :buffer)
   (op :accessor op :initform nil)
   (hi :accessor hi :initform nil)
   (lo :accessor lo :initform nil)
   (cl :accessor cl :initform nil))
  (:documentation "Agent that creates compressed tick bars for consumption by other agents."))


;;; tick-bar-generator methods

(defmethod observe ((a tick-bar-generator) (e market-update))
  (and (equal (mkt a) (security e))
       (not (equal (type-of e) 'bar))))

(defmethod initialize ((a tick-bar-generator))
  (with-slots (states mkt n name counter buffer op hi lo cl transitions) a
    (when (null states)
      (push :emit states)
      (setf name (format nil "TICK-BAR-GENERATOR_~A_~A" mkt n))
      (setf transitions `((:calc . (,(make-instance
                                       'transition
                                       :initial-state :calc
                                       :final-state   :calc
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (< counter n))
                                       :actuator (lambda (p)
                                                   (setf cl p
                                                         hi (max hi p)
                                                         lo (min lo p))
                                                   (push p buffer)
                                                   (logv:format-log "~S CALC -> CALC ~%" name)))
                                     ,(make-instance
                                       'transition
                                       :initial-state :calc
                                       :final-state   :emit
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (= counter n))
                                       :actuator (lambda (p)
                                                   (emit a (make-instance
                                                             'tick-bar
                                                             :timestamp (first (timestamps a))
                                                             :security mkt
                                                             :value (list op hi lo cl)
                                                             :num-ticks n))
                                                   (setf buffer nil)
                                                   (logv:format-log "~S CALC -> EMIT ~%" name)))))
                          (:emit . (,(make-instance
                                       'transition
                                       :initial-state :emit
                                       :final-state   :calc
                                       :sensor #'price
                                       :predicate (lambda (p) t)
                                       :actuator (lambda (p)
                                                   (push p buffer)
                                                   (setf op p
                                                         hi p
                                                         lo p
                                                         cl p)
                                                   (logv:format-log "~S EMIT -> CALC ~%" name)))
                                     ,(make-instance
                                       'transition
                                       :initial-state :emit
                                       :final-state   :emit
                                       :sensor #'price
                                       :predicate (lambda (p) nil)
                                       :actuator (lambda (p) nil)))))))))

(defmethod preprocess ((a tick-bar-generator) (e market-update))
  (with-slots (counter buffer positions) a
    (push 0 positions)
    (setf counter (length buffer))))

(defmethod set-fsm ((a tick-bar-generator))
  (with-slots (current-state states) a
    (setf current-state (first states))))



;;EOF
