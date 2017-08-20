;;;; tick-bar-generator.lisp

(in-package #:trading-core)

(defclass tick-bar-generator (fsm-agent)
  ((n :accessor n :initarg :n)
   (counter :accessor counter :initform 0)
   (op :accessor op :initform nil)
   (hi :accessor hi :initform nil)
   (lo :accessor lo :initform nil)
   (cl :accessor cl :initform nil))
  (:documentation "Agent that creates compressed tick bars for consumption by other agents."))


;;; tick-bar-generator methods

(defmethod observe ((a tick-bar-generator) (e market-update))
  (and (equal (security a) (security e))
       (not (typep e 'bar))))

(defmethod initialize ((a tick-bar-generator))
  (with-slots (states security n name counter op hi lo cl transitions) a
    (assert (> n 1))
    (when (null states)
      (push :emit states)
      (setf transitions `((:calc . (,(make-instance
                                       'transition
                                       :initial-state :calc
                                       :final-state   :calc
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (declare (ignore p))
                                                    (< counter n))
                                       :actuator (lambda (p)
                                                   (setf cl p
                                                         hi (max hi p)
                                                         lo (min lo p))))
                                     ,(make-instance
                                       'transition
                                       :initial-state :calc
                                       :final-state   :emit
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (declare (ignore p))
                                                    (= counter n))
                                       :actuator (lambda (p)
                                                   (setf cl p
                                                         hi (max hi p)
                                                         lo (min lo p))
                                                   (emit a (make-instance
                                                             'tick-bar
                                                             :timestamp (first (timestamps a))
                                                             :security security
                                                             :value (list op hi lo cl)
                                                             :num-ticks n))))))
                          (:emit . (,(make-instance
                                       'transition
                                       :initial-state :emit
                                       :final-state   :calc
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (declare (ignore p))
                                                    t)
                                       :actuator (lambda (p)
                                                   (setf op p
                                                         hi p
                                                         lo p
                                                         cl p)))
                                     ,(make-instance
                                       'transition
                                       :initial-state :emit
                                       :final-state   :emit
                                       :sensor #'price
                                       :predicate #1=(lambda (p)
                                                       (declare (ignore p))
                                                       nil)
                                       :actuator #1#))))))))

(defmethod preprocess ((a tick-bar-generator) (e market-update))
  (with-slots (N counter positions) a
    (push 0 positions)
    (setf counter (1+ (rem counter N)))))

;;EOF
