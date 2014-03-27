;;;; simple-model-comm.lisp

(in-package #:trading-core)

(defclass simple-model-comm (simple-model)
  ((unblock-short :accessor unblock-short :initform -1)
   (unblock-long :accessor unblock-long :initform 1)))

;;; simple-model-comm methods

(defmethod initialize ((a simple-model-comm))
  (call-next-method)
  (with-slots (name transitions L sma initialized unblock-long unblock-short positions) a
    (setf name (format nil "SIMPLE-MODEL-COMM_~A" L))
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
                                     (push unblock-long positions)
                                     (emit a :long)))
                      ,(make-instance
                         'transition
                         :initial-state :init
                         :final-state   :short
                         :sensor #'price
                         :predicate (lambda (p)
                                      (and initialized (<= p (value sma))))
                         :actuator (lambda (p)
                                     (declare (ignore p))
                                     (push unblock-short positions)
                                     (emit a :short)))))
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
                         :predicate (lambda (p) (> p (value sma)))
                         :actuator (lambda (p)
                                     (declare (ignore p))
                                     (push unblock-long positions)
                                     (emit a :long)))
                      ,(make-instance
                         'transition
                         :initial-state :long
                         :final-state   :short
                         :sensor #'price
                         :predicate (lambda (p) (<= p (value sma)))
                         :actuator (lambda (p)
                                     (declare (ignore p))
                                     (push unblock-short positions)
                                     (emit a :short)))))
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
                          :predicate (lambda (p) (> p (value sma)))
                          :actuator (lambda (p)
                                      (declare (ignore p))
                                      (push unblock-long positions)
                                      (emit a :long)))
                       ,(make-instance
                          'transition
                          :initial-state :short
                          :final-state   :short
                          :sensor #'price
                          :predicate (lambda (p) (<= p (value sma)))
                          :actuator (lambda (p)
                                      (declare (ignore p))
                                      (push unblock-short positions)
                                      (emit a :short)))))))))

(defmethod preprocess ((a simple-model-comm) (e comm))
  (with-slots (unblock-long unblock-short) a
    (case (value e)
      (:init (setf unblock-short 0
                   unblock-long 0))
      (:long (setf unblock-short -1
                   unblock-long 0))
      (:short (setf unblock-short 0
                    unblock-long 1)))))

(defmethod postprocess ((a simple-model) (e comm))
  (call-next-method)
  (with-slots (unblock-long unblock-short) a
    (logv:format-log "Output: UNBLOCK-SHORT= ~S UNBLOCK-LONG= ~S~%"
                      unblock-long unblock-short)))

;;EOF
