;;;; simple-model.lisp

(in-package #:trading-core)

(defclass simple-model-comm (simple-model)
  ((mkt :accessor mkt :initarg :mkt)
   (unblock-short :accessor unblock-short :initform -1)
   (unblock-long :accessor unblock-long :initform 1)))

;;; simple-model-comm methods

(defmethod observe ((a simple-model-comm) (e market-update))
  (equal (mkt a) (security e)))

(defmethod initialize ((a simple-model-comm))
  (with-slots (L counter ma revalprices transitions unblock-long unblock-short) a
    (setf counter (length revalprices))
    (setf ma (avg-list (sub-list revalprices 0 L)))
    (setf transitions `((:init . (,(make-instance
                                     'transition
                                     :initial-state :init
                                     :final-state   :init
                                     :sensor #'price
                                     :predicate (lambda (p) (<= counter L))
                                     :actuator (lambda (p)
                                                 (push 0 positions)
                                                 (logv:format-log "~S INIT -> INIT ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :init
                                     :final-state   :long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (> counter L) (> p MA)))
                                     :actuator (lambda (p)
                                                 (push unblock-long positions)
                                                 (emit a :long)
                                                 (logv:format-log "~S INIT -> LONG ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :init
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (> counter L) (<= p MA)))
                                     :actuator (lambda (p)
                                                 (push unblock-short positions)
                                                 (emit a :short)
                                                 (logv:format-log "~S INIT -> SHORT ~%" name)))))
                         (:long . (,(make-instance
                                      'transition
                                      :initial-state :long
                                      :final-state   :init
                                      :sensor #'price
                                      :predicate (lambda (p) nil)
                                      :actuator (lambda (p) nil))
                                   ,(make-instance
                                      'transition
                                      :initial-state :long
                                      :final-state   :long
                                      :sensor #'price
                                      :predicate (lambda (p) (> p MA))
                                      :actuator (lambda (p)
                                                  (push unblock-long positions)
                                                  (emit a :long)
                                                  (logv:format-log "~S LONG -> LONG ~%" name)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :long
                                      :final-state   :short
                                      :sensor #'price
                                      :predicate (lambda (p) (<= p MA))
                                      :actuator (lambda (p)
                                                  (push unblock-short positions)
                                                  (emit a :short)
                                                  (logv:format-log "~S LONG -> SHORT ~%" name)))))
                         (:short . (,(make-instance
                                       'transition
                                       :initial-state :short
                                       :final-state   :init
                                       :sensor #'price
                                       :predicate (lambda (p) nil)
                                       :actuator (lambda (p) nil))
                                    ,(make-instance
                                       'transition
                                       :initial-state :short
                                       :final-state   :long
                                       :sensor #'price
                                       :predicate (lambda (p) (> p MA))
                                       :actuator (lambda (p)
                                                   (push unblock-long positions)
                                                   (emit a :long)
                                                   (logv:format-log "~S SHORT -> LONG ~%" name)))
                                    ,(make-instance
                                       'transition
                                       :initial-state :short
                                       :final-state   :short
                                       :sensor #'price
                                       :predicate (lambda (p) (<= p MA))
                                       :actuator (lambda (p)
                                                   (push unblock-short positions)
                                                   (emit a :short)
                                                   (logv:format-log "~S SHORT -> SHORT ~%" name)))))))))

(defmethod preprocess ((a simple-model-comm) (e comm))
  (with-slots (unblock-long unblock-short)
    (case (value e)
      (:init (setf unblock-short 0 unblock-long 0))
      (:long (setf unblock-short -1 unblock-long 0))
      (:short (setf unblock-short 0 unblock-long 1)))))

(defmethod set-fsm ((a simple-model-comm))
  (with-slots (states current-state)
    (setf current-state (first states))))

;;EOF
