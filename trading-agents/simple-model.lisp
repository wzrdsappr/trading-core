;;;; simple-model.lisp

(in-package #:trading-core)

(defclass simple-model (fsm-agent)
  ((L :accessor L :initarg :L)
   (counter :accessor counter :initform 0)
   (ma :accessor ma :initform 0)))

(defmethod initialize ((a simple-model))
  (with-slots (L states name) a
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
                                        (<= counter L))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (logv:format-log "~S INIT -> INIT ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :init
                            :final-state   :long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and (> counter L) (> p ma)))
                            :actuator (lambda (p)
                                        (push 1 positions)
                                        (logv:format-log "~S INIT -> LONG ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :init
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and (> counter L) (<= p ma)))
                            :actuator (lambda (p)
                                        (push -1 positions)
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
                            :predicate (lambda (p)
                                         (> p ma))
                            :actuator (lambda (p)
                                        (push 1 positions)
                                        (logv:format-log "~S LONG -> LONG ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :long
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (<= p ma))
                            :actuator (lambda (p)
                                        (push -1 positions)
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
                             :predicate (lambda (p)
                                          (> p ma))
                             :actuator (lambda (p)
                                         (push 1 positions)
                                         (logv:format-log "~S SHORT -> LONG ~%" name)))
                          ,(make-instance
                             'transition
                             :initial-state :short
                             :final-state   :short
                             :sensor #'price
                             :predicate (lambda (p)
                                          (<= p ma))
                             :actuator (lambda (p)
                                         (push -1 positions)
                                         (logv:format-log "~S SHORT -> SHORT ~%" name))))))))))

(defmethod preprocess ((a simple-model) (e market-update))
  (with-slots (L counter ma revalprices) a
    (setf counter (length revalprices))
    (setf ma (avg-list (subseq revalprices 0 L)))))

(defmethod set-fsm ((a simple-model))
  (with-slots (states current-state) a
    (setf current-state (first states))))

(defmethod postprocess ((a simple-model) (e market-update))
  (with-slots (name counter ma states positions pls) a
    (logv:format-log "Event ~S ~S Consumed for Agent ~S :~%"
            (timestamp e) (price e) name)
    (logv:format-log "Output: counter= ~S MA= ~S State= ~S
               Position= ~S PL= ~S~%" counter ma (first states)
               (first positions) (first pls))))

;;EOF
