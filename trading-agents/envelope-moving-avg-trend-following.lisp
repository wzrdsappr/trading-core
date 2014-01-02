;;;; envelope-moving-avg-trend-following.lisp

(in-package #:trading-core)

(defclass envelope-moving-avg-trend-following (fsm-agent)
  ((N :accessor N :initarg :N)               ; Length of moving average
   (width :accessor width :initarg :width)   ; Envelope width (1/2)
   (sensitivity :accessor sensitivity)  ; EMA factor, calculated from N
   (L :accessor L :initform nil)     ; Envelope upper boundary (go long)
   (S :accessor S :initform nil)     ; Envelope lower boundary (go short)
   (ema :accessor ema :initform 0))) ; Exponential moving average

;;; envelope-moving-avg-trend-following methods

(defmethod initialize ((a envelope-moving-avg-trend-following))
  (with-slots (L S states name positions transitions) a
    (when (null states)
      (setf sensitivity (/ 2 (1+ n)))
      (push :init states)
      (setf name (format nil "ENVELOPE-MOVING-AVG-TREND-FOLLOWING_~A_~A" sensitivity width))
      (setf transitions
            `((:init . (,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :init
                           :sensor #'price
                           :predicate (lambda (p)
                                        (< S p L))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (format t "~S INIT -> INIT ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (>= p L))
                           :actuator (lambda (p)
                                       (push 1 positions)
                                       (format t "~S INIT -> LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (<= p S))
                           :actuator (lambda (p)
                                       (push -1 positions)
                                       (format t "~S INIT -> SHORT ~%" name)))))
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
                                                  (> p S))
                                     :actuator (lambda (p)
                                                 (push 1 positions)
                                                 (format t "~S LONG -> LONG ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :long
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (<= p S))
                                     :actuator (lambda (p)
                                                 (push -1 positions)
                                                 (format t "~S LONG -> SHORT ~%" name)))))
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
                                         (>= p L))
                            :actuator (lambda (p)
                                        (push 1 positions)
                                        (format t "~S SHORT -> LONG ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (< p L))
                            :actuator (lambda (p)
                                        (push -1 positions)
                                        (format t "~S SHORT -> SHORT ~%" name))))))))))

(defmethod preprocess ((a envelope-moving-avg-trend-following) (e market-update))
  (with-slots (sensitivity width ema S L) a
    (setf ema (if (null ema)
                (price e) 
                (+ (* sensitivity (price e)) (* (- 1 sensitivity) ema))))
    (setf L (* (+ 1 width) ema)
          S (* (- 1 width) ema))))

(defmethod set-fsm ((a envelope-moving-avg-trend-following))
  (with-slots (states current-state) a
    (setf current-state (first states))))

(defmethod postprocess ((a envelope-moving-avg-trend-following) (e market-update))
  (with-slots (name counter ma states positions pls) a
    (format t "Event ~S ~S Consumed for Agent ~S :~%"
            (timestamp e) (price e) name)
    (format t "Output: EMA= ~S State= ~S
               Position= ~S PL= ~S~%" counter ema (first states)
               (first positions) (first pls))))

;;EOF
