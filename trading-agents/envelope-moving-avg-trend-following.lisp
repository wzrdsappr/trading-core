;;;; envelope-moving-avg-trend-following.lisp

(in-package #:trading-core)

(defclass envelope-moving-avg-trend-following (fsm-agent)
  ((N :accessor N :initarg :N)               ; Length of moving average
   (width :accessor width :initarg :width)   ; Envelope width (1/2)
   (sensitivity :accessor sensitivity)       ; EMA factor, calculated from N
   (L :accessor L :initform nil)             ; Envelope upper boundary (go long)
   (S :accessor S :initform nil)             ; Envelope lower boundary (go short)
   (ema :accessor ema :initform nil)))       ; Exponential moving average

;;; envelope-moving-avg-trend-following methods

(defmethod initialize ((a envelope-moving-avg-trend-following))
  (with-slots (N sensitivity width L S states name long-size short-size positions transitions) a
    (when (null states)
      (setf sensitivity (/ 2 (1+ N)))
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
                                       (declare (ignore p))
                                       (push 0 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (>= p L))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push long-size positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (<= p S))
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
                                                  (> p S))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push long-size positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :long
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (<= p S))
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
                                         (>= p L))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push long-size positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (< p L))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push short-size positions))))))))))

(defmethod preprocess ((a envelope-moving-avg-trend-following) (e market-update))
  (with-slots (sensitivity width ema S L indicators) a
    (setf ema (if (null ema)
                (price e) 
                (+ (* sensitivity (price e)) (* (- 1 sensitivity) ema))))
    (setf L (* (+ 1 width) ema)
          S (* (- 1 width) ema))
    (push (list ema L S) indicators)))

(defmethod postprocess ((a envelope-moving-avg-trend-following) (e market-update))
  (call-next-method)
  (with-slots (counter ema states positions pls) a
    (logv:format-log "Output: EMA= ~S State= ~S
               Position= ~S PL= ~S~%" counter ema (first states)
               (first positions) (first pls))))

(defmethod extract-context-data ((a envelope-moving-avg-trend-following))
  `(,@(call-next-method)
     (:indicators . ,(extract-indicators a ("EMA" "UB" "LB")))))

;;EOF
