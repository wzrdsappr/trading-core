;;;; opening-range-breakout.lisp

(in-package #:trading-core)

(defclass opening-range-breakout (fsm-agent)
  ((market-hours :accessor market-hours :initarg :market-hours :initform (list (f-h-m 0800) (f-h-m 1600)))
   (volatility-limit :accessor volatility-limit :initarg :volatility-limit)
   (N :accessor N :initarg :N)
   (market-on-close :accessor market-on-close :initform t)
   (counter :accessor counter :initform 0)
   (volatility :accessor volatility :initform 0)
   (K1 :accessor K1 :initarg K1 :initform 3/2)     ;; volatility multiple to set entry point
   (K2 :accessor K2 :initarg K2 :initform 3)     ;; volatility multiple to set profit point
   (R1 :accessor R1 :initform 0)
   (R2 :accessor R2 :initform 0)
   (S1 :accessor S1 :initform 0)
   (S2 :accessor S2 :initform 0)))

(defmethod initialize ((a opening-range-breakout))
  (with-slots (market-on-close counter N states name) a
    (when (null states)
      (setf scale-factor (/ 2 (1+ N)))
      (push :init states)
      (setf name (format nil "OPENING-RANGE-BREAKOUT_~A_~A" volatility-limit N))
      (setf transitions
            `((:init . (,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :init
                           :sensor #'price
                           :predicate (lambda (p)
                                        (or market-on-close
                                            (<= counter L)
                                            (< S1 p R1)))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (logv:format-log "~S INIT -> INIT ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (not market-on-close)
                                             (> counter L)
                                             (< R1 p R2)))
                           :actuator (lambda (p)
                                       (push 1 positions)
                                       (logv:format-log "~S INIT -> LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :flat-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (not market-on-close)
                                             (> counter L)
                                             (> p R2)))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (logv:format-log "~S INIT -> FLAT-FROM-LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (not market-on-close)
                                             (> counter L)
                                             (<= p ma)))
                           :actuator (lambda (p)
                                       (push -1 positions)
                                       (logv:format-log "~S INIT -> SHORT ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :flat-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (not market-on-close)
                                             (> counter L)
                                             (< p S2)))
                           :actuator (lambda (p)
                                       (push -1 positions)
                                       (logv:format-log "~S INIT -> FLAT-FROM-SHORT ~%" name)))))
              (:long . (,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :init
                           :sensor #'price
                           :predicate (lambda (p) market-on-close)
                           :actuator (lambda (p)
                                        (push 0 positions)
                                        (logv:format-log "~S LONG -> INIT ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (> S1 p R2))
                           :actuator (lambda (p)
                                       (push 1 positions)
                                       (logv:format-log "~S LONG -> LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :flat-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (>= p R2))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (logv:format-log "~S LONG -> FLAT-FROM-LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (< S2 p S1))
                           :actuator (lambda (p)
                                       (push -1 positions)
                                       (logv:format-log "~S LONG -> SHORT ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :flat-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (>= p S2))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (logv:format-log "~S LONG -> FLAT-FROM-SHORT ~%" name)))))
              (:flat-from-long . (,(make-instance
                                     'transition
                                     :initial-state :flat-from-long
                                     :final-state   :init
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (<= S1 p R1))
                                     :actuator (lambda (p)
                                                 (push 0 positions)
                                                 (logv:format-log "~S FLAT-FROM-LONG -> INIT ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :flat-from-long
                                     :final-state   :long
                                     :sensor #'price
                                     :predicate (lambda (p) nil)
                                     :actuator (lambda (p) nil))
                                  ,(make-instance
                                     'transition
                                     :initial-state :flat-from-long
                                     :final-state   :flat-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (> p R1))
                                     :actuator (lambda (p)
                                                 (push 0 positions)
                                                 (logv:format-log "~S FLAT-FROM-LONG -> FLAT-FROM-LONG ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :flat-from-long
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (< S2 p S1))
                                     :actuator (lambda (p)
                                                 (push -1 positions)
                                                 (logv:format-log "~S FLAT-FROM-LONG -> SHORT ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :flat-from-long
                                     :final-state   :flat-from-short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (>= p S2))
                                     :actuator (lambda (p)
                                                 (push 0 positions)
                                                 (logv:format-log "~S FLAT-FROM-LONG -> FLAT-FROM-SHORT ~%" name)))))
              (:short . (,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :init
                            :sensor #'price
                            :predicate (lambda (p) market-on-close)
                            :actuator (lambda (p)
                                        (push 0 positions)
                                        (logv:format-log "~S SHORT -> INIT ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (< R1 p R2))
                            :actuator (lambda (p)
                                        (push 1 positions)
                                        (logv:format-log "~S SHORT -> LONG ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :flat-from-long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (>= p R2))
                            :actuator (lambda (p)
                                        (push 0 positions)
                                        (logv:format-log "~S SHORT -> FLAT-FROM-LONG ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (<= S2 p R1))
                            :actuator (lambda (p)
                                        (push -1 positions)
                                        (logv:format-log "~S SHORT -> SHORT ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :flat-from-short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (<= p S2))
                            :actuator (lambda (p)
                                        (push 0 positions)
                                        (logv:format-log "~S SHORT -> FLAT-FROM-SHORT ~%" name)))))
              (:flat-from-short . (,(make-instance
                                      'transition
                                      :initial-state :flat-from-short
                                      :final-state   :init
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (<= S1 p R1))
                                      :actuator (lambda (p)
                                                  (push 0 positions)
                                                  (logv:format-log "~S FLAT-FROM-SHORT -> INIT ~%" name)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :flat-from-short
                                      :final-state   :long
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (< R1 p R2))
                                      :actuator (lambda (p)
                                                  (push 1 positions)
                                                  (logv:format-log "~S FLAT-FROM-SHORT -> LONG ~%" name)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :flat-from-short
                                      :final-state   :flat-from-long
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (>= p R2))
                                      :actuator (lambda (p)
                                                  (push 0 positions)
                                                  (logv:format-log "~S FLAT-FROM-SHORT -> FLAT-FROM-LONG ~%" name)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :flat-from-short
                                      :final-state   :short
                                      :sensor #'price
                                      :predicate (lambda (p) nil)
                                      :actuator (lambda (p) nil))
                                   ,(make-instance
                                      'transition
                                      :initial-state :flat-from-short
                                      :final-state   :flat-from-short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (<= p S1))
                                      :actuator (lambda (p)
                                                  (push 0 positions)
                                                  (logv:format-log "~S FLAT-FROM-SHORT -> FLAT-FROM-SHORT ~%" name))))))))))

(defmethod preprocess ((a opening-range-breakout) (e market-update))
  (with-slots (volatility-limit N revalprices volatility market-on-close K1 K2 R1 R2 S1 S2) a
    (incf counter)
    (let ((prev-p (second revalprices))
          (prev-volatility volatility)) 
      (setf market-on-close (or (<  (timestamp p) (first market-hours))
                                (>= (timestamp p) (- (second market-hours) 15)))) 
      (setf volatility (+ (* scale-factor (abs (/ (- (price e) prev-p) prev-p)))
                          (* (- 1 scale-factor) volatility))))
    (when (and (not market-on-close)
               (< prev-volatility volatility-limit)
               (>= volatility volatility-limit))
      (setf R1 (* (price e) (1+ (* volatility K1)))
            R2 (* (price e) (1+ (* volatility K2)))
            S1 (/ (price e) (1+ (* volatility K1)))
            S2 (/ (price e) (1+ (* volatility K2)))))))

(defmethod postprocess ((a opening-range-breakout) (e market-update))
  (with-slots (name counter volatility R1 R2 S1 S2 states positions pls) a
    (logv:format-log "Event ~S ~S Consumed for Agent ~S :~%"
            (timestamp e) (price e) name)
    (logv:format-log "Output: counter= ~S volatility= ~S R1= ~S R2= ~S S1= ~S S2= ~S
               State= ~S Position= ~S PL= ~S~%" counter volatility R1 R2 S1 S2
    (first states) (first positions) (first pls))))

;;EOF
