;;;; range-projection-mean-reversion.lisp

(in-package #:trading-core)

(defclass range-projection-mean-reversion (fsm-agent)
  ((N :accessor N :initarg :N)                                           ; Number of bars used to smooth price range
   (expected-width :accessor expected-width :initarg :expected-width)    ; Expected width of the breakout box
   (max-allowed-breakout :accessor max-allowed-breakout :initarg :max-allowed-breakout) ; Stop loss level beyond box width
   (counter :accessor counter :initform 0)
   (scale-factor :accessor scale-factor :initform 0)               ; Exponention factor with same response as SMA
   (pivot-ma :accessor pivot-ma :initarg :pivot-ma :initform nil)  ; Pivot moving average
   (tr-ma :accessor tr-ma :initform 0)                             ; Trading range moving average
   (pivot-ma-history :accessor pivot-ma-history :initform nil)     ; Previous Pivot MA values, used for calc. of channel center
   (L :accessor L :initform nil)       ; Lower channel (go long)
   (S :accessor S :initform nil)       ; Upper channel (go short)
   (SFL :accessor SFL :initform nil)   ; Stop from long value
   (SFS :accessor SFS :initform nil))) ; Stop from short value

;;; range-projection-mean-reversion methods

(defmethod initialize ((a range-projection-mean-reversion))
  (with-slots (scale-factor pivot-ma-history counter L S SFL SFS states name positions transitions) a
    (when (null states)
      (setf scale-factor (/ 2 (1+ event-count))
            pivot-ma-history (make-array N :initial-element 0))
      (push :init states)
      (setf name (format nil "RANGE-PROJECTION-MEAN-REVERSION_~A_~A" expected-width max-allowed-breakout))
      (setf transitions
            `((:init . (,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :init
                           :sensor #'price
                           :predicate (lambda (p)
                                        (or (<= counter event-count)
                                            (< L p S)))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (logv:format-log "~S INIT -> INIT ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> counter event-count)
                                             (<= p L)
                                             (> p SFL)))
                           :actuator (lambda (p)
                                       (push 1 positions)
                                       (logv:format-log "~S INIT -> LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :stop-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> counter event-count)
                                             (<= p SFL)))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (logv:format-log "~S INIT -> STOP-FROM-LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> counter event-count)
                                             (>= p S)
                                             (< p SFS)))
                           :actuator (lambda (p)
                                       (push -1 positions)
                                       (logv:format-log "~S INIT -> SHORT ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :stop-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> counter event-count)
                                             (>= p SFS)))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (logv:format-log "~S INIT -> STOP-FROM-SHORT ~%" name)))))
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
                                        (and (> p SFL) (< p S)))
                           :actuator (lambda (p)
                                       (push 1 positions)
                                       (logv:format-log "~S LONG -> LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :stop-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (<= p SFL))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (logv:format-log "~S LONG -> STOP-FROM-LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (>= p S) (< p SFS)))
                           :actuator (lambda (p)
                                       (push -1 positions)
                                       (logv:format-log "~S LONG -> SHORT ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :stop-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (>= p SFS))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (logv:format-log "~S LONG -> STOP-FROM-SHORT ~%" name)))))
              (:stop-from-long . (,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :init
                                     :sensor #'price
                                     :predicate (lambda (p) nil)
                                     :actuator (lambda (p) nil))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :long
                                     :sensor #'price
                                     :predicate (lambda (p) nil)
                                     :actuator (lambda (p) nil))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :stop-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (< p S))
                                     :actuator (lambda (p)
                                                 (push 0 positions)
                                                 (logv:format-log "~S STOP-FROM-LONG -> STOP-FROM-LONG ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (>= p S) (< p SFS)))
                                     :actuator (lambda (p)
                                                 (push -1 positions)
                                                 (logv:format-log "~S STOP-FROM-LONG -> SHORT ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :stop-from-short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (>= p SFS))
                                     :actuator (lambda (p)
                                                 (push 0 positions)
                                                 (logv:format-log "~S STOP-FROM-LONG -> STOP-FROM-SHORT ~%" name)))))
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
                                         (and (> p SFL) (<= p L)))
                            :actuator (lambda (p)
                                        (push 1 positions)
                                        (logv:format-log "~S SHORT -> LONG ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :stop-from-long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (<= p SFL))
                            :actuator (lambda (p)
                                        (push 0 positions)
                                        (logv:format-log "~S SHORT -> STOP-FROM-LONG ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and (> p L) (< p SFS)))
                            :actuator (lambda (p)
                                        (push -1 positions)
                                        (logv:format-log "~S SHORT -> SHORT ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :stop-from-short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (> p SFS))
                            :actuator (lambda (p)
                                        (push 0 positions)
                                        (logv:format-log "~S SHORT -> STOP-FROM-SHORT ~%" name)))))
              (:stop-from-short . (,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :init
                                      :sensor #'price
                                      :predicate (lambda (p) nil)
                                      :actuator (lambda (p) nil))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :long
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (and (> p SFL) (<= p L)))
                                      :actuator (lambda (p)
                                                  (push 1 positions)
                                                  (logv:format-log "~S STOP-FROM-SHORT -> LONG ~%" name)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :stop-from-long
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (<= p SFL))
                                      :actuator (lambda (p)
                                                  (push 0 positions)
                                                  (logv:format-log "~S STOP-FROM-SHORT -> STOP-FROM-LONG ~%" name)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :short
                                      :sensor #'price
                                      :predicate (lambda (p) nil)
                                      :actuator (lambda (p) nil))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :stop-from-short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (> p L))
                                      :actuator (lambda (p)
                                                  (push 0 positions)
                                                  (logv:format-log "~S STOP-FROM-SHORT -> STOP-FROM-SHORT ~%" name))))))))))

(defmethod preprocess ((a range-projection-mean-reversion) (e bar))
  (with-slots (N counter scale-factor pivot-ma-history tr-ma L S revalprices) a
    (incf counter)
    (let ((pivot-ma-history-index (mod (1- counter) N))
          (trading-range (* 2 (- (h e) (l e)) (+ (h e) (l e))))
          (channel-center (+ (o e) (/ pivot-ma (aref pivot-ma-history pivot-ma-history-index) N)))
          (prev-tr-ma tr-ma))
      (setf pivot-ma (+ (* scale-factor (pivot e))
                          (* (- 1 scale-factor) pivot-ma))
            (aref pivot-ma-history pivot-ma-history-index) pivot-ma
            tr-ma (+ (* scale-factor trading-range) (* (- 1 scale-factor) tr-ma)))
      (when (>= counter event-count)
        (setf L (/ channel-center (1+ (* prev-tr-ma 1/2)))
              S (* channel-center (1+ (* prev-tr-ma 1/2)))
              SFL (/ channel-center (1+ (* prev-tr-ma 2/3)))
              SFS (* channel-center (1+ (* prev-tr-ma 2/3))))))))

(defmethod postprocess ((a range-projection-mean-reversion) (e bar))
  (with-slots (name counter L S SFL SFS states positions pls) a
    (logv:format-log "Event ~S ~S Consumed for Agent ~S :~%"
            (timestamp e) (pivot e) name)
    (logv:format-log "Output: L= ~S S= ~S SFL= ~S SFS= ~S State= ~S
               Position= ~S PL= ~S~%" L S SFL SFS (first states)
               (first positions) (first pls))))

;;EOF
