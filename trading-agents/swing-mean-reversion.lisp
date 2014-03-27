;;;; swing-mean-reversion.lisp

(in-package #:trading-core)

(defclass swing-mean-reversion (fsm-agent)
  ((event-count :accessor event-count :initarg :event-count)             ; Number of events needed to initialize box size
   (expected-width :accessor expected-width :initarg :expected-width)    ; Expected width of the breakout box
   (max-allowed-breakout :accessor max-allowed-breakout :initarg :max-allowed-breakout) ; Stop loss level beyond box width
   (counter :accessor counter :initform 0)
   (scale-factor :accessor scale-factor :initform 0)                     ; Exponention factor with same response as SMA
   (volatility :accessor volatility :initform 0)                         ; Calculated price volatility
   (L :accessor L :initform nil)       ; Lower channel (go long)
   (S :accessor S :initform nil)       ; Upper channel (go short)
   (max-price :accessor max-price :initform nil)
   (min-price :accessor min-price :initform nil)
   (SFL :accessor SFL :initform nil)   ; Stop from long value
   (SFS :accessor SFS :initform nil))) ; Stop from short value

;;; swing-mean-reversion methods

(defmethod initialize ((a swing-mean-reversion))
  (with-slots (counter event-count expected-width max-allowed-breakout scale-factor
                L S SFL SFS states name positions transitions) a
    (when (null states)
      (setf scale-factor (/ 2 (1+ event-count)))
      (push :init states)
      (setf name (format nil "SWING-MEAN-REVERSION_~A_~A" expected-width max-allowed-breakout))
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
                                       (declare (ignore p))
                                       (push 0 positions)))
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
                                       (declare (ignore p))
                                       (push 1 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :stop-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> counter event-count)
                                             (<= p SFL)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))
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
                                       (declare (ignore p))
                                       (push -1 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :stop-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> counter event-count)
                                             (>= p SFS)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))))
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
                                        (and (> p SFL) (< p S)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 1 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :stop-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (<= p SFL))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (>= p S) (< p SFS)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push -1 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :stop-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (>= p SFS))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))))
              (:stop-from-long . (,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :init
                                     :sensor #'price
                                     :predicate #1#
                                     :actuator #1#)
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :long
                                     :sensor #'price
                                     :predicate #1#
                                     :actuator #1#)
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :stop-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (< p S))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push 0 positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (>= p S) (< p SFS)))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push -1 positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :stop-from-short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (>= p SFS))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push 0 positions)))))
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
                                         (and (> p SFL) (<= p L)))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push 1 positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :stop-from-long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (<= p SFL))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push 0 positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and (> p L) (< p SFS)))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push -1 positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :stop-from-short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (> p SFS))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push 0 positions)))))
              (:stop-from-short . (,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :init
                                      :sensor #'price
                                      :predicate #1#
                                      :actuator #1#)
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :long
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (and (> p SFL) (<= p L)))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push 1 positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :stop-from-long
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (<= p SFL))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push 0 positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :short
                                      :sensor #'price
                                      :predicate #1#
                                      :actuator #1#)
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :stop-from-short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (> p L))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push 0 positions))))))))))

(defmethod preprocess ((a swing-mean-reversion) (e market-update))
  (with-slots (event-count counter scale-factor expected-width max-allowed-breakout
               min-price max-price volatility L S SFL SFS states revalprices) a
    (incf counter)
    (when (> counter 1)
      (let ((prev-p (second revalprices)))
        (setf volatility (+ (* scale-factor (abs (/ (- (price e) prev-p) prev-p)))
                            (* (- 1 scale-factor) volatility)))))
    (when (< counter event-count)  ;; skip the rest until we have enough data
      (return-from preprocess))
    (when (or (= counter event-count)
              (and (member (first states) '(:init :stop-from-long :stop-from-short))
                   (not (eql (first states) (second states)))))
      (setf L (/ (price e) (+ 1 (* volatility expected-width 1/2)))
            S (* (price e) (+ 1 (* volatility expected-width 1/2)))
            ;; Calculate SFL and SFS here to account for gaps
            min-price (price e)
            max-price (price e)
            SFL (/ (price e) (+ 1 (* volatility max-allowed-breakout)))
            SFS (* (price e) (+ 1 (* volatility max-allowed-breakout)))))
    (when (member (first states) '(:long :short))
      (when (not (eql (first states) (second states)))
        (setf SFL (/ (price e) (+ 1 (* volatility max-allowed-breakout)))
              SFS (* (price e) (+ 1 (* volatility max-allowed-breakout)))))
      (setf max-price (max max-price (price e))
            min-price (min min-price (price e))
            L (/ max-price (+ 1 (* volatility expected-width)))
            S (* min-price (+ 1 (* volatility expected-width)))))))

(defmethod postprocess ((a swing-mean-reversion) (e market-update))
  (call-next-method)
  (with-slots (counter L S SFL SFS states positions pls) a
    (logv:format-log "Output: counter= ~S L= ~S S= ~S SFL= ~S SFS= ~S State= ~S
               Position= ~S PL= ~S~%" counter L S SFL SFS (first states)
               (first positions) (first pls))))

;;EOF
