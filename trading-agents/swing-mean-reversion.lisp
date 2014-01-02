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
                                       (push 0 positions)
                                       (format t "~S INIT -> INIT ~%" name)))
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
                                       (format t "~S INIT -> LONG ~%" name)))
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
                                       (format t "~S INIT -> STOP-FROM-LONG ~%" name)))
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
                                       (format t "~S INIT -> SHORT ~%" name)))
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
                                       (format t "~S INIT -> STOP-FROM-SHORT ~%" name)))))
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
                                       (format t "~S LONG -> LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :stop-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (<= p SFL))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (format t "~S LONG -> STOP-FROM-LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (>= p S) (< p SFS)))
                           :actuator (lambda (p)
                                       (push -1 positions)
                                       (format t "~S LONG -> SHORT ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :stop-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (>= p SFS))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (format t "~S LONG -> STOP-FROM-SHORT ~%" name)))))
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
                                                 (format t "~S STOP-FROM-LONG -> STOP-FROM-LONG ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (>= p S) (< p SFS)))
                                     :actuator (lambda (p)
                                                 (push -1 positions)
                                                 (format t "~S STOP-FROM-LONG -> SHORT ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :stop-from-short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (>= p SFS))
                                     :actuator (lambda (p)
                                                 (push 0 positions)
                                                 (format t "~S STOP-FROM-LONG -> STOP-FROM-SHORT ~%" name)))))
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
                                        (format t "~S SHORT -> LONG ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :stop-from-long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (<= p SFL))
                            :actuator (lambda (p)
                                        (push 0 positions)
                                        (format t "~S SHORT -> STOP-FROM-LONG ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and (> p L) (< p SFS)))
                            :actuator (lambda (p)
                                        (push -1 positions)
                                        (format t "~S SHORT -> SHORT ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :stop-from-short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (> p SFS))
                            :actuator (lambda (p)
                                        (push 0 positions)
                                        (format t "~S SHORT -> STOP-FROM-SHORT ~%" name)))))
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
                                                  (format t "~S STOP-FROM-SHORT -> LONG ~%" name)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :stop-from-long
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (<= p SFL))
                                      :actuator (lambda (p)
                                                  (push 0 positions)
                                                  (format t "~S STOP-FROM-SHORT -> STOP-FROM-LONG ~%" name)))
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
                                                  (format t "~S STOP-FROM-SHORT -> STOP-FROM-SHORT ~%" name))))))))))

(defmethod preprocess ((a swing-mean-reversion) (e market-update))
  (with-slots (event-count counter scale-factor expected-width max-allowed-breakout
                volatility L S SFL SFS states revalprices) a
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

(defmethod set-fsm ((a swing-mean-reversion))
  (with-slots (states current-state) a
    (setf current-state (first states))))

(defmethod postprocess ((a swing-mean-reversion) (e market-update))
  (with-slots (name counter L S SFL SFS states positions pls) a
    (format t "Event ~S ~S Consumed for Agent ~S :~%"
            (timestamp e) (price e) name)
    (format t "Output: counter= ~S L= ~S S= ~S SFL= ~S SFS= ~S State= ~S
               Position= ~S PL= ~S~%" counter L S SFL SFS (first states)
               (first positions) (first pls))))

;;EOF
