;;;; swing-breakout.lisp

(in-package #:trading-core)

(defclass swing-breakout (fsm-agent)
  ((event-count :accessor event-count :initarg :event-count)             ; Number of events needed to initialize box size
   (expected-width :accessor expected-width :initarg :expected-width)    ; Expected width of the breakout box
   (price-extension :accessor price-extension :initarg :price-extension) ;
   (counter :accessor counter :initform 0)
   (scale-factor :accessor scale-factor :initform 0)                     ; Exponention factor with same response as SMA
   (volatility :accessor volatility :initform 0)                         ; Calculated price volatility
   (L :accessor L :initform nil)       ; Upper channel (go long)
   (S :accessor S :initform nil)       ; Lower channel (go short)
   (max-price :accessor max-price :initform nil)
   (min-price :accessor min-price :initform nil)
   (PFL :accessor PFL :initform nil)   ; Profit from long value
   (PFS :accessor PFS :initform nil))) ; Profit from short value

;;; swing-breakout methods

(defmethod initialize ((a swing-breakout))
  (with-slots (counter event-count expected-width price-extension scale-factor
                L S SFL SFS states name positions transitions) a
    (when (null states)
      (setf scale-factor (/ 2 (1+ event-count)))
      (push :init states)
      (setf name (format nil "SWING-BREAKOUT_~A_~A" expected-width price-extension))
      (setf transitions
            `((:init . (,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :init
                           :sensor #'price
                           :predicate (lambda (p)
                                        (or (<= counter event-count)
                                            (< S p L)))
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
                                             (>= p L)
                                             (< p PFL)))
                           :actuator (lambda (p)
                                       (push 1 positions)
                                       (logv:format-log "~S INIT -> LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :profit-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> counter event-count)
                                             (>= p PFL)))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (logv:format-log "~S INIT -> PROFIT-FROM-LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> counter event-count)
                                             (<= p S)
                                             (> p PFS)))
                           :actuator (lambda (p)
                                       (push -1 positions)
                                       (logv:format-log "~S INIT -> SHORT ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :profit-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> counter event-count)
                                             (<= p PFS)))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (logv:format-log "~S INIT -> PROFIT-FROM-SHORT ~%" name)))))
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
                                        (and (> p S) (< p PFL)))
                           :actuator (lambda (p)
                                       (push 1 positions)
                                       (logv:format-log "~S LONG -> LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :profit-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (>= p PFL))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (logv:format-log "~S LONG -> PROFIT-FROM-LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (<= p S) (> p PFS)))
                           :actuator (lambda (p)
                                       (push -1 positions)
                                       (logv:format-log "~S LONG -> SHORT ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :profit-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (<= p PFS))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (logv:format-log "~S LONG -> PROFIT-FROM-SHORT ~%" name)))))
              (:profit-from-long . (,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :init
                                       :sensor #'price
                                       :predicate (lambda (p) nil)
                                       :actuator (lambda (p) nil))
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :long
                                       :sensor #'price
                                       :predicate (lambda (p) nil)
                                       :actuator (lambda (p) nil))
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :profit-from-long
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (> p S))
                                       :actuator (lambda (p)
                                                   (push 0 positions)
                                                   (logv:format-log "~S PROFIT-FROM-LONG -> PROFIT-FROM-LONG ~%" name)))
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :short
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (and (<= p S) (> p PFS)))
                                       :actuator (lambda (p)
                                                   (push -1 positions)
                                                   (logv:format-log "~S PROFIT-FROM-LONG -> SHORT ~%" name)))
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :profit-from-short
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (<= p PFS))
                                       :actuator (lambda (p)
                                                   (push 0 positions)
                                                   (logv:format-log "~S PROFIT-FROM-LONG -> PROFIT-FROM-SHORT ~%" name)))))
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
                                         (and (>= p L) (< p PFL)))
                            :actuator (lambda (p)
                                        (push 1 positions)
                                        (logv:format-log "~S SHORT -> LONG ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :profit-from-long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (>= p PFL))
                            :actuator (lambda (p)
                                        (push 0 positions)
                                        (logv:format-log "~S SHORT -> PROFIT-FROM-LONG ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and (> p PFS) (< p SFS)))
                            :actuator (lambda (p)
                                        (push -1 positions)
                                        (logv:format-log "~S SHORT -> SHORT ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :profit-from-short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (< p PFS))
                            :actuator (lambda (p)
                                        (push 0 positions)
                                        (logv:format-log "~S SHORT -> PROFIT-FROM-SHORT ~%" name)))))
              (:profit-from-short . (,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :init
                                        :sensor #'price
                                        :predicate (lambda (p) nil)
                                        :actuator (lambda (p) nil))
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :long
                                        :sensor #'price
                                        :predicate (lambda (p)
                                                     (and (>= p L) (< p PFL)))
                                        :actuator (lambda (p)
                                                    (push 1 positions)
                                                    (logv:format-log "~S PROFIT-FROM-SHORT -> LONG ~%" name)))
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :profit-from-long
                                        :sensor #'price
                                        :predicate (lambda (p)
                                                     (>= p PFL))
                                        :actuator (lambda (p)
                                                    (push 0 positions)
                                                    (logv:format-log "~S PROFIT-FROM-SHORT -> PROFIT-FROM-LONG ~%" name)))
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :short
                                        :sensor #'price
                                        :predicate (lambda (p) nil)
                                        :actuator (lambda (p) nil))
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :profit-from-short
                                        :sensor #'price
                                        :predicate (lambda (p)
                                                     (< p L))
                                        :actuator (lambda (p)
                                                    (push 0 positions)
                                                    (logv:format-log "~S PROFIT-FROM-SHORT -> PROFIT-FROM-SHORT ~%" name))))))))))

(defmethod preprocess ((a swing-breakout) (e market-update))
  (with-slots (event-count counter scale-factor expected-width price-extension
                volatility L S PFL PFS states revalprices) a
    (incf counter)
    (when (> counter 1)
      (let ((prev-p (second revalprices)))
      (setf volatility (+ (* scale-factor (abs (/ (- (price e) prev-p) prev-p)))
                          (* (- 1 scale-factor) volatility)))))
    (when (< counter event-count)  ;; skip the rest until we have enough data
      (return-from preprocess))
    (when (or (= counter event-count)
              (and (member (first states) '(:init :profit-from-long :profit-from-short))
                   (not (eql (first states) (second states)))))
      (setf L (* (price e) (+ 1 (* volatility expected-width 1/2)))
            S (/ (price e) (+ 1 (* volatility expected-width 1/2)))
            ;; Calculate PFL and PFS here to account for gaps
            min-price (price e)
            max-price (price e)
            PFL (* (price e) (+ 1 (* volatility price-extension)))
            PFS (/ (price e) (+ 1 (* volatility price-extension)))))
    (when (member (first states) '(:long :short))
      (when (not (eql (first states) (second states)))
        (setf PFL (* (price e) (+ 1 (* volatility price-extension)))
              PFS (/ (price e) (+ 1 (* volatility price-extension)))))
      (setf max-price (max max-price (price e))
            min-price (min min-price (price e))
            L (* min-price (+ 1 (* volatility expected-width)))
            S (/ max-price (+ 1 (* volatility expected-width)))))))

(defmethod postprocess ((a swing-breakout) (e market-update))
  (with-slots (name counter L S PFL PFS states positions pls) a
    (logv:format-log "Event ~S ~S Consumed for Agent ~S :~%"
            (timestamp e) (price e) name)
    (logv:format-log "Output: counter= ~S L= ~S S= ~S PFL= ~S PFS= ~S State= ~S
               Position= ~S PL= ~S~%" counter L S PFL PFS (first states)
               (first positions) (first pls))))

;;EOF
