;;;; adaptive-moving-avg-trend-following.lisp

(in-package #:trading-core)

(defclass adaptive-moving-avg-trend-following (fsm-agent)
  ((n-min :accessor n-min :initarg :n-min)      ; minimum length of moving average
   (n-max :accessor n-max :initarg :n-max)      ; maximum length of moving average
   (width-factor :accessor width-factor :initarg :width-factor)   ; scaling factor for channel width
   (snr-factor :accessor snr-factor :initarg :snr-factor)         ; scaling factor for signal-to-noise ratio
   (sensitivity-min :accessor sensitivity-min)  ; minimum EMA sensitivity
   (sensitivity-max :accessor sensitivity-max)  ; maximum EMA sensitivity
   (sensitivity :accessor sensitivity)          ; current (adaptive) EMA sensitivity
   (L :accessor L :initform most-positive-fixnum)       ; Upper channel (go long)
   (S :accessor S :initform most-negative-fixnum)       ; Lower channel (go short)
   (d+ :accessor d+ :initform 1)       ; AMA of upside deviation
   (d- :accessor d- :initform -1)       ; AMA of downside deviation
   (ama :accessor ama :initform 0)     ; Adaptive moving average of price
   (snr :accessor snr :initform 0)     ; Signal-to-noise ratio
   (SFL :accessor SFL :initform most-negative-fixnum) ; stop from long
   (SFS :accessor SFS :initform most-positive-fixnum) ; Fast highest high (stop from short)
   (PFL :accessor PFL :initform nil)   ; Profit from long value, 2R above L
   (PFS :accessor PFS :initform nil))) ; Profit from short value, 2R below S

;;; adaptive-moving-avg-trend-following methods

(defmethod initialize ((a adaptive-moving-avg-trend-following))
  (with-slots (n-min n-max sensitivity-min sensitivity-max sensitivity width-factor
                L S PFL PFS SFL SFS states name positions transitions) a
    (assert (< n-min n-max))
    (setf sensitivity-min (/ 2 (1+ n-min))
          sensitivity-max (/ 2 (1+ n-max))
          sensitivity (+ sensitivity-min (/ (- sensitivity-max sensitivity-min) 2)))
    (when (null states)
      (push :init states)
      (setf name (format nil "ADAPTIVE-MOVING-AVG-TREND-FOLLOWING_~A_~A_~A" n-min n-max width-factor))
      (setf transitions
            `((:init . (,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :init
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and S L (< S p L)))
                           :actuator (lambda (p)
                                       (push 0 positions)
                                       (format t "~S INIT -> INIT ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and L PFL (>= p L) (< p PFL)))
                           :actuator (lambda (p)
                                       (push 1 positions)
                                       (format t "~S INIT -> LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :stop-from-long
                           :sensor #'price
                           :predicate (lambda (p) nil)
                           :actuator (lambda (p) nil))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :profit-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and PFL (>= p PFL)))
                           :actuator (lambda (p)
                                       (push 1 positions)
                                       (format t "~S INIT -> PROFIT-FROM-LONG ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and S (<= p S)))
                           :actuator (lambda (p)
                                       (push -1 positions)
                                       (format t "~S INIT -> SHORT ~%" name)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :stop-from-short
                           :sensor #'price
                           :predicate (lambda (p) nil)
                           :actuator (lambda (p) nil))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :profit-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and PFS (<= p PFS)))
                           :actuator (lambda (p)
                                       (format t "~S INIT -> PROFIT-FROM-SHORT ~%" name)))))
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
                                                  (and (> p SFL) (< p PFL)))
                                     :actuator (lambda (p)
                                                 (push 1 positions)
                                                 (format t "~S LONG -> LONG ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :long
                                     :final-state   :stop-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (> p S) (<= p SFL)))
                                     :actuator (lambda (p)
                                                 (push 0 positions)
                                                 (format t "~S LONG -> STOP-FROM-LONG ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :long
                                     :final-state   :profit-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (>= p PFL))
                                     :actuator (lambda (p)
                                                 (push 2 positions)
                                                 (format t "~S LONG -> PROFIT-FROM-LONG ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :long
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (<= p S) ()))
                                     :actuator (lambda (p)
                                                 (push -1 positions)
                                                 (format t "~S LONG -> SHORT ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :long
                                     :final-state   :stop-from-short
                                     :sensor #'price
                                     :predicate (lambda (p) nil)
                                     :actuator (lambda (p) nil))
                                  ,(make-instance
                                     'transition
                                     :initial-state :long
                                     :final-state   :profit-from-short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (<= p PFS))
                                     :actuator (lambda (p)
                                                 (push -1 positions)
                                                 (format t "~S LONG -> PROFIT-FROM-SHORT ~%" name)))))
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
                                     :predicate (lambda (p)
                                                  (and (>= p L) (< p PFL)))
                                     :actuator (lambda (p)
                                                 (push 1 positions)
                                                 (format t "~S STOP-FROM-LONG -> LONG ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :stop-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (< S p L))
                                     :actuator (lambda (p)
                                                 (push 0 positions)
                                                 (format t "~S STOP-FROM-LONG -> STOP-FROM-LONG ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :profit-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (>= p PFL))
                                     :actuator (lambda (p)
                                                 (push 1 positions)
                                                 (format t "~S STOP-FROM-LONG -> PROFIT-FROM-LONG ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (<= p S) (> p PFS)))
                                     :actuator (lambda (p)
                                                 (push -1 positions)
                                                 (format t "~S STOP-FROM-LONG -> SHORT ~%" name)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :stop-from-short
                                     :sensor #'price
                                     :predicate (lambda (p) nil)
                                     :actuator (lambda (p) nil))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :profit-from-short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (<= p PFS))
                                     :actuator (lambda (p)
                                                 (push -1 positions)
                                                 (format t "~S STOP-FROM-LONG -> PROFIT-FROM-SHORT ~%" name)))))
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
                                       :final-state   :stop-from-long
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
                                                   (push (first positions) positions)
                                                   (format t "~S PROFIT-FROM-LONG -> PROFIT-FROM-LONG ~%" name)))
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :short
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (and (<= p S) (> p PFS)))
                                       :actuator (lambda (p)
                                                   (push -1 positions)
                                                   (format t "~S PROFIT-FROM-LONG -> SHORT ~%" name)))
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :stop-from-short
                                       :sensor #'price
                                       :predicate (lambda (p) nil)
                                       :actuator (lambda (p) nil))
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :profit-from-short
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (<= p PFS))
                                       :actuator (lambda (p)
                                                   (push -1 positions)
                                                   (format t "~S PROFIT-FROM-LONG -> PROFIT-FROM-SHORT ~%" name)))))
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
                                        (format t "~S SHORT -> LONG ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :stop-from-long
                            :sensor #'price
                            :predicate (lambda (p) nil)
                            :actuator (lambda (p) nil))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :profit-from-long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (>= p PFL))
                            :actuator (lambda (p)
                                        (push 1 positions)
                                        (format t "~S SHORT -> PROFIT-FROM-LONG ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and (> p PFS) (< p SFS)))
                            :actuator (lambda (p)
                                        (push -1 positions)
                                        (format t "~S SHORT -> SHORT ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :stop-from-short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and (> p SFS) (< p L)))
                            :actuator (lambda (p)
                                        (push 0 positions)
                                        (format t "~S SHORT -> STOP-FROM-SHORT ~%" name)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :profit-from-short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (< p PFS))
                            :actuator (lambda (p)
                                        (push 1 positions)
                                        (format t "~S SHORT -> PROFIT-FROM-SHORT ~%" name)))))
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
                                                   (and (>= p L) (< p PFL)))
                                      :actuator (lambda (p)
                                                  (push 1 positions)
                                                  (format t "~S STOP-FROM-SHORT -> LONG ~%" name)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :stop-from-long
                                      :sensor #'price
                                      :predicate (lambda (p) nil)
                                      :actuator (lambda (p) nil))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :profit-from-long
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (>= p PFL))
                                      :actuator (lambda (p)
                                                  (push 1 positions)
                                                  (format t "~S STOP-FROM-SHORT -> PROFIT-FROM-LONG ~%" name)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (and (<= p S) (> p PFS)))
                                      :actuator (lambda (p)
                                                  (push -1 positions)
                                                  (format t "~S STOP-FROM-SHORT -> SHORT ~%" name)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :stop-from-short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (< S p L))
                                      :actuator (lambda (p)
                                                  (push 0 positions)
                                                  (format t "~S STOP-FROM-SHORT -> STOP-FROM-SHORT ~%" name)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :profit-from-short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (<= p PFS))
                                      :actuator (lambda (p)
                                                  (push -1 positions)
                                                  (format t "~S STOP-FROM-SHORT -> PROFIT-FROM-SHORT ~%" name)))))
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
                                                    (format t "~S PROFIT-FROM-SHORT -> LONG ~%" name)))
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :stop-from-long
                                        :sensor #'price
                                        :predicate (lambda (p) nil)
                                        :actuator (lambda (p) nil))
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :profit-from-long
                                        :sensor #'price
                                        :predicate (lambda (p)
                                                     (>= p PFL))
                                        :actuator (lambda (p)
                                                    (push 1 positions)
                                                    (format t "~S PROFIT-FROM-SHORT -> PROFIT-FROM-LONG ~%" name)))
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
                                        :final-state   :stop-from-short
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
                                                    (push (first positions) positions)
                                                    (format t "~S PROFIT-FROM-SHORT -> PROFIT-FROM-SHORT ~%" name))))))))))

(defmethod preprocess ((a adaptive-moving-avg-trend-following) (e market-update))
  (with-slots (sensitivity sensitivity-min sensitivity-max width-factor snr-factor ama
                d+ d- snr S L PFL PFS SFL SFS states revalprices) a
    (unless (null (second revalprices))
      (let ((prev-p (second revalprices))
            (prev-d+ d+)
            (prev-d- d-)
            (prev-ama ama)
            (prev-sensitivity sensitivity))
        (setf d+ (+ (* prev-sensitivity (max (/ (- (price e) prev-p) prev-p) 0))
                    (* (- 1 prev-sensitivity) prev-d+))
              d- (+ (* -1 prev-sensitivity (min (/ (- (price e) prev-p) prev-p) 0))
                    (* (- 1 prev-sensitivity) prev-d-)))
        (setf ama (if (null ama)
                    (price e)
                    (+ (* prev-sensitivity (price e)) (* (- 1 prev-sensitivity) prev-ama))))
        (let ((scale-factor- (* width-factor prev-d-))
              (scale-factor+ (* width-factor prev-d+)))
          (setf L (* (+ 1 scale-factor-) ama)
                S (* (- 1 scale-factor+) ama)
                SFL (if (< prev-p SFL) (- L (* d- 2)) (max SFL (- L (* d- 2))))
                SFS (if (> prev-p SFS) (+ S (* d+ 2)) (min SFS (+ S (* d+ 2)))))
          (setf snr (cond ((or (null prev-ama) (= prev-ama 0)) 0)
                          ((> (price e) L)
                           (/ (- (price e) prev-ama) prev-ama scale-factor-))
                          ((< (price e) S)
                           (/ (* -1 (- (price e) prev-ama)) prev-ama scale-factor+))
                          (t 0)))))
      (setf sensitivity (+ sensitivity-min (* (- sensitivity-max sensitivity-min)
                                              (atan (* snr-factor snr)))))
      (unless (member (first states) '(:long :short))
        (setf PFL (+ L (* 2 (- L SFL)))
              PFS (- S (* 2 (- SFS S))))))))

(defmethod set-fsm ((a adaptive-moving-avg-trend-following))
  (with-slots (states current-state) a
    (setf current-state (first states))))

(defmethod postprocess ((a adaptive-moving-avg-trend-following) (e market-update))
  (with-slots (name ama  L S PFL PFS states positions pls) a
    (format t "Event ~S ~S Consumed for Agent ~S :~%"
            (timestamp e) (price e) name)
    (format t "Output: AMA= ~S L= ~S S= ~S PFL= ~S PFS= ~S
               State= ~S Position= ~S PL= ~S~%" ama L S PFL PFS
            (first states) (first positions) (first pls))))

;;EOF
