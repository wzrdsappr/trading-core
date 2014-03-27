;;;; channel-breakout-trend-following.lisp

(in-package #:trading-core)

(defclass channel-breakout-trend-following (fsm-agent)
  ((slow-channel-length :accessor slow-channel-length :initarg :slow-channel-length)
   (fast-channel-length :accessor fast-channel-length :initarg :fast-channel-length)
   (L :accessor L :initform most-negative-fixnum) ; Slow highest high (enter long)
   (S :accessor S :initform most-positive-fixnum) ; Slow lowest low   (enter short)
   (SFL :accessor SFL :initform most-negative-fixnum) ; Fast lowest low   (stop from long)
   (SFS :accessor SFS :initform most-positive-fixnum) ; Fast highest high (stop from short)
   (PFL :accessor PFL :initform nil) ; Profit from long value, 2R above L
   (PFS :accessor PFS :initform nil) ; Profit from short value, 2R below S
   (counter :accessor counter :initform 0)))

(defmethod initialize ((a channel-breakout-trend-following))
  (with-slots (fast-channel-length slow-channel-length L S SFL SFS PFL PFS positions states
               name counter transitions) a
    (when (null states)
      (push :init states)
      (setf name (format nil "CHANNEL-BREAKOUT-TREND-FOLLOWING_~A_~A"
                         fast-channel-length slow-channel-length))
      (setf transitions
            `((:init . (,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :init
                           :sensor #'price
                           :predicate (lambda (p)
                                        (or (<= counter slow-channel-length) (< S p L)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> counter slow-channel-length) (>= p L) (< p PFL)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 1 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :stop-from-long
                           :sensor #'price
                           :predicate #1=(lambda (p)
                                           (declare (ignore p))
                                           nil)
                           :actuator #1#)
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :profit-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> counter slow-channel-length) (>= p PFL)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 1 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> counter slow-channel-length) (<= p S) (> p PFS)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push -1 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :stop-from-short
                           :sensor #'price
                           :predicate #1#
                           :actuator #1#)
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :profit-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> counter slow-channel-length) (<= p PFS)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push -1 positions)))))
              (:long . (,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :init
                           :sensor #'price
                           :predicate #1#
                           :actuator #1#)
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> p SFL) (< p PFL)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 1 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :stop-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (> p S) (<= p SFL)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :profit-from-long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (>= p PFL))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 2 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (<= p S) (> p PFS)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push -1 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :stop-from-short
                           :sensor #'price
                           :predicate #1#
                           :actuator #1#)
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :profit-from-short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (<= p PFS))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push -1 positions)))))
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
                                     :predicate (lambda (p)
                                                  (and (>= p L) (< p PFL)))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push 1 positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :stop-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (< S p L))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push 0 positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :profit-from-long
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (>= p PFL))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push 1 positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (> p PFS) (<= p S)))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push -1 positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :stop-from-short
                                     :sensor #'price
                                     :predicate #1#
                                     :actuator #1#)
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :profit-from-short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (<= p PFS))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push -1 positions)))))
              (:profit-from-long . (,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :init
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :long
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :stop-from-long
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :profit-from-long
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (> p S))
                                       :actuator (lambda (p)
                                                   (declare (ignore p))
                                                   (push (car positions) positions)))
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :short
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (and (> p PFS) (<= p S)))
                                       :actuator (lambda (p)
                                                   (declare (ignore p))
                                                   (push -1 positions)))
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :stop-from-short
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :profit-from-short
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (<= p PFS))
                                       :actuator (lambda (p)
                                                   (declare (ignore p))
                                                   (push -1 positions)))))
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
                                         (and (>= p L) (< p PFL)))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push 1 positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :stop-from-long
                            :sensor #'price
                            :predicate #1#
                            :actuator #1#)
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :profit-from-long
                            :sensor #'price
                            :predicate (lambda (p)
                                         (>= p PFL))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push 1 positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (< PFS p SFS))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push -1 positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :stop-from-short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (and (>= p SFS) (< p L)))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push 0 positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :profit-from-short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (<= p PFS))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push 2 positions)))))
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
                                                    (and (>= p L) (< PFL)))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push 1 positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :stop-from-long
                                      :sensor #'price
                                      :predicate #1#
                                      :actuator #1#)
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :profit-from-long
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                    (>= p PFL))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push 1 positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                    (and (<= p S) (> p PFS)))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push -1 positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :stop-from-short
                                      :sensor #'price
                                      :predicate #1#
                                      :actuator #1#)
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :profit-from-short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                    (<= p PFS))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push -1 positions)))))
              (:profit-from-short . (,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :init
                                        :sensor #'price
                                        :predicate #1#
                                        :actuator #1#)
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :long
                                        :sensor #'price
                                        :predicate (lambda (p)
                                                     (and (>= p L) (< p PFL)))
                                        :actuator (lambda (p)
                                                    (declare (ignore p))
                                                    (push 1 positions)))
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :stop-from-long
                                        :sensor #'price
                                        :predicate #1#
                                        :actuator #1#)
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :profit-from-long
                                        :sensor #'price
                                        :predicate (lambda (p)
                                                     (>= p PFL))
                                        :actuator (lambda (p)
                                                    (declare (ignore p))
                                                    (push 1 positions)))
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :short
                                        :sensor #'price
                                        :predicate #1#
                                        :actuator #1#)
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :stop-from-short
                                        :sensor #'price
                                        :predicate #1#
                                        :actuator #1#)
                                     ,(make-instance
                                        'transition
                                        :initial-state :profit-from-short
                                        :final-state   :profit-from-short
                                        :sensor #'price
                                        :predicate (lambda (p)
                                                     (< p L))
                                        :actuator (lambda (p)
                                                    (declare (ignore p))
                                                    (push (car positions) positions))))))))))

(defmethod preprocess ((a channel-breakout-trend-following) (e market-update))
  (with-slots (slow-channel-length fast-channel-length L S PFL PFS SFL SFS
               counter revalprices current-state) a
    (incf counter)
    (when (> counter slow-channel-length)
      (let ((slow-channel-prices (subseq revalprices 1 slow-channel-length))
            (fast-channel-prices (subseq revalprices 1 fast-channel-length)))
        (setf L (reduce #'max slow-channel-prices)
              S (reduce #'min slow-channel-prices))
        (setf SFS (reduce #'max fast-channel-prices)
              SFL (reduce #'min fast-channel-prices))
        (unless (member current-state '(:long :short))
          (setf PFL (+ L (* 2 (- L SFL))))
          (setf PFS (+ S (* 2 (- S SFS)))))))))

(defmethod postprocess ((a channel-breakout-trend-following) (e market-update))
  (call-next-method)
  (with-slots (slow-channel-length fast-channel-length states positions pls) a
    (logv:format-log "Output: Slow-Channel= ~S Fast Channel= ~S~%~
               State= ~S Position= ~S PL= ~S~%"
            slow-channel-length fast-channel-length
            (first states) (first positions) (first pls))))

;;EOF
