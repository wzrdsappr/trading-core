;;;; channel-breakout-trend-following.lisp

(in-package #:trading-core)

(defclass channel-breakout-trend-following (fsm-agent)
  ((slow-period :accessor slow-period :initarg :slow-period)
   (fast-period :accessor fast-period :initarg :fast-period)
   (slow-channel :type 'donchian-channel)
   (fast-channel :type 'donchian-channel)
   (L :accessor L)          ; Slow highest high (enter long)
   (S :accessor S)          ; Slow lowest low   (enter short)
   (SFL :accessor SFL)      ; Fast lowest low   (stop from long)
   (SFS :accessor SFS)      ; Fast highest high (stop from short)
   (PFL :accessor PFL)      ; Profit from long value, 2R above L
   (PFS :accessor PFS)))    ; Profit from short value, 2R below S

(defmethod initialize ((a channel-breakout-trend-following))
  (with-slots (fast-channel slow-channel fast-period slow-period
               initialized L S SFL SFS PFL PFS long-size short-size positions states name transitions) a
    (assert (> slow-period fast-period 0))
    (when (null states)
      (push :init states)
      (setf fast-channel (make-instance 'donchian-channel
                                        :period fast-period)
            slow-channel (make-instance 'donchian-channel
                                        :period slow-period)
            name (format nil "CBTF_~A_~A"
                         fast-period slow-period))
      (setf transitions
            `((:init . (,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :init
                           :sensor #'price
                           :predicate (lambda (p)
                                        (or (not initialized) (< S p L)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push 0 positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :long
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and initialized (>= p L) (< p PFL)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push long-size positions)))
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
                                        (and initialized (>= p PFL)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push long-size positions)))
                        ,(make-instance
                           'transition
                           :initial-state :init
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and initialized (<= p S) (> p PFS)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push short-size positions)))
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
                                        (and initialized (<= p PFS)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push short-size positions)))))
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
                                       (push long-size positions)))
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
                                       (push (* 2 long-size) positions)))
                        ,(make-instance
                           'transition
                           :initial-state :long
                           :final-state   :short
                           :sensor #'price
                           :predicate (lambda (p)
                                        (and (<= p S) (> p PFS)))
                           :actuator (lambda (p)
                                       (declare (ignore p))
                                       (push short-size positions)))
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
                                       (push short-size positions)))))
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
                                                 (push long-size positions)))
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
                                                 (push long-size positions)))
                                  ,(make-instance
                                     'transition
                                     :initial-state :stop-from-long
                                     :final-state   :short
                                     :sensor #'price
                                     :predicate (lambda (p)
                                                  (and (> p PFS) (<= p S)))
                                     :actuator (lambda (p)
                                                 (declare (ignore p))
                                                 (push short-size positions)))
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
                                                 (push short-size positions)))))
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
                                                   (push (original-position (first positions)) positions)))
                                    ,(make-instance
                                       'transition
                                       :initial-state :profit-from-long
                                       :final-state   :short
                                       :sensor #'price
                                       :predicate (lambda (p)
                                                    (and (> p PFS) (<= p S)))
                                       :actuator (lambda (p)
                                                   (declare (ignore p))
                                                   (push short-size positions)))
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
                                         (and (>= p L) (< p PFL)))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push long-size positions)))
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
                                        (push long-size positions)))
                         ,(make-instance
                            'transition
                            :initial-state :short
                            :final-state   :short
                            :sensor #'price
                            :predicate (lambda (p)
                                         (< PFS p SFS))
                            :actuator (lambda (p)
                                        (declare (ignore p))
                                        (push short-size positions)))
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
                                        (push (* 2 short-size) positions)))))
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
                                                  (push long-size positions)))
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
                                                  (push long-size positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (and (<= p S) (> p PFS)))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push short-size positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :stop-from-short
                                      :sensor #'price
                                      :predicate(lambda (p)
                                                  (< S p L))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push 0 positions)))
                                   ,(make-instance
                                      'transition
                                      :initial-state :stop-from-short
                                      :final-state   :profit-from-short
                                      :sensor #'price
                                      :predicate (lambda (p)
                                                   (<= p PFS))
                                      :actuator (lambda (p)
                                                  (declare (ignore p))
                                                  (push short-size positions)))))
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
                                                    (push long-size positions)))
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
                                                    (push long-size positions)))
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
                                                    (push (original-position (first positions)) positions))))))))))

(defmethod preprocess ((a channel-breakout-trend-following) (e market-update))
  (with-slots (slow-channel fast-channel L S PFL PFS SFL SFS
               initialized current-state indicators) a
    (update-indicator slow-channel (price e))
    (update-indicator fast-channel (price e))
    (when (and (not initialized)
               (initialized slow-channel)
               (initialized fast-channel))
      (setf initialized t))
    (setf L (upper-band slow-channel)
          S (lower-band slow-channel)
          SFS (upper-band fast-channel)
          SFL (lower-band fast-channel))
    (cond ((member current-state '(:long :profit-from-long))
           (setf PFL (max PFL L)))
          ((member current-state '(:short :profit-from-short))
           (setf PFS (min PFS S)))
          (t (setf PFL (+ L (- L SFL))
                   PFS (+ S (- S SFS)))))
    (push (list L S SFL SFS PFL PFS) indicators)))

(defmethod postprocess ((a channel-breakout-trend-following) (e market-update))
  (call-next-method)
  (with-slots (slow-period fast-period states positions pls) a
    (logv:format-log "Output: Slow-Channel= ~S Fast Channel= ~S~%~
               State= ~S Position= ~S PL= ~S~%"
            slow-period fast-period (first states) (first positions) (first pls))))

(defmethod extract-context-data ((a channel-breakout-trend-following))
  "Returns the indicators that should be displayed on the price chart as context data for analysis output."
  `(,@(call-next-method)                       ;; Get the generic agent context data relevant to any agent
    (:indicators . ,(extract-indicators a ("L" "S" "SFL" "SFS" "PFL" "PFS")))))

;;EOF
