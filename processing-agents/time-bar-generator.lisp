;;;; time-bar-generator.lisp

(in-package #:trading-core)

(defclass time-bar-generator (fsm-agent)
  ((time-unit :accessor time-unit :initarg :time-unit :initform :day
    :type (member :month :week :day :hour :minute :sec))
   (num-time-units :initarg :num-time-units :initform 1
    :type (integer 1 *))
   (current-bar-timestamp :initform nil)
   (op :accessor op :initform nil)
   (hi :accessor hi :initform nil)
   (lo :accessor lo :initform nil)
   (cl :accessor cl :initform nil))
  (:documentation "Agent that creates compressed time bars for consumption by other agents."))


;;; time-bar-generator methods

(defmethod observe ((a time-bar-generator) (e market-update))
  (and (equal (security a) (security e))
       (or (not (typep e 'time-bar))
           (and (typep e 'time-bar)
                (let ((time-units '(:month :week :day :hour :minute :sec)))
                  (or (< (position (time-unit a) time-units)
                         (position (time-unit e) time-units))
                      (and (= (position (time-unit a) time-units)
                              (position (time-unit e) time-units))
                           (> (num-time-units a) (num-time-units e)))))))))

(defgeneric initialize-price (agent price)
  (:documentation "Initialize the fields used to compress prices"))

(defmethod initialize-price ((a time-bar-generator) (e prc))
  (with-slots (op hi lo cl) a
    (setf op e
          hi e
          lo e
          cl e)))

(defmethod initialize-price ((a time-bar-generator) (e bar))
  (with-slots (current-bar-timestamp op hi lo cl) a
    (setf current-bar-timestamp (get-bar-timestamp a e)
          op (o e)
          hi (h e)
          lo (l e)
          cl (c e))))

(defgeneric compress-price (agent price)
  (:documentation "Compress a price"))

(defmethod compress-price ((a time-bar-generator) (e prc))
  (with-slots (op hi lo cl) a
    (setf hi (max hi e)
          lo (min lo e)
          cl e)))

(defmethod compress-price ((a time-bar-generator) (e bar))
  (with-slots (op hi lo cl) a
    (setf hi (max hi (h e))
          lo (min lo (l e))
          cl e)))

(defun get-bar-timestamp (agent event)
  "Compute the bar timestamp for the current price event.
Values are the ending values of the time period."
  (with-slots (time-unit num-time-units) agent
    (case time-unit
      (:month
        (local-time:timestamp+
          (local-time:timestamp+
            (local-time:timestamp-minimize-part (timestamp event) :month)
            (* (floor (local-time:timestamp-month (timestamp event)) num-time-units)
                 num-time-units)
            :month)
          -1 :day))
      (:week
        (local-time:timestamp+
          (local-time:timestamp-minimize-part (timestamp event) :hour)
          (if (> (local-time:timestamp-day-of-week (timestamp event)) 5)
            (local-time:timestamp-day-of-week (timestamp event))
            (- 5 (local-time:timestamp-day-of-week (timestamp event))))
          :day))
      (:day
        (local-time:timestamp-minimize-part (timestamp event) :hour))
      ((:sec :hour :minute)
       (let ((time-part-fn (case time-unit
                             (:sec #'local-time:timestamp-second)
                             (:minute #'local-time:timestamp-minute)
                             (:hour #'local-time:timestamp-hour))))
         (local-time:timestamp+
           (local-time:timestamp-minimize-part (timestamp event) time-unit) 
           (* (1+ (floor (funcall time-part-fn (timestamp event)) num-time-units))
              num-time-units)
           time-unit))))))

(defmethod initialize ((a time-bar-generator))
  (with-slots (states security time-unit num-time-units name
               op hi lo cl current-bar-timestamp transitions) a
    (when (null states)
      (push :calc states)
      (setf name (format nil "TIME-BAR-GENERATOR_~A_~A_~A" security time-unit num-time-units))
      (setf transitions `((:calc . (,(make-instance
                                       'transition
                                       :initial-state :calc
                                       :final-state   :calc
                                       :sensor #'price
                                       :predicate #1=(lambda (p)
                                                       (local-time:timestamp= (get-bar-timestamp a p)
                                                                              current-bar-timestamp))
                                       :actuator #4=(lambda (p)
                                                      (compress-price a p)))
                                    ,(make-instance
                                      'transition
                                      :initial-state :calc
                                      :final-state   :emit
                                      :sensor #'price
                                      :predicate #2=(lambda (p)
                                                      (local-time:timestamp>
                                                        (get-bar-timestamp a p)
                                                        current-bar-timestamp))
                                      :actuator #5=(lambda (p)
                                                     (emit a (make-instance
                                                               'time-bar
                                                               :timestamp current-bar-timestamp
                                                               :security security
                                                               :value (list op hi lo cl)
                                                               :time-unit time-unit
                                                               :num-time-units num-time-units))
                                                     ;; emit filler bars for non-traded times
                                                     (loop with new-bar-timestamp = (get-bar-timestamp a p)
                                                           for filler-timestamp = (local-time:timestamp+
                                                                                    current-bar-timestamp
                                                                                    num-time-units time-unit)
                                                             then (local-time:timestamp+
                                                                     filler-timestamp
                                                                     num-time-units time-unit)
                                                           while (local-time:timestamp< filler-timestamp
                                                                                        new-bar-timestamp)
                                                           unless (market-closed-p a filler-timestamp)
                                                           do (emit a (make-instance
                                                                        'time-bar
                                                                        :timestamp filler-timestamp
                                                                        :security security
                                                                        :value (list cl cl cl cl)
                                                                        :time-unit time-unit
                                                                        :num-time-units num-time-units)))   
                                                     (initialize-price a p)))
                                    ,(make-instance       ;; Skip out-of-order event
                                       'transition
                                       :initial-state :calc
                                       :final-state   :skip
                                       :sensor #'price
                                       :predicate #3=(lambda (p)
                                                       (local-time:timestamp< (get-bar-timestamp a p)
                                                                              current-bar-timestamp))
                                       :actuator #6=(lambda (p)
                                                      (declare (ignore p))
                                                      nil))))
                          (:skip . (,(make-instance
                                       'transition
                                       :initial-state :skip
                                       :final-state   :calc
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #4#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :skip
                                       :final-state   :emit
                                       :sensor #'price
                                       :predicate #3#
                                       :actuator #6#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :skip
                                       :final-state   :skip
                                       :sensor #'price
                                       :predicate #3#
                                       :actuator #6#)))
                          (:emit . (,(make-instance
                                       'transition
                                       :initial-state :emit
                                       :final-state   :calc
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #4#)
                                    ,(make-instance
                                      'transition
                                      :initial-state :emit
                                      :final-state   :emit
                                      :sensor #'price
                                      :predicate #2#
                                      :actuator #5#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :emit
                                       :final-state   :skip
                                       :sensor #'price
                                       :predicate #3#
                                       :actuator #6#))))))))

(defmethod preprocess ((a time-bar-generator) (e market-update))
  (with-slots (initialized positions) a
    (push 0 positions)
    (when (not initialized)
      (initialize-price a e)
      (setf initialized t))))

;;EOF
