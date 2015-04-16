;;;; event.lisp

(in-package #:trading-core)
(named-readtables:in-readtable rutils-readtable)

(defclass event ()
  ((timestamp :accessor timestamp :initarg :timestamp)
   (value :accessor value :initarg :value)))

(defclass comm (event)
  ((originator :accessor originator :initarg :originator)
   (recipients :accessor recipients :initarg :recipients))
  (:documentation "Message for internal inter-agent communication."))

(defclass market-direction-comm (comm)
  ()
  (:documentation "Message for filtering agent trades based on market direction."))

(defclass market-update (event)
  ((security :accessor security :initarg :security)))

;;; Incoming message event types

(defclass prc (market-update)
  ((last-price :accessor last-price)
   (last-volume :accessor last-volume))
  (:documentation "Last traded price and volume for a security as of the given time."))

(defclass book (market-update)
  ((mid :accessor mid)
   (bid-best :accessor bid-best)
   (bid-best-size :accessor bid-best-size)
   (bid-total-size :accessor bid-total-size)
   (bid-avg-price :accessor bid-avg-price)
   (ask-best :accessor ask-best)
   (ask-best-size :accessor ask-best-size)
   (ask-total-size :accessor ask-total-size)
   (ask-avg-price :accessor ask-avg-price))
  (:documentation "Represents the set of posted resting orders for a particular
security at an exchange at a point in time."))

;; TODO : Fill in the definition of this later, if needed for communication with an ECN.
(defclass delta (market-update)
  ()
  (:documentation "Change to the order book for a security as of the given time."))

(defclass bar (market-update)
  ((pivot :accessor pivot)
   (o :accessor o)
   (h :accessor h)
   (l :accessor l)
   (c :accessor c)
   (body-fill :accessor body-fill)
   (volume :accessor volume :initform nil))
  (:documentation "Compressed price information."))

(defclass tick-bar (bar)
  ((num-ticks :accessor num-ticks :initarg :num-ticks :type (integer 0)))
  (:documentation "Compressed price for a fixed number of trades."))

(defclass time-bar (bar)
  ((num-time-units :accessor num-time-units :initarg :num-time-units
    :type (integer 0) :initform 1)
   (time-unit :accessor time-unit :initarg :time-unit :initform :day
     :type (member :year :month :week :day :hour :minute)))
  (:documentation "Compressed price for a fixed unit of time."))

(defclass box (market-update)
  ((column :accessor column)
   (h :accessor h)
   (l :accessor l)
   (box-type :accessor box-type))
  (:documentation "Cross or circle box for box chart."))

;;; Outgoing message event types

(defclass order (market-update)
  ((order-type :accessor order-type :initarg :order-type
     :type (member :mkt       ; market
                   :moo       ; market on open
                   :moc       ; market on close
                   :loo       ; limit on open
                   :loc       ; limit on close
                   :stp       ; stop
                   :lmt       ; limit
                   :stp-lmt)) ; stop-limit
   (order-quantity :accessor order-quantity :initarg :order-quantity)
   (order-price :accessor order-price :initarg :order-price)
   (algo-instance :accessor algo-instance :initarg :algo-instance :type algo)))

;; Methods

(defmethod assert-initargs ((e market-update))
  (assert (slot-boundp e 'timestamp))
  (assert (slot-boundp e 'value))
  (assert (slot-boundp e 'security)))

(defmethod initialize-instance :after ((e prc) &key)
  (assert-initargs e)
  (setf (last-price e) (first (value e)))
  (when (second (value e))
    (setf (last-volume e) (second (value e)))))

(defmethod initialize-instance :after ((e book) &key)
  (assert-initargs e)
  (let* ((v (value e))
         (lb (first v))
         (la (second v)))
    (setf lb (sort lb #`(> (first %) (first %%)))
          la (sort la #`(< (first %) (first %%))))
    (setf (bid-best e) (first (first lb))
          (bid-best-size e) (second (first lb))
          (bid-total-size e) (reduce #'+ lb :key #'second :initial-value 0))
    (setf (bid-avg-price e) (/ (reduce #'+ lb :key #`(* (first %) (second %)))
                               (bid-total-size e)))
    (setf (ask-best e) (first (first la))
          (ask-best-size e) (second (first la))
          (ask-total-size e) (reduce #'+ la :key #'second :initial-value 0))
    (setf (ask-avg-price e) (/ (reduce #'+ la :key #`(* (first %) (second %)))
                               (ask-total-size e)))
    (setf (mid e) (* 0.5 (+ (bid-best e) (ask-best e))))
    (setf (value e) (list lb la))))

(defmethod initialize-instance :after ((e bar) &key)
  (assert-initargs e)
  (let ((v (value e)))
    (setf (pivot e) (avg-list (subseq v 0 4))
          (o e) (first v)
          (h e) (second v)
          (l e) (third v)
          (c e) (fourth v)
          (body-fill e) (< (c e) (o e))
          (volume e) (or (fifth v) 0))))

(defmethod initialize-instance :after ((e box) &key)
  (assert-initargs e)
  (let ((v (value e)))
    (setf (column e) (first v)
          (h e) (second v)
          (l e) (third v)
          (box-type e) (fourth v))))

(defmethod print-object ((obj bar) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (when (slot-boundp obj 'security)
      (princ (security obj) stream))
    (when (slot-boundp obj 'timestamp)
      (princ " " stream)
      (princ (timestamp obj) stream))
    (when (slot-boundp obj 'pivot)
      (princ " " stream)
      (princ (pivot obj) stream))))

(defgeneric price (event)
  (:documentation "Average price for a market-event."))

(defmethod price ((e prc))
  (first (value e)))

(defmethod price ((e bar))
  (pivot e))

(defmethod price ((e box))
  (/ (- (h e) (l e)) 2))

;; Methods used to apply a slippage value to a simulated trade price

(defmethod slippage-function ((e prc) size order-type)
  "Example slippage function - .1 percent of order price"
  (declare (ignore order-type))
  (* (signum size) 0.001))

(defmethod slippage-function ((e bar) size order-type)
  "Example slippage function - 1 percent of disadvantageous range"
  (declare (ignore order-type))
  (/ (* (case (signum size)
          (1 (- (h e) (o e)))
          (-1 (- (l e) (o e)))
          (otherwise 0))
        0.01)
     (o e)))

(defun adjust-price (p slippage-fn e order)
  "Adjust the order price by a slippage amount when simulating order fulfillment."
  (* p (1+ (if slippage-fn
             (funcall slippage-fn e (order-quantity order) (order-type order))
             0))))

(defgeneric simulated-trade-price (event slippage-fn order)
  (:documentation "Calculate the price of a simulated trade."))

(defmethod simulated-trade-price ((e prc) slippage-fn order)
  (let ((p (car (value e))))
    (adjust-price p slippage-fn e order)))

(defmethod simulated-trade-price ((e bar) slippage-fn order)
  (let ((p (case (order-type order)
             ((:moo :loo) (o e))
             ((:moc :loc) (c e))
             (otherwise (pivot e)))))
    (adjust-price p slippage-fn e order)))

(defmethod simulated-trade-price ((e book) slippage-fn order)
  (let ((p (cond ((zerop (order-quantity order)) (mid e))
                 ((> (order-quantity order) 0) (ask-best e))
                 (t (bid-best e)))))
    (adjust-price p slippage-fn e order)))

;; Order execution simulation method

(defgeneric execute (order algo event)
  (:documentation "Simulate execution of an order."))

(defmethod execute ((o order) (l simul) (e market-update))
  "Simulate execution of an order."
  (values
    (list (make-trade
            :timestamp (timestamp e)
            :price (simulated-trade-price e (slippage l) o)
            :quantity (order-quantity o)))
    nil))

(defmethod execute ((o order) (l simul) (e bar))
  "Simulate execution of an order."
  (let ((trades '())
        (within-range nil))
    (when (or (member (order-type o) '(nil :mkt :moo :moc))
              (and (eql (order-type o) :stp) (< (order-price o) (h e)))
              (and (eql (order-type o) :lmt) (> (order-price o) (l e))))
      (push (make-trade
              :timestamp (timestamp e)
              :price (simulated-trade-price e (slippage l) o)
              :quantity (order-quantity o))
            trades)
      (setf within-range t))
    (values
      trades
      (if within-range
        nil
        (make-instance
          'order
          :timestamp (timestamp e)
          :value (value o)
          :security (security e)
          :order-price (order-price o)
          :order-quantity (order-quantity o)
          :order-type (order-type o)
          :algo-instance l)))))

(defun lift-quotes (quotes-list quantity max-depth)
  "Calculate the list of price quotes and quantities need to fulfill as much of an
order as feasible for the specified maximum book depth."
  (loop with result = nil
        for i below max-depth
        for quote in quotes-list
        for sweep-quantity = (second quote) then (+ sweep-quantity (second quote))
        do (push quote result)
        while (< sweep-quantity quantity)
        finally (progn
                  (when (> (- sweep-quantity quantity) 0)
                    (decf (second (first result)) (- sweep-quantity quantity)))
                  (return result))))

(defmethod execute ((o order) (l agressor) (e book))
  (let* ((q (order-quantity o))
         (book-side (if (> q 0) (second (value e)) (first (value e))))
         (sweep (lift-quotes book-side q (max-depth l)))
         (trades (mapcar #`(make-trade
                             :timestamp (timestamp e)
                             :price (first %)
                             :quantity (* (signum q) (second %)))
                         sweep))
         (sweep-size (reduce #'+ sweep :key #'second :initial-value 0))
         (shortfall (- q sweep-size)))
    (values trades
            (if (zerop shortfall)
              nil
              (make-instance
                'order
                :timestamp (timestamp e)
                :value (value o)
                :security (security e)
                :order-price (order-price o)
                :order-quantity shortfall
                :order-type (order-type o)
                :algo-instance l)))))

;;EOF
