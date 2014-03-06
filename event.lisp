;;;; event.lisp

(in-package #:trading-core)

(defclass event ()
  ((timestamp :accessor timestamp :initarg :timestamp)
   (value :accessor value :initarg :value)))

(defclass comm (event)
  ((originator :accessor originator :initarg :originator)
   (recipients :accessor recipients :initarg :recipients))
  (:documentation "Message for internal inter-agent communication."))

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
   (volume :accessor volume :initarg :volume :initform nil))
  (:documentation "Compressed price information."))

(defclass tick-bar (bar)
  ((num-ticks :accessor num-ticks :initarg :num-ticks))
  (:documentation "Compressed price for a fixed number of trades."))

(defclass time-bar (bar)
  ((num-time-units :accessor num-time-units :initarg :num-time-units)
   (time-unit :accessor time-unit :initarg :time-unit :initform :minute))
  (:documentation "Compressed price for a fixed unit of time."))

(defclass box (market-update)
  ((column :accessor column)
   (h :accessor h)
   (l :accessor l)
   (box-type :accessor box-type))
  (:documentation "Cross or circle box for box chart."))

;;; Outgoing message event types

(defclass order (market-update)
  ((order-type :accessor order-type :initarg :order-type)
   (order-quantity :accessor order-quantity :initarg :order-quantity)
   (order-price :accessor order-price :initarg :order-price)
   (algo-instance :accessor algo-instance :initarg :algo-instance :type algo)))

;; Methods

(defmethod initialize-instance :after ((e prc) &key)
  (setf (last-price e) (first (value e)))
  (when (second (value e))
    (setf (last-volume e) (second (value e)))))

(defmethod initialize-instance :after ((e book) &key)
  (let* ((v (value e))
         (lb (first v))
         (la (second v)))
    (setf lb (sort lb (lambda (x y) (> (first x) (first y))))
          la (sort la (lambda (x y) (< (first x) (first y)))))
    (setf (bid-best e) (first (first lb))
          (bid-best-size e) (second (first lb))
          (bid-total-size e) (reduce #'+ lb :key #'second :initial-value 0))
    (setf (bid-avg-price e) (/ (reduce #'+ lb :key (lambda (x)
                                                     (* (first x) (second x))))
                               (bid-total-size e)))
    (setf (ask-best e) (first (first la))
          (ask-best-size e) (second (first la))
          (ask-total-size e) (reduce #'+ la :key #'second :initial-value 0))
    (setf (ask-avg-price e) (/ (reduce #'+ la :key (lambda (x)
                                                     (* (first x) (second x))))
                               (ask-total-size e)))
    (setf (mid e) (* 0.5 (+ (bid-best e) (ask-best e))))
    (setf (value e) (list lb la))))

(defmethod initialize-instance :after ((e bar) &key)
  (let ((v (value e)))
    (setf (pivot e) (avg-list (subseq v 0 4))
          (o e) (first v)
          (h e) (second v)
          (l e) (third v)
          (c e) (fourth v)
          (body-fill e) (< (c e) (o e))
          (volume e) (or (fifth v) 0))))

(defmethod initialize-instance :after ((e box) &key)
  (let ((v (value e)))
    (setf (column e) (first v)
          (h e) (second v)
          (l e) (third v)
          (box-type e) (fourth v))))

(defmethod print-object ((obj bar) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (timestamp obj) stream)
    (princ (pivot obj) stream)))

(defmethod slippage-function ((e prc) size order-type)
  "Example slippage function - .1 percent of order price"
  (* (signum size) 0.001)) 

(defmethod slippage-function ((e bar) size order-type)
  "Example slippage function - 1 percent of disadvantageous range"
  (* (case (signum size)
       (1 (- (h e) (o e)))
       (-1 (- (o e) (l e)))
       (otherwise 0))
     0.01))

(defun adjust-price (p slippage-fn e size order-type)
  "Adjust the order price by a slippage amount when simulating order fulfillment."
  (* p (1+ (if slippage-fn
             (funcall slippage-fn e size order-type)
             0))))

(defmethod price ((e prc) &key (slippage-fn nil) (size 0) (order-type nil))
  (let ((p (car (value e))))
    (adjust-price p slippage-fn e size order-type)))

(defmethod price ((e bar) &key (slippage-fn nil) (size 0) (order-type nil))
  (let ((p (case order-type
             ((:STP_ON_OPEN :LMT_ON_OPEN) (o e))
             ((:STP_ON_CLOSE :LMT_ON_CLOSE) (c e))
             (otherwise (pivot e)))))
    (adjust-price p slippage-fn e size order-type)))

(defmethod price ((e book) &key (slippage-fn nil) (size 0) (order-type nil))
  (let ((p (cond ((zerop size) (mid e))
                 ((> size 0) (ask-best e))
                 (t (bid-best e)))))
    (adjust-price p slippage-fn e size order-type)))

;; Order execution simulation method

(defmethod execute ((o order) (l simul) (e market-update))
  "Simulate execution of an order."
  (values
    (list (make-trade :timestamp (timestamp e)
                      :price (price e
                                    :slippage-fn (slippage l)
                                    :size (order-quantity o)
                                    :order-type (order-type o))
                      :quantity (order-quantity o)))
    nil))

(defun lift-quotes (quotes-list quantity max-depth)
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
         (sweep (lift-quotes bookside q (max-depth l)))
         (trades (mapar (lambda (x) (make-trade
                                      :timestamp (timestamp e)
                                      :price (first x)
                                      :quantity (* (signum q) (second x))))
                        sweep))
         (sweep-size (reduce #'+ sweep :key #'second :initial-value 0))
         (shortfall (- q sweep-size)))
    (values trades
            (if (zerop shortfall)
              nil
              (make-instance 'order
                             :timestamp (timestamp e)
                             :value (value o)
                             :security (security e)
                             :order-price (order-price o)
                             :order-quantity shortfall
                             :order-type (order-type o)
                             :algo-instance l)))))

;;EOF
