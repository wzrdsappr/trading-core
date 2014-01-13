;;;; agent.lisp

(in-package #:trading-core)

(defclass agent ()
  ((name :accessor name :initarg :name)
   (security :accessor security :initarg :security :initform nil)
   (timestamps :accessor timestamps :initform nil)
   (revalprices :accessor revalprices :initform nil)
   (orders :accessor orders :initform nil)
   (positions :accessor positions :initform nil)
   (pls :accessor pls :initform nil)
   (fitnesses :accessor fitnesses :initform nil)
   (trades :accessor trades :initform nil)
   (tradestats :accessor tradestats :initform nil)
   (incoming-messages :accessor incoming-messages :initform nil)
   (outgoing-messages :accessor outgoing-messages :initform nil)
   (recipients-list :accessor recipients-list :initarg :recipients-list :initform nil)))

;;; Agent methods

(defmethod observe ((a agent) (e event))
  t)

(defmethod observe ((a agent) (e market-update))
  (or (not (security a))
      (eql (security a) (security e))))

(defmethod observe ((a agent) (e comm))
  (and (not (equal a (originator e)))
       (member a (recipients e))))

(defun consume (agent event)
  (when (observe agent event)
    (update agent event)))

(defmethod emit ((a agent) msg)
  (push (make-instance
          'comm
          :originator a
          :recipients (recipients-list a)
          :timestamp (first (timestamps a))
          :value msg)
        *events-queue*)
  (push (list (first (timestamps a)) msg) (outgoing-messages a)))

;; UPDATE :BEFORE methods

(defmethod update :before ((a agent) (e market-update))
  (when (null (timestamps a))
    (push 0 (pls a))
    (push 0 (fitnesses a)))
  (push (timestamp e) (timestamps a))
  (push (price e) (revalprices a))
  (oms a e :algo-category :all)
  (preprocess a e)
  (logv:format-log ":BEFORE completed for agent ~A and event ~A~%" a e))

(defmethod update :before ((a agent) (e comm))
  (push e (incoming-messages a))
  (preprocess a e)
  (logv:format-log ":BEFORE completed for agent ~A and COMM event ~A~%" a e))

;; UPDATE MAIN methods

(defmethod update ((a agent) (e market-update))
  (logv:format-log "Enter new position for T= ~A and P= ~A" (timestamp e) (price e))
  (let ((new-position "read"))
    (push new-position (positions a))))

;; UPDATE :AFTER methods

(defmethod update :after ((a agent) (e market-update))
  (let* ((last-position (first (positions a)))
         (prev-position (or (second (positions a)) 0))
         (trade-quantity (- last-position prev-position))
         (last-price (first (revalprices a)))
         (prev-price (or (second (revalprices a)) 0))
         (pl (or (* prev-position (- last-price prev-price)) 0)))
    (push pl (pls a))
    (unless (zerop trade-quantity)
      (send-order a e
                  :opc (price e)
                  :oqt trade-quantity
                  :otp :STP
                  :oid :POSCHG)
      (logv:format-log "generated aggressive order for ~S and quantity ~S~%" a trade-quantity))
    (postprocess a e)
    (logv:format-log ":AFTER completed for agent ~A and event ~A~%" a e)))

(defmethod update :after ((a agent) (e comm))
  (postprocess a e)
  (logv:format-log ":AFTER completed for agent ~A and COMM event ~A~%" a e))

;; SEND-ORDER - Three places in the event consumption cycle by the agent where orders can
;; be emitted:
;;  1. In the FSM transitions ACTUATOR functions. Passive orders should be emitted
;;     when transitions happen.
;;  2. In the UPDATE :AFTER method. The immediate change in the agent's desired market position
;;     is being generated at the level of the FSM transition, and the resulting aggressive order
;;     can be dealt with after the FSM is processed and before post-processing.
;;  3. In the aggregator.  When several agents are trading in the same security it is also possible
;;     to route all the agents' positions into an aggregator so that the slippage is minimized.

(defmethod send-order ((a agent) (e market-update) &key opc oqt otp oid)
  "Create an order."
  (push (make-instance 'order
                       :timestamp (timestamp e)
                       :value oid
                       :security (security e)
                       :order-type otp
                       :order-quantity oqt
                       :order-price opc
                       :algo-instance (make-instance 'simul
                                                    :algo-type (case otp
                                                                 ((:stp :ioc :moc :moo) :aggressive)
                                                                 ((:lmt) :passive))))
        (orders a)))

(defmethod change-order ((a agent) (e market-update) &key new-opc new-oqt new-otp old-oid)
  "Change an unfilled, passive order."
  (let ((o (car (remove-if-not (lambda (x) (equal (value x) old-oid))
                               (orders a))))
        (rest-orders (remove-if (lambda (x) (equal (value x) old-oid))
                                (orders a))))
    (when new-opc (setf (order-price o) new-opc))
    (when new-oqt (setf (order-quantity o) new-oqt))
    (when new-otp (setf (order-type o) new-otp))
    (setf (timestamp o) (timestamp e)
          (orders a) `(,o ,@rest-orders))))

(defmethod cancel-order ((a agent) old-oid)
  (setf (orders a) (remove-if (lambda (x) (equal (value x) old-oid))
                              (orders a))))

;; Order Management system simulation

(defmethod oms ((a agent) (e market-update) &key (algo-category :all))
  (let* ((category-p (lambda (x) (or (equal algo-category :all)
                                    (equal (algo-type (algo-instance x)) algo-cate))))
         (bins (classify (orders a) (list category-p (complement category-p))))
         (category-orders (first bins))
         (non-category-orders (second bins))
         (new-category-orders nil))
    (dolist (o category-orders)
      (when (equal (security o) (security e))
        (multiple-value-bind (executions remaining-order)
            (execute o (algo-instance o) e)
          (when executions
            (setf (trades a) `(,@executions ,@(trades a)))
            (push (compute-tradestats (trades a)) (tradestats a)))
          (when remaining-order
            (push remaining-order new-category-orders)))))
    (setf (orders a) (append new-category-orders non-category-orders))))

;; EOF
