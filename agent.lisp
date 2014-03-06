;;;; agent.lisp

(in-package #:trading-core)
(named-readtables:in-readtable rutils-readtable)

(defclass agent ()
  ((name :accessor name :initarg :name :initform nil
     :documentation "Human-readable name of the agent.")
   (security :accessor security :initarg :security :initform nil
     :documentation "Keyword symbol of the security being traded by the agent.")
   (timestamps :accessor timestamps :initform nil
     :documentation "Reverse-chronological series of event timestamps seen by the agent.")    ; 
   (revalprices :accessor revalprices :initform nil
     :documentation "Reverse-chronological series of price events seen by the agent.")
   (orders :accessor orders :initform nil
     :documentation "Reverse-chronological series of orders issued by the agent.")
   (positions :accessor positions :initform nil
     :documentation "Reverse-chronological series of positions requested by the agent.")
   (pls :accessor pls :initform nil
     :documentation "Reverse-chronological series of profits/losses. Does not account for slippage.")
   (fitnesses :accessor fitnesses :initform nil
     :documentation "Reverse-chronological series of trading strategy fitness calculations.")
   (trade-groups-cache :accessor trade-groups-cache :initform nil
     :documentation "Cached grouped trades (market entry to exit) in reverse-chronological order.")
   (unprocessed-trades :accessor unprocessed-trades :initform nil
     :documentation "New, unprocessed/grouped trades in reverse-chronological order.")
   (latest-trade-stats :initform nil :type (or 'trade-stats nil)
     :documentation "Cached version of the last computed trade stats.")
   (incoming-messages :accessor incoming-messages :initform nil
     :documentation "Set of messages from other agents.  Used for inter-agent coordination.")
   (outgoing-messages :accessor outgoing-messages :initform nil
     :documentation "Set of messages to be passed to the other agents in the RECIPIENTS-LIST.")
   (recipients-list :accessor recipients-list :initarg :recipients-list :initform nil
     :documentation "Names of other agents that should receive outgoing messages.")))

;;; Agent methods

(defmethod print-object ((obj agent) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (when (slot-boundp obj 'name)
      (princ (name obj) stream)
      (princ " " stream))
    (princ (security obj) stream)))

;; Memoized "slot" functions

(defmethod trade-groups ((agent agent))
  "Retrieve/partition trades grouped by market position (entry/exit).
   
Single-trade position reversals (long-to-short/short-to-long) will be broken into
two dummy trades for easier accounting/statistics calcuations. A dummy trade will
also be inserted into the last group to close out an on-going trade."
  (with-slots (trade-groups-cache unprocessed-trades timestamps revalprices) agent
    ;; group any unprocessed trades and add them to the trade group cache.
    (unless (null unprocessed-trades)
      (let* ((dummy-exit-trade-p #`(string= "DummyExit" (trade-description %)))
             (last-group-trades (and (not (null trade-groups-cache))
                                     (trade-group-trades (first trade-groups-cache)))))
        (setf trade-groups-cache
              (if (and (not (null last-group-trades))
                       (some dummy-exit-trade-p last-group-trades))
                `(,@(partitions-trades
                      `(,@unprocessed-trades
                        ,@(remove-if dummy-exit-trade-p last-group-trades)))
                   ,@(rest trade-groups-cache))
                `(,@(partition-trades unprocessed-trades (first timestamps) (first revalprices))
                   ,@trade-groups-cache))
              unprocessed-trades nil)))
    trade-groups-cache))

(defmethod trade-stats ((a agent))
  "Retrieve/calculate the current the agent's current trade stats."
  (with-slots (unprocessed-trades latest-trade-stats) a
    (if (and latest-trade-stats (null unprocessed-trades))
      latest-trade-stats
      (setf latest-trade-stats (compute-trade-stats a)))))

(defmethod trades ((a agent))
  "Retrieve all trades an agent has made."
  (with-slots (unprocessed-trades trade-groups-cache) a
    `(,@unprocessed-trades
       ,@(unless (null trade-groups-cache)
           (rutils:flatten (loop for trade-group in trade-groups-cache
                          collecting (trade-group-trades trade-group)))))))

;; Trading methods

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
  (let ((timestamp (first (timestamps a))))
   (push (make-instance
          'comm
          :originator a
          :recipients (recipients-list a)
          :timestamp timestamp
          :value msg)
        *events-queue*)
  (push (list timestamp msg) (outgoing-messages a))))

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


(defmethod send-order ((a agent) (e market-update) &key opc oqt otp oid)
  "Create an order.

SEND-ORDER - Three places in the event consumption cycle by the agent where orders can
be emitted:
 1. In the FSM transitions ACTUATOR functions. Passive orders should be emitted
    when transitions happen.
 2. In the UPDATE :AFTER method. The immediate change in the agent's desired market position
    is being generated at the level of the FSM transition, and the resulting aggressive order
    can be dealt with after the FSM is processed and before post-processing.
 3. In the aggregator.  When several agents are trading in the same security it is also possible
    to route all the agents' positions into an aggregator so that the slippage is minimized. "
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
  (let* ((o (car (remove-if-not #`(equal (value %) old-oid) (orders a))))
         (rest-orders (remove-if #`(equal (value %) old-oid) (orders a))))
    (when new-opc (setf (order-price o) new-opc))
    (when new-oqt (setf (order-quantity o) new-oqt))
    (when new-otp (setf (order-type o) new-otp))
    (setf (timestamp o) (timestamp e)
          (orders a) `(,o ,@rest-orders))))

(defmethod cancel-order ((a agent) old-oid)
  (setf (orders a) (remove-if #`(equal (value %) old-oid)
                              (orders a))))

;; Order Management system simulation

(defmethod oms ((a agent) (e market-update) &key (algo-category :all))
  (let* ((category-p #`(or (equal algo-category :all)
                           (equal (algo-type (algo-instance %)) algo-category)))
         (bins (classify (orders a) (list category-p (complement category-p))))
         (category-orders (first bins))
         (non-category-orders (second bins))
         (new-category-orders nil))
    (dolist (o category-orders)
      (when (equal (security o) (security e))
        (multiple-value-bind (executions remaining-order)
            (execute o (algo-instance o) e)
          (when executions
            (setf (unprocessed-trades a) `(,@executions ,@(unprocessed-trades a))))
          (when remaining-order
            (push remaining-order new-category-orders)))))
    (setf (orders a) (append new-category-orders non-category-orders))))

;; EOF
