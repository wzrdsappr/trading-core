;;;; agent.lisp

(in-package #:trading-core)
(named-readtables:in-readtable rutils-readtable)

(defclass agent ()
  ((name :accessor name :initarg :name :initform nil
     :documentation "Human-readable name of the agent.")
   (security :accessor security :initarg :security :type keyword
     :documentation "Keyword symbol of the security being traded by the agent.")
   (market-hours :accessor market-hours :initarg :market-hours
     :initform (list (local-time:parse-timestring "09:30:00")
                     (local-time:parse-timestring "15:45:00"))
     :documentation "Hours the market is open. Can be used in conjunction with the MARKET-CLOSED-P method
to close out open positions or prevent trading during high volatility/low liquidity times.")
   (short-size :accessor unblock-short :initform -1)
   (long-size :accessor unblock-long :initform 1)
   (timestamps :accessor timestamps :initform nil
     :documentation "Reverse-chronological series of event timestamps seen by the agent.")
   (revalprices :accessor revalprices :initform nil
     :documentation "Reverse-chronological series of price events seen by the agent.")
   (orders :accessor orders :initform nil
     :documentation "Reverse-chronological series of orders issued by the agent.")
   (positions :accessor positions :initform nil
     :documentation "Reverse-chronological series of positions requested by the agent.")
   (pls :accessor pls :initform nil
     :documentation "Reverse-chronological series of profits/losses. Does not account for slippage.")
   (navs :initform nil
     :documentation "Reverse-chronological series of agent's clock-time Net Asset Value (NAV). Does not account for slippage.")
   (indicators :accessor indicators :initform nil
     :documentation "Reverse-chronological series of indicator values.")
   (fitnesses :accessor fitnesses :initform nil
     :documentation "Reverse-chronological series of trading strategy fitness calculations.")
   (trade-groups-cache :accessor trade-groups-cache :initform nil :type (or list nil)
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
     :documentation "Names of other agents that should receive outgoing messages.")
   (fitness-feedback-control :accessor fitness-feedback-control :initarg :fitness-feedback-control :initform nil)))

;;; Agent methods

(defmethod initialize-instance :after ((a agent) &key)
  ;; Build the agent name
  (let ((format-string "~A")
        (name-parameter-values (list (extract-initials (type-of a)))))
    ;; Add security differentiator
    (when (and (slot-boundp a 'security)
               (slot-value a  'security))
      (setf format-string (concatenate 'string format-string "_~A"))
      (push (slot-value a 'security) name-parameter-values))
    ;; Add direct initialization values
    (loop for slot in (c2mop:class-direct-slots (class-of a))
          for slot-name = (c2mop:slot-definition-name slot)
          for arg-value = (when (slot-boundp a slot-name)
                            (slot-value a slot-name))
          when (and arg-value
                    (c2mop:slot-definition-initargs slot)
                    (not (eql (type-of arg-value) 'fitness-feedback-control)))
          do (progn
               (setf format-string (concatenate 'string format-string
                                                (cond
                                                  ((floatp arg-value) "_~4,2F")
                                                  (t "_~A"))))
               (push arg-value name-parameter-values)))
    ;; Add fitness-feedback-control differentiator
    (when (and (slot-boundp a 'fitness-feedback-control)
               (slot-value a  'fitness-feedback-control))
      (setf format-string (concatenate 'string format-string "_~A"))
      (push (slot-value (slot-value a 'fitness-feedback-control) 'name) name-parameter-values))
    (setf (slot-value a 'name) (format nil "~?" format-string (nreverse name-parameter-values)))))

(defmethod print-object ((a agent) stream)
  (with-slots (name security) a
    (print-unreadable-object (a stream :type t :identity t)
    (when (and (slot-boundp a 'name) name)
      (princ name stream))
    (when (slot-boundp a 'security)
      (princ " " stream)
      (princ security stream)))))

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
              (if (and (not (null last-group-trades))               ;; Reprocess last trade group if it
                       (some dummy-exit-trade-p last-group-trades)) ;; contains a dummy exit trade.
                `(,@(partition-trades
                      `(,@unprocessed-trades
                        ,@(remove-if dummy-exit-trade-p last-group-trades))
                       (first timestamps) (first revalprices))
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
                          collecting (reverse (trade-group-trades trade-group))))))))

;; Position retrieval functions

(defun adjusted-position (position)
  "Retrieve the final fitness-feedback-control adjusted position."
  (if (listp position)
    (car position)
    position))

(defun original-position (position)
  "Retrieve the original, unadjusted position."
  (if (listp position)
    (second position)
    position))

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

(defmethod emit ((a agent) msg &optional (comm-type 'comm))
  (let ((timestamp (first (timestamps a))))
   (push (make-instance
          comm-type
          :originator a
          :recipients (recipients-list a)
          :timestamp timestamp
          :value msg)
        *events-queue*)
  (push (list timestamp msg) (outgoing-messages a))))

(defun market-closed-p (agent timestamp)
  "Predicate to determine if the market is in after-hours trading for the given event.
The market is indicated as closed 15 minutes before the end of the trading session to give the
agent time to close any open positions."
  (with-slots (market-hours) agent
    (or (member (local-time:timestamp-day-of-week timestamp)
                '(0 6))    ; Sunday or Saturday
        (or (not (<= (local-time:sec-of (first market-hours))
                     (local-time:sec-of timestamp)
                     (local-time:sec-of (second market-hours))))))))

(defmethod preprocess ((a agent) (e market-direction-comm))
  "Set the allowed trading position based on market direction"
  (with-slots (short-size long-size) a
    (case (value e)
      (:range (setf short-size 0
                    long-size 0))
      (:long (setf short-size 0
                   long-size 1))
      (:short (setf short-size -1
                    long-size 0)))))

;; UPDATE :BEFORE methods

(defmethod update :before ((a agent) (e market-update))
  (with-slots (timestamps revalprices) a
    (push (timestamp e) timestamps)
    (push (price e) revalprices))
  (oms a e :algo-category :all)
  (preprocess a e)
  (log:debug ":BEFORE completed for agent ~A and event ~A~%" a e))

(defmethod update :before ((a agent) (e comm))
  (push e (incoming-messages a))
  (preprocess a e)
  (log:debug ":BEFORE completed for agent ~A and COMM event ~A~%" a e))

;; UPDATE MAIN methods

(defmethod update ((a agent) (e market-update))
  (log:debug "Enter new position for T= ~A and P= ~A" (timestamp e) (price e))
  (let ((new-position (read)))
    (push new-position (positions a))))

;; UPDATE :AFTER methods

(defmethod adjust-positions-for-fitness ((a agent))
  "Adjust an agent's position size per the fitness feedback."
  (with-slots (fitness-feedback-control positions) a
    (when fitness-feedback-control
      (let ((ffc-state (compute-fitness-feedback a fitness-feedback-control)))
        (when (and (eql ffc-state :offline) (/= (first positions) 0))  ;; force agent to flat when fitness is bad
          (push (list 0 (pop positions)) positions))))))

(defmethod update :after ((a agent) (e market-update))
  (adjust-positions-for-fitness a)
  (with-slots (positions revalprices pls navs) a
    (let* ((last-position (adjusted-position (first positions)))
           (prev-position-adjusted (or (adjusted-position (second positions)) 0))
           (prev-position-original (or (original-position (second positions)) 0))
           (trade-quantity (- last-position prev-position-adjusted))
           (last-price (first revalprices))
           (prev-price (or (second revalprices) 0))
           (pl (or (* prev-position-original (- last-price prev-price)) 0)))
      (push pl pls)
      (push (+ (or (first navs) 0) pl) navs)
      (unless (zerop trade-quantity)
        (send-order a e
                    :opc (price e)
                    :oqt trade-quantity
                    :otp (cond ((and (typep e 'time-bar) (eql (time-unit e) :day)) :moo)
                               (t :stp))
                    :oid :poschg)
        (log:debug "generated aggressive order for ~S and quantity ~S~%" a trade-quantity))
      (postprocess a e)
      (log:debug ":AFTER completed for agent ~A and event ~A~%" a e))))

(defmethod update :after ((a agent) (e comm))
  (postprocess a e)
  (log:debug ":AFTER completed for agent ~A and COMM event ~A~%" a e))

(defmethod postprocess ((a agent) (e event))
  (log:debug "Event ~S ~S Consumed for Agent ~S :~%"
             (timestamp e) (value e) (name a)))

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
                       :algo-instance (make-instance
                                        'simul
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
