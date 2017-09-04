;;;; fsm.lisp

(in-package #:trading-core)

(defclass fsm ()
  ((current-state :accessor current-state :initform nil)
   (transitions   :accessor transitions   :initform nil)))

(defclass transition ()
  ((initial-state :accessor initial-state :initarg :initial-state)
   (final-state   :accessor final-state   :initarg :final-state)
   (sensor        :accessor sensor        :initarg :sensor)
   (predicate     :accessor predicate     :initarg :predicate)
   (actuator      :accessor actuator      :initarg :actuator)
   (effected      :accessor effected      :initform nil)))

;;; FSM methods

(defmethod perform ((tr transition) e)
  (setf (effected tr) (funcall (predicate tr)
                               (funcall (sensor tr) e))))

(defmethod perform ((tr transition) (e event))
  (setf (effected tr) (funcall (predicate tr)
                               (funcall (sensor tr) e))))

(defmethod operate-fsm ((fsm fsm) (e event))
  (let* ((applicable-transitions
           (cdr (assoc (current-state fsm) (transitions fsm))))
         (effected-transition
           (car (remove-if-not (lambda (x) (perform x e))
                               applicable-transitions))))
    (funcall (actuator effected-transition)
             (funcall (sensor effected-transition) e))
    (setf (current-state fsm) (final-state effected-transition))
    (log:debug "~A Transition ~A -> ~A~%"
                     fsm
                     (initial-state effected-transition)
                     (final-state effected-transition))))

;; EOF
