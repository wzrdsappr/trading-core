;;;; fsm-agent.lisp

(in-package #:trading-core)

(defclass fsm-agent (fsm agent)
  ((states :accessor states :initform nil)
   (initialized :initform nil)))              ; determines if the agent is ready to begin trading

;;; FSM-Agent methods

(defmethod set-fsm ((a fsm-agent))
  (with-slots (states current-state) a
    (setf current-state (first states))))

(defmethod update ((a fsm-agent) (e comm))
  (set-fsm a)
  (logv:format-log "Set FSM completed for ~S~%" a)
  (logv:format-log "MAIN method completed for ~S and COMM event ~S~%" a e))

(defmethod operate-fsm ((a fsm-agent) (e event))
  (let* ((applicable-transitions
           (cdr (assoc (current-state a) (transitions a))))
         (effected-transition
           (car (remove-if-not (lambda (x) (perform x e))
                               applicable-transitions))))
    (funcall (actuator effected-transition)
             (funcall (sensor effected-transition) e))
    (setf (current-state a) (final-state effected-transition))
    (logv:format-log "~S Transition ~A -> ~A~%"
                     (name a)
                     (initial-state effected-transition)
                     (final-state effected-transition))))

 (defmethod update ((a fsm-agent) (e market-update))
  (set-fsm a)
  (logv:format-log "Set FSM completed for ~S~%" (name a))
  (operate-fsm a e)
  (logv:format-log "Operate FSM completed for ~S~%" (name a))
  (push (current-state a) (states a))
  (logv:format-log ":MAIN completed for ~S and new state ~S added ~%"
          (name a) (current-state a)))


;;EOF
