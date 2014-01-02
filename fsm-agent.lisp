;;;; event.lisp

(in-package #:trading-core)

(defclass fsm-agent (fsm agent)
  ((states :accessor states :initform nil)))

;;; FSM-Agent methods

(defmethod update ((a fsm-agent) (e comm))
  (set-fsm a)
  (format t "Set FSM completed for ~S~%" a)
  (format t "MAIN method completed for ~S and COMM event ~S~%" a e))

(defmethod update ((a fsm-agent) (e market-update))
  (set-fsm a)
  (format t "Set FSM completed for ~S~%" (name a))
  (operate-fsm a e)
  (format t "Operate FSM completed for ~S~%" (name a))
  (push (current-state a) (states a))
  (format t ":MAIN completed for ~S and new state ~S added ~%"
          (name a) (current-state a)))


;;EOF
