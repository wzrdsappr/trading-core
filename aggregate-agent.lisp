;;;; aggregate-agent.lisp

(in-package #:trading-core)

(defclass aggregate-agent (agent)
  ((members :accessor members :initarg :members :initform nil)))

;;; Aggregate Agent methods

(defmethod update ((a aggregate-agent) (e market-update))
  (push (reduce #'+ (car (members a))
                :key (lambda (m) (car (positions m))))
        (positions a))
  (setf (orders a) (reduce 'append (car (members a)) :key #'orders))
  (log:debug "MAIN complete for agent ~S and event ~S~%" a e))

;; EOF
