;;;; trading-core.lisp

(in-package #:trading-core)

;; Global constants

(defconstant *epsilon* 0.0000001)

;; Global parameters

(defparameter *events-queue* '())
(defparameter *agents* '())
(defparameter *aggregate-agents* '())

;; API methods

(defgeneric update (agent event)
  (:documentation "Main method used to process events of various types."))

(defgeneric observe (agent event)
  (:documentation "Determine if a particular event is relevent to the specified agent."))

(defgeneric emit (agent msg)
  (:documentation "Add an event to the event queue for other agents to (optionally) process."))

(defgeneric preprocess (agent event)
  (:documentation "Do prerequisite calculations needed by the update method."))

(defgeneric postprocess (agent event)
  (:documentation "Perform any required cleanup/procesing required after the update method has run."))

; load-event-data
; run-simulation

;; EOF
