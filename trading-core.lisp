;;;; trading-core.lisp

(in-package #:trading-core)

;; Global constants

(defconstant +epsilon+ least-positive-normalized-single-float)

;; Global parameters

(defparameter *agents* '()
  "List of agents producing useful effects (processing events to produce other events)
or applying various trading strategies")

(defparameter *aggregate-agents* '()
  "List of agents that aggregate other agent (agents trading againsts a portfolio
of securities or agents trading multiple strategies against a single security).
These agents are in a separate list since the need to be updated after all of
their child agents have been updated.")

(defparameter *events-queue* '()
  "Chronological list of events (security prices or communications from other agents)
to be processed by the agents.")

;; API methods

(defgeneric update (agent event)
  (:documentation "Main method used to process events of various types."))

(defgeneric observe (agent event)
  (:documentation "Determine if a particular event is relevent to the specified agent."))

(defgeneric emit (agent msg &optional comm-type)
  (:documentation "Add an event to the event queue for other agents to (optionally) process."))

(defgeneric preprocess (agent event)
  (:documentation "Do prerequisite calculations needed by the update method."))

(defgeneric postprocess (agent event)
  (:documentation "Perform any required cleanup/procesing required after the update method has run."))

; load-event-data
; run-simulation

;; EOF
