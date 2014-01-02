;;;; package.lisp

(defpackage #:trading-core
  (:use #:cl)
  (:export #:*data-dir-string*
           #:*agents*
           #:*aggregate-agents*
           #:*events*
           #:load-event-data
           #:run-simulation
           #:analyze
           ))

