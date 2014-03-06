;;;; algo.lisp

(in-package #:trading-core)

(defclass algo ()
  ((algo-type :accessor algo-type :initarg :algo-type :type (or :aggressive :passive))))

(defclass simul (algo)
  ((slippage :accessor slippage :initarg :slippage :initform #'slippage-function)))

(defclass agressor (algo)
  ((max-depth :accessor max-depth :initarg :max-depth)))

;;; Methods

(defgeneric slippage-function (e size order-type)
  (:documentation "Add a slippage amount to each trade of a simulation to make
the trading results more realistic."))

(defmethod slippage-function (e size order-type)
  "Example slippage function - .1 percent of order price"
  (* (signum size) 0.001)) 

;; Example slippage function where slippage is specified for specific securities
#|
(defmethod slippage-function (e size order-type)
  (* signum size
     (case-equal (security e)
       ("AAPL" (case order-type
                 ((:LMT :LMT-ON-OPEN :LMT-ON-CLOSE)
                  (* 0.00002 size))
                 (otherwise (* 0.0002 size))))
       ("MSFT" (case order-type
                 ((:LMT :LMT-ON-OPEN :LMT-ON-CLOSE)
                  (* 0.00004 size))
                 (otherwise (* 0.0004 size))))
       (t 0))))
|#

;; EOF
