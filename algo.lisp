;;;; algo.lisp

(in-package #:trading-core)

(defclass algo ()
  ((algo-type :accessor algo-type :initarg :algo-type :type (or :aggressive :passive))))

(defclass simul (algo)
  ((slippage :accessor slippage :initarg :slippage :initform #'slippage-function)))

(defclass agressor (algo)
  ((max-depth :accessor max-depth :initarg :max-depth)))

;;; Methods

;; Example slippage function
(defun slippage-function (e size order-type)
  (* (signum size) 0.01))

;; Example slippage function where slippage is specified for specific securities
#|
(defun slippage-function (e size order-type)
  (* signum size
     (case-equal (security e)
       ("AAPL" (case order-type
                 ((:LMT :LMT-ON-OPEN :LMT-ON-CLOSE)
                  (* 0.00002 s))
                 (otherwise (* 0.0002 s))))
       ("MSFT" (case order-type
                 ((:LMT :LMT-ON-OPEN :LMT-ON-CLOSE)
                  (* 0.00004 s))
                 (otherwise (* 0.0004 s))))
       (t 0))))
|#

;; EOF
