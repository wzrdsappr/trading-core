;;;; trading-core.asd

(asdf:defsystem #:trading-core
  :serial t
  :description "Trading platform that allows trading strategies to be implemented via finite state machines."
  :author "Jonathan Lee"
  :license "BSD 2-Clause"
  :depends-on (#:file-io #:cl-ppcre #:logv #:cl-mustache)
  :components ((:file "package")
               (:file "utility-functions")
               (:file "trading-core")
               (:file "algo")
               (:file "event")
               (:file "fsm")
               (:file "trade")
               (:file "agent")
               (:file "aggregate-agent")
               (:file "fsm-agent")
               (:file "processing-agents/tick-bar-generator")
               (:file "processing-agents/box-chart-agent")
               (:file "trading-agents/simple-model")
               (:file "trading-agents/simple-model-comm")
               (:file "trading-agents/channel-breakout-trend-following")
               (:file "trading-agents/envelope-moving-avg-trend-following")
               (:file "trading-agents/adaptive-moving-avg-trend-following")
               (:file "trading-agents/swing-breakout")
               (:file "trading-agents/opening-range-breakout")
               (:file "trading-agents/swing-mean-reversion")
               (:file "trading-agents/range-projection-mean-reversion")
               (:file "trade-stats")
               (:file "simulate")))

