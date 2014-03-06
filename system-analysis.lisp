;;; system-analysis.lisp
;;;
;;; Analyze the results of a trading session/simulation, generating HTML files
;;; charting the security data and any indicators for each trading agent. The
;;; trading stats are displayed along with each actual trade that was made.

(in-package #:trading-core)

;; Paths where the analysis UI templates are located and where the analysis
;; result files will be written.

(defparameter *ui-template-path*
  #P"C:/Users/Jonathan/Lisp/projects/trading-core/trading-ui/templates/"
  "Base directory where the cl-mustache library can find the UI templates.")

(defparameter *analysis-results-path*
  #P"C:/Users/Jonathan/Lisp/projects/trading-core/trading-ui/"
  "Base directory where the cl-mustache library can find the UI templates.")

;; Functions to compare multiple agents when performing a parameter search for
;; trading agents

(defun classify (objects-list predicates-list)
  "Iterate of a set of predicates and a set of objects and create a set of bins
containing the objects specified by the predicates."
  (let ((bins-list nil))
    (dolist (pred predicates-list)
      (let ((p-bin nil))
        (dolist (obj objects-list)
          (when (funcall pred obj)
            (push obj p-bin)))
        (push (nreverse p-bin) bins-list)))
    (values (nreverse bins-list))))

(defun cluster-agents (agents num-bins what)
  "Cluster a set of agents into a specified number of bins based on a trade statistic."
  (labels ((getstat (a)
             (let ((trade-stats (trade-stats a)))
               (case what
                 (:tpl (trade-stats-total-pl trade-stats))
                 (:lrt (trade-stats-average-logret trade-stats))
                 (:wtl (trade-stats-win-to-loss trade-stats))
                 (:pcp (trade-stats-percent-profitable trade-stats))
                 (:pff (trade-stats-profit-factor trade-stats))))))
    (loop for a in agents
          for stat = (getstat a)
          minimize stat into min-stats
          maximize stat into max-stats
          finally (multiple-value-bind (numeric-predicates bins)
                    (interval-division-predicates-bins
                      min-stats max-stats num-bins
                      :hard-lower-bound t :hard-upper-bound t)
                    (let* ((agent-predicates (mapcar (lambda (p)
                                                       (lambda (a)
                                                         (funcall p (getstat a))))
                                                     numeric-predicates))
                           (agent-clusters (classify agents agent-predicates)))
                      (return (values bins agent-clusters)))))))

;; Context data extraction methods

(defgeneric extract-context-data (obj)
  (:documentation "Return the relevant context data for an object."))

(defmethod extract-context-data ((p prc))
  "Return the relevant context data for a PRC market-event."
  `((:utc-date . ,(* 1000 (julian-to-unix-timestamp (timestamp p))))
    (:price . ,(value p)) (:high . ,(value p))
    (:low . ,(value p)) (:close . ,(value p))
    (:volume . 0)))

(defmethod extract-context-data ((p bar))
  "Return the relevant context data for a BAR market-event."
  `((:utc-date . ,(* 1000 (julian-to-unix-timestamp (timestamp p))))
    (:open . ,(o p)) (:high . ,(h p))
    (:low . ,(l p)) (:close . ,(c p))
    (:volume . ,(volume p))))

(defmethod extract-context-data ((trade trade))
  "Return the relevant context data for a TRADE object."
  (let* ((unix-timestamp (julian-to-unix-timestamp (trade-timestamp trade)))
         (local-timestamp (local-time:unix-to-timestamp unix-timestamp)))
    `((:utc-date . ,(* 1000 unix-timestamp))
      (:display-date . ,(local-time:format-timestring
                          nil local-timestamp
                          :format '(:year "-" (:month 2) "-" (:day 2))))
      (:quantity . ,(trade-quantity trade))
      (:price . ,(format nil "~,4F" (trade-price trade))))))

(defmethod extract-context-data ((stats trade-stats))
  "Returns the relevant context data for a TRADE-STATS data."
  `#(((:stat-name . "End Date")         (:stat-value . ,(local-time:format-timestring
                                                          nil (local-time:unix-to-timestamp
                                                                (julian-to-unix-timestamp
                                                                  (trade-stats-timestamp stats))) 
                                                          :format '(:year "-" (:month 2) "-" (:day 2)))))
     ((:stat-name . "Total P/L")
      (:stat-value . ,(format nil "~,4F" (trade-stats-total-pl stats))))
     ((:stat-name . "% Profitable")
      (:stat-value . ,(format nil "~,4F" (trade-stats-percent-profitable stats))))
     ((:stat-name . "Win/Loss Ratio")
      (:stat-value . ,(format nil "~,4F" (trade-stats-win-to-loss stats))))
     ((:stat-name . "Avg. Log Return")
      (:stat-value . ,(format nil "~,4F" (trade-stats-average-logret stats))))
     ((:stat-name . "Avg. Duration")
      (:stat-value . ,(format nil "~,4F" (trade-stats-average-duration stats))))
     ((:stat-name . "Profit")
      (:stat-value . ,(format nil "~,4F" (trade-stats-pos-pl stats))))
     ((:stat-name . "Loss")
      (:stat-value . ,(format nil "~,4F" (trade-stats-neg-pl stats))))
     ((:stat-name . "Profit Factor")
      (:stat-value . ,(format nil "~,4F" (trade-stats-profit-factor stats))))))

(defmethod extract-context-data ((agent agent))
  "Returns the agent's trade and profit/loss data as context data for analysis output."
  (let ((trades (reverse (trades agent))))
    `((:agent-name . ,(name agent))
      (:long-trades . ,(let ((first t))
                         (map 'vector
                              (lambda (trade)
                                `((:is-first . ,(prog1
                                                  first
                                                  (and first (setf first nil))))
                                  ,@(extract-context-data trade)))
                              (remove-if #'minusp trades :key #'trade-quantity))))
      (:short-trades . ,(let ((first t))
                          (map 'vector
                               (lambda (trade)
                                 `((:is-first . ,(prog1
                                                   first
                                                   (and first (setf first nil))))
                                   ,@(extract-context-data trade)))
                               (remove-if #'plusp trades :key #'trade-quantity))))
      (:all-trades . ,(let ((first t)
                            (row-number 0))
                        (map 'vector
                             (lambda (trade)
                               `((:is-first . ,(prog1
                                                 first
                                                 (and first (setf first nil))))
                                 (:even-row . ,(evenp (incf row-number)))
                                 ,@(extract-context-data trade)))
                             trades)))
      (:agent-profit-losses . ,(let ((first t))
                                 (map 'vector
                                      (lambda (ts rolling-pl)
                                        `((:is-first . ,(prog1
                                                          first
                                                          (and first (setf first nil))))
                                          (:utc-date . ,(* 1000 (julian-to-unix-timestamp ts)))
                                          (:equity . ,(format nil "~,4F" rolling-pl))))
                                      (reverse (timestamps agent))
                                      (rc-integrate (pls agent)))))
      (:agent-positions . ,(let ((first t))
                             (map 'vector
                                  (lambda (ts pos)
                                    `((:is-first . ,(prog1
                                                      first
                                                      (and first (setf first nil))))
                                      (:utc-date . ,(* 1000 (julian-to-unix-timestamp ts)))
                                      (:position . ,pos)))
                                  (reverse (timestamps agent))
                                  (reverse (positions agent)))))
      (:agent-trade-profit-losses . ,(let ((first t)
                                           (rolling-pl 0))
                                       (map 'vector
                                            (lambda (trade-group)
                                              (let ((ts (trade-group-exit-timestamp trade-group))
                                                    (trade-pl (trade-group-pl trade-group)))
                                                (incf rolling-pl trade-pl)
                                                `((:is-first . ,(prog1
                                                                first
                                                                (and first (setf first nil))))
                                                (:utc-date . ,(* 1000 (julian-to-unix-timestamp ts)))
                                                (:equity . ,(format nil "~,4F" rolling-pl)))))
                                            (reverse (trade-groups agent)))))
      (:trading-stats . ,(extract-context-data (trade-stats agent))))))

(defun analyze (agents security-data &key (template-name "analysis"))
  "Create web pages that display the results and statistics for a trading simulation."
  ;; Add the location of the mustache templates to the search path
  (pushnew *ui-template-path* mustache:*load-path* :test #'string-equal :key #'namestring)
  ;; Process each agent
  (dolist (agent agents)
    (let ((analysis-context
             `((:title . ,(format nil "~:A - ~A Analysis"
                                  (string-capitalize
                                    (substitute-if #\Space
                                                   (lambda (c)
                                                     (or (char= c #\-)
                                                         (char= c #\_)))
                                                   (name agent)))
                                  (symbol-name (security agent))))
               (:stock-symbol . ,(security agent))
               (:stock-data .
                ,(let ((first t))
                   (map 'vector
                        (lambda (p)
                          `((:is-first . ,(prog1
                                            first
                                            (and first (setf first nil))))
                            ,@(extract-context-data p)))
                        (cdr (assoc (security agent) security-data)))))
               ,@(extract-context-data agent))))
      (file-io:spit-file
        (mustache:mustache-render-to-string
          (format nil "{{{> ~A}}}" template-name) analysis-context)
        (make-pathname :name (format nil "~A-~A-analysis"
                                     (name agent) (symbol-name (security agent)))
                       :type "html"
                       :defaults *analysis-results-path*))))
  nil)

(defun compare-results (agents comparison-title security-data &key (template-name "comparison"))
  "Create a web page that compares the trading statistics for a set of trading agents."
  ;; Add the location of the mustache templates to the search path
  (pushnew *ui-template-path* mustache:*load-path* :test #'string-equal :key #'namestring)
  ;; Process each agent
  (let ((analysis-context
          `((:title . ,(format nil "~:A Trading Result Comparison"
                               (string-capitalize
                                 (substitute-if #\Space
                                                (lambda (c)
                                                  (or (char= c #\-)
                                                      (char= c #\_)))
                                                comparison-title))))
            (:buy-hold-data .
             ,(let ((first t)
                    starting-price)
                (map 'vector
                     (lambda (p)
                       (when (null starting-price)
                         (setf starting-price (price p)))
                       `((:is-first . ,(prog1
                                         first
                                         (and first (setf first nil))))
                         (:utc-date . ,(* 1000 (julian-to-unix-timestamp (timestamp p))))
                         (:equity . ,(format nil "~,4F" (- (price p) starting-price)))))
                     (cdr (assoc (security (first agents)) security-data)))))
            (:agents . ,(let ((first t)
                               (row-number 0))
                           (map 'vector
                                (lambda (agent)
                                  `((:is-first . ,(prog1
                                                    first
                                                    (and first (setf first nil))))
                                    (:even-row . ,(evenp (incf row-number)))
                                    ,@(extract-context-data agent)))
                                agents))))))
    (file-io:spit-file
      (mustache:mustache-render-to-string
        (format nil "{{{> ~A}}}" template-name) analysis-context)
      (make-pathname :name (format nil "~A-comparison"
                                   (substitute-if #\_ (lambda (c) (char= c #\Space))
                                                  comparison-title))
                     :type "html"
                     :defaults *analysis-results-path*))))

;; EOF
