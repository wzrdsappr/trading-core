;;; system-analysis.lisp
;;;
;;; Analyze the results of a trading session/simulation, generating HTML files
;;; charting the security data and any indicators for each trading agent. The
;;; trading stats are displayed along with each actual trade that was made.

(in-package #:trading-core)

;; Paths where the analysis UI templates are located and where the analysis
;; result files will be written.

(defparameter *ui-template-path*
  (merge-pathnames "trading-core/trading-ui/templates/"
                   (first ql:*local-project-directories*))
  "Base directory where the cl-mustache library can find the UI templates.")

(defparameter *analysis-results-path*
  (merge-pathnames "trading-core/trading-ui/"
                   (first ql:*local-project-directories*))
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

;;


(defmacro extract-indicators (agent (&rest indicator-names))
  "Helper macro to simplify the extraction of indicator data from an agent."
  (let ((indicator-vars (loop for n in indicator-names
                              collecting `(,n
                                           ,(gensym (format nil "~a-" n))
                                           ,(gensym (format nil "~a-data-" n))))))
    (alexandria:once-only (agent)
      (alexandria:with-gensyms (is-first ts utc-date)
        (let ((collect-clauses
                (loop for (nil var array-var) in indicator-vars
                      appending `(when ,var collect (list (cons :is-first ,is-first)
                                                          (cons :utc-date ,utc-date)
                                                          (cons :value (format nil "~,8f" ,var)))
                                   into ,array-var)))
              (return-values (loop for (name nil array-var) in indicator-vars
                                   collecting `(list (cons :indicator-name ,name)
                                                     (cons :indicator-data (coerce ,array-var 'vector))))))
          `(loop for ,is-first = t then nil
                 for ,ts in (reverse (timestamps ,agent))
                 and ,(loop for (nil var nil) in indicator-vars
                            collecting var) in (reverse (indicators ,agent))
                 for ,utc-date = (* 1000 (local-time:timestamp-to-unix ,ts))
                 ,@collect-clauses
                 finally (return `#(,,@return-values))))))))

;; Context data extraction methods

(defgeneric extract-context-data (obj)
  (:documentation "Return the relevant context data for an object."))

(defmethod extract-context-data ((p prc))
  "Return the relevant context data for a PRC market-event."
  `((:utc-date . ,(* 1000 (local-time:timestamp-to-unix (timestamp p))))
    (:price . ,(value p)) (:high . ,(value p))
    (:low . ,(value p)) (:close . ,(value p))
    (:volume . 0)))

(defmethod extract-context-data ((p bar))
  "Return the relevant context data for a BAR market-event."
  `((:utc-date . ,(* 1000 (local-time:timestamp-to-unix (timestamp p))))
    (:open . ,(o p)) (:high . ,(h p))
    (:low . ,(l p)) (:close . ,(c p))
    (:volume . ,(volume p))))

(defmethod extract-context-data ((trade trade))
  "Return the relevant context data for a TRADE object."
  (let* ((unix-timestamp (local-time:timestamp-to-unix (trade-timestamp trade))))
    `((:utc-date . ,(* 1000 unix-timestamp))
      (:display-date . ,(local-time:format-timestring
                          nil (trade-timestamp trade)
                          :format '(:year "-" (:month 2) "-" (:day 2))))
      (:quantity . ,(trade-quantity trade))
      (:price . ,(format nil "~,4F" (trade-price trade))))))

(defmethod extract-context-data ((stats trade-stats))
  "Returns the relevant context data for a TRADE-STATS data."
  `#(((:stat-name . "End Date") (:stat-value . ,(or
                                                  (and (trade-stats-timestamp stats)
                                                     (local-time:format-timestring
                                                       nil (trade-stats-timestamp stats)
                                                       :format '(:year "-" (:month 2) "-" (:day 2))))
                                                  "N/A")))
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
      (:agent-prices . ,(let ((first t))
                         (map 'vector
                              (lambda (ts price)
                                `((:is-first . ,(prog1
                                                  first
                                                  (and first (setf first nil))))
                                  (:utc-date . ,(* 1000 (local-time:timestamp-to-unix ts)))
                                  (:price . ,(format nil "~,4F" price))))
                              (reverse (timestamps agent))
                              (reverse (revalprices agent)))))
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
      (:agent-in-trade-equity . 
       ,(let ((first t)
              (rolling-pl 0)
              (timestamps (reverse (timestamps agent)))
              (prices (reverse (revalprices agent))))
          (coerce
            (delete-duplicates
              (mapcan
                (lambda (trade-group)
                  (let ((ts-start (trade-group-entry-timestamp trade-group))
                        (ts-end (trade-group-exit-timestamp trade-group))
                        (trade-pl (trade-group-pl trade-group))
                        (position (trade-quantity (first (trade-group-trades trade-group))))
                        starting-equity
                        prev-price)
                    (loop while (local-time:timestamp< (first timestamps) ts-start)
                          do (pop timestamps)
                          do (pop prices))
                    (setf starting-equity rolling-pl
                          prev-price (first prices))
                    `(((:is-first . ,(prog1
                                       first
                                       (and first (setf first nil))))
                       (:utc-date . ,(* 1000 (local-time:timestamp-to-unix ts-start)))
                       (:equity . ,(format nil "~,4F" rolling-pl)))
                      ,@(loop while (local-time:timestamp< (first timestamps) ts-end)
                          for trade = (first (trade-group-trades trade-group)) then
                                      (find (first timestamps) (trade-group-trades trade-group)
                                            :key #'trade-timestamp :test #'local-time:timestamp=)
                          for position = (trade-quantity trade) then
                                          (if trade (+ position (trade-quantity trade)) position)
                          do (incf rolling-pl (* position (- (first prices) prev-price)))
                          collect `((:is-first . nil)
                                    (:utc-date . ,(* 1000 (local-time:timestamp-to-unix (first timestamps))))
                                    (:equity . ,(format nil "~,4F" rolling-pl)))
                          do (progn
                               (setf prev-price (first prices))
                               (pop timestamps)
                               (pop prices)))
                      ((:is-first . nil)
                       (:utc-date . ,(* 1000 (local-time:timestamp-to-unix ts-end)))
                       (:equity . ,(format nil "~,4F" (incf rolling-pl (- (+ starting-equity trade-pl) rolling-pl))))))))
                (reverse (trade-groups agent)))
              :test #'equalp)
            'vector)))
      (:agent-positions . ,(let ((first t))
                             (map 'vector
                                  (lambda (ts pos)
                                    `((:is-first . ,(prog1
                                                      first
                                                      (and first (setf first nil))))
                                      (:utc-date . ,(* 1000 (local-time:timestamp-to-unix ts)))
                                      (:position . ,(adjusted-position pos))))
                                  (reverse (timestamps agent))
                                  (reverse (positions agent)))))
      (:agent-trade-profit-losses . ,(let ((first t)
                                           (rolling-pl 0))
                                       (coerce
                                         (delete-duplicates
                                          (mapcan
                                           (lambda (trade-group)
                                             (let ((ts-start (trade-group-entry-timestamp trade-group))
                                                   (ts-end (trade-group-exit-timestamp trade-group))
                                                   (trade-pl (trade-group-pl trade-group)))
                                               `(((:is-first . ,(prog1
                                                                  first
                                                                  (and first (setf first nil))))
                                                  (:utc-date . ,(* 1000 (local-time:timestamp-to-unix ts-start)))
                                                  (:equity . ,(format nil "~,4F" rolling-pl)))
                                                 ((:is-first . nil)
                                                  (:utc-date . ,(* 1000 (local-time:timestamp-to-unix ts-end)))
                                                  (:equity . ,(format nil "~,4F" (incf rolling-pl trade-pl)))))))
                                           (reverse (trade-groups agent)))
                                          :test #'equalp)
                                         'vector)))
      (:buy-hold-equity . ,(let* ((first t)
                                  (first-trade (car (last (trades agent))))
                                  (first-trade-date (when (not (null first-trade))
                                                      (trade-timestamp first-trade)))
                                  prev-price
                                  (rolling-pl 0))
                             (map 'vector
                                  (lambda (ts p)
                                    ;; Force buy-hold start date to same date as first trade for valid comparison
                                    ;; unless no trades were made, then show equity from the first event
                                    (when (or (null prev-price)
                                              (and first-trade-date
                                                   (local-time:timestamp<= ts first-trade-date)))
                                      (setf prev-price p))
                                    (incf rolling-pl (- p prev-price))
                                    (setf prev-price p)
                                    `((:is-first . ,(prog1
                                                      first
                                                      (and first (setf first nil))))
                                      (:utc-date . ,(* 1000 (local-time:timestamp-to-unix ts)))
                                      (:equity . ,(format nil "~,4F" rolling-pl))))
                                  (reverse (timestamps agent))
                                  (reverse (revalprices agent)))))
      (:trading-stats . ,(extract-context-data (trade-stats agent))))))

(defun analyze (agents security-data &key (template-name "analysis"))
  "Create web pages that display the results and statistics for a trading simulation."
  ;; Add the location of the mustache templates to the search path
  (pushnew *ui-template-path* mustache:*load-path* :test #'string-equal :key #'namestring)
  ;; Process each agent
  (let (agent-analysis-files)
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
        (let* ((analysis-file (format nil "~A-analysis.html"
                                      (name agent)))
               (analysis-file-path (merge-pathnames analysis-file *analysis-results-path*)))
          (file-io:spit-file
            (mustache:render*
              (format nil "{{{> ~A}}}" template-name) analysis-context)
            analysis-file-path)
          (push analysis-file agent-analysis-files))))
    (format t "~&Analysis files can be found at this location: ~A~{~&  ~A~%~}"
            *analysis-results-path* (nreverse agent-analysis-files)))
  nil)

(defun compare-results (agents comparison-title security-data &key (template-name "comparison"))
  "Create a web page that compares the trading statistics for a set of trading agents."
  (assert (and (not (null agents)) (listp agents)))
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
            (:chart-title-prefix . ,(string-capitalize
                                      (substitute-if #\Space
                                                     (lambda (c)
                                                       (or (char= c #\-)
                                                           (char= c #\_)))
                                                     comparison-title)))
            (:buy-hold-data . ,(let* ((first t)
                                      (first-trade (car (last (trades agent))))
                                      (first-trade-date (when (not (null first-trade))
                                                          (trade-timestamp first-trade)))
                                      prev-price
                                      (rolling-pl 0))
                                 (map 'vector
                                      (lambda (ts p)
                                        ;; Force buy-hold start date to same date as first trade for valid comparison
                                        ;; unless no trades were made, then show equity from the first event
                                        (when (or (null prev-price)
                                                  (and first-trade-date
                                                       (local-time:timestamp<= ts first-trade-date)))
                                          (setf prev-price p))
                                        (incf rolling-pl (- p prev-price))
                                        (setf prev-price p)
                                        `((:is-first . ,(prog1
                                                          first
                                                          (and first (setf first nil))))
                                          (:utc-date . ,(* 1000 (local-time:timestamp-to-unix ts)))
                                          (:equity . ,(format nil "~,4F" rolling-pl))))
                                      (reverse (timestamps (first agents)))
                                      (reverse (revalprices (first agents))))))
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
    (let ((comparison-file (format nil "~A-comparison.html"
                                   (substitute-if #\_ (lambda (c) (char= c #\Space))
                                                  comparison-title))))
      (file-io:spit-file
        (mustache:render*
          (format nil "{{{> ~A}}}" template-name) analysis-context)
        (merge-pathnames :name comparison-file *analysis-results-path*))
      (format t "~&The result comparison file can be found at this location: ~A~A"
              *analysis-results-path* comparison-file))))

;; EOF
