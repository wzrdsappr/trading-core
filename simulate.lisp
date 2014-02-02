;;;; simulate.lisp

(in-package #:trading-core)

(defparameter *historical-data-path* "C:/Worden/TeleChart/Export/SP500_Components/"
  "Location of the historical data used in the simulation.")

(defparameter *ui-template-path*
  #P"C:/Users/Jonathan/Lisp/projects/trading-core/trading-ui/templates/"
  "Base directory where the cl-mustache library can find the UI templates.")

(defparameter *analysis-results-path*
  #P"C:/Users/Jonathan/Lisp/projects/trading-core/trading-ui/"
  "Base directory where the cl-mustache library can find the UI templates.")

;; TODO : Move this code to an example file.  At the point this is being executed
;; the agents probabably haven't been added to the *agents* parameter yet.
(push (make-instance 'aggregate-agent
                     :name "MyAggregateAgent"
                     :members (list *agents*))
      *aggregate-agents*)

(defparameter *csv-pattern-regex* (cl-ppcre:create-scanner "([0-9.,]+)([A-Z]+)"))

(defun create-sexpr (match &rest registers)
  (format nil "(~A~{\"~A\"~^ ~})"
          (map 'string (lambda (c) (if (char= c #\,) #\Space c)) (first registers))
          (rest registers)))

(defun load-event-data (data-name &key
                                  (data-format :prc)
                                  (data-dir *historical-data-path*)
                                  (start-date nil)
                                  (end-date nil))
  "Read a comma-separated data file of historical price data and create
appropriate market-update price classes for each data record. Optionally
filter out prices based on a desired date range."
  (let ((data-path (make-pathname
                     :defaults data-dir
                     :name data-name
                     :type "txt"))
        (security (intern data-name "KEYWORD"))
        events)
    (when (probe-file data-path)
      (let ((price-data (concatenate 'string
                                     "("
                                     (cl-ppcre:regex-replace-all *csv-pattern-regex*
                                                                 (file-io:slurp-file data-path)
                                                                 #'create-sexpr
                                                                 :simple-calls t)
                                     ")"))
            data-time-series)
        (setf data-time-series (safe-read-from-string price-data :data-error))
        (when (eq data-time-series :data-error)
          (error "Unable to import data for ~A security. Code injection point found." data-name))
        (mapc (lambda (r)
                (let ((date (if (< (first r) 100000000)
                              (* (first r) 10000)
                              (first r))))
                  (when (and (or (not start-date) (>= date start-date))
                             (or (not end-date) (<= date end-date)))
                    (push (make-instance
                            (ccase data-format
                                   (:prc 'prc)
                             (cluster-agents *agents* 10 :tpl)      (:book 'book)
                                   (:delta 'delta)
                                   (:bar 'bar))
                            :security security
                            :timestamp (u-d-h-m date)
                            :value (rest r))
                          events))))
              data-time-series))
      (nreverse events))))

(defun compute-future-data (historical-events)
  "Compute a set of \"future\" data based on a set of historical events."
  (cond ((typep (first historical-events) 'prc)
         (compute-future-prc-data historical-events))
        ((typep (first historical-events) 'bar)
         (compute-future-bar-data historical-events))
        (t (error "Forward casting of type ~A events is not supported."
                  (type-of (first historical-events))))))

(defun classify-price-change (price-change ranges)
  (labels ((check-range (range)
             (and (>= price-change (second range))
                  (< price-change (third range)))))
    (loop for range in ranges
          for range-found = (check-range range)
          when range-found return range)))

(defun compute-future-prc-data (historical-events)
  "Compute a set of \"future\" data based on a set of historical events."
  (multiple-value-bind (historical-changes max-positive-change max-negative-change)
      (loop for e = (first historical-events) then next-e
            for next-e in (rest historical-events)
            while (not (null next-e))
            for percent-change = (/ (- (last-price next-e) (last-price e)) (last-price e))
            collecting percent-change into percent-changes
            maximizing (max 0 percent-change) into max-pos-change
            minimizing (min 0 percent-change) into max-neg-change
            finally (return (values percent-changes max-pos-change max-neg-change)))
    (multiple-value-bind (positive-numeric-predicates positive-bins)
        (interval-division-predicates-bins 0 (+ max-positive-change least-positive-normalized-single-float) 3
                                           :hard-lower-bound t :hard-upper-bound t)
      (multiple-value-bind (negative-numeric-predicates negative-bins)
        (interval-division-predicates-bins max-negative-change 0 3
                                           :hard-lower-bound t :hard-upper-bound t)
        (let* ((ranges (pairlis '(:neg-tail :neg-mid :neg-body :pos-body :pos-mid :pos-tail)
                                (union negative-bins positive-bins)))
               (historical-ranges (loop for change in historical-changes
                                        collecting (classify-price-change change ranges))))
          (loop for future-prices = (list (random 1000))
                for (range min-change max-change) in historical-ranges
                do (push (* (first future-prices) (+ min-change (random (- max-change min-change))))
                         future-prices)
                finally (return (nreverse future-prices))))))))

;; TODO : Bar future price calculations not yet implemented
(defun compute-future-bar-data (historical-events)
  "Compute a set of \"future\" data based on a set of historical events."
  (let* ((percent-innovations ()))
    nil))

(defun run-simulation (events)
  (dolist (a *agents*)
    (initialize a))
  (setf *events-queue* events)
  (loop for e = (pop *events-queue*)
        while e do (dolist (a *agents*)
                     (consume a e))))

(defun interval-division-predicates-bins (min-lvl max-lvl num-bins
                                          &key (hard-lower-bound nil) (hard-upper-bound nil))
  "Generate the set of predicates needed to group a set of agents into a specific
number of bins. The HARD-LOWER-BOUND and HARD-UPPER-BOUND options allow strict limitations
of the covered bin ranges."
  (let ((predicates-list nil)
        (bins-list nil)
        (subdivision (/ (- max-lvl min-lvl) num-bins))
        (first-pred (lambda (x) (< x min-lvl)))
        (first-bin (list most-negative-short-float min-lvl))
        (last-pred (lambda (x) (>= x max-lvl)))
        (last-bin (list max-lvl most-positive-short-float)))
    (unless hard-lower-bound
      (push first-pred predicates-list)
      (push first-bin bins-list))
    (dotimes (i num-bins)
      (let* ((bin-left (+ min-lvl (* i subdivision)))
             (bin-right (+ bin-left subdivision)))
        (push (lambda (x)
                (and (>= x bin-left)
                     (< x bin-right)))
              predicates-list)
        (push (list bin-left bin-right) bins-list)))
    (unless hard-upper-bound
      (push last-pred predicates-list)
      (push last-bin bins-list))
    (values (nreverse predicates-list) (nreverse bins-list))))

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
             (let ((tradestat (first (tradestats a))))
               (case what
                 (:tpl (trade-stats-total-pl tradestat))
                 (:lrt (trade-stats-average-logret tradestat))
                 (:wtl (trade-stats-win-to-loss tradestat))
                 (:pcp (trade-stats-percent-profitable tradestat))
                 (:pff (trade-stats-profit-factor tradestat))))))
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

(defmethod extract-context-data ((p prc))
  `((:utc-date . ,(* 1000 (julian-to-unix-timestamp (timestamp p))))
    (:price . ,(value p)) (:high . ,(value p))
    (:low . ,(value p)) (:close . ,(value p))
    (:volume . 0)))

(defmethod extract-context-data ((p bar))
  `((:utc-date . ,(* 1000 (julian-to-unix-timestamp (timestamp p))))
    (:open . ,(o p)) (:high . ,(h p))
    (:low . ,(l p)) (:close . ,(c p))
    (:volume . ,(volume p))))

(defmethod extract-context-data ((trade trade))
  (let* ((unix-timestamp (julian-to-unix-timestamp (trade-timestamp trade)))
         (local-timestamp (local-time:unix-to-timestamp unix-timestamp)))
    `((:utc-date . ,(* 1000 unix-timestamp))
      (:display-date . ,(local-time:format-timestring
                          nil local-timestamp
                          :format '(:year "-" (:month 2) "-" (:day 2))))
      (:quantity . ,(trade-quantity trade))
      (:price . ,(/ (fround (trade-price trade) .01) 100)))))

;; TODO : Add final trade stats to agent analysis context so mustache template engine can populate that
;; data on the agent UI page.
(defun analyze (agents security-data)
  "PARTIAL IMPLEMENTATION. Function to create web pages that display the results of a trading simulation."
  ;; Add the location of the mustache templates to the search path
  (pushnew *ui-template-path* mustache:*load-path* :test #'string-equal :key #'namestring)
  ;; Process each agent
  (dolist (agent agents)
    (let* ((trades (reverse (trades agent)))
           (analysis-context
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
               (:agent-trades .  ((:all-trades . ,(let ((first t))
                                                    (map 'vector
                                                         (lambda (trade)
                                                           `((:is-first . ,(prog1
                                                                             first
                                                                             (and first (setf first nil))))
                                                             ,@(extract-context-data trade)))
                                                         trades)))
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
                                                           (remove-if #'plusp trades :key #'trade-quantity))))))
               (:agent-profit-losses . ,(let ((first t)
                                              (rolling-pl 0))
                                          (map 'vector
                                             (lambda (ts pl)
                                               (incf rolling-pl pl)
                                               `((:is-first . ,(prog1
                                                                 first
                                                                 (and first (setf first nil))))
                                                 (:utc-date . ,(* 1000 (julian-to-unix-timestamp ts)))
                                                 (:equity . ,(/ (fround rolling-pl .001) 1000))))
                                             (reverse (timestamps agent))
                                             (reverse (pls agent))))))))
      (file-io:spit-file
        (mustache:mustache-render-to-string "{{{> analysis}}}" analysis-context)
        (make-pathname :name (format nil "~A-~A-analysis"
                                     (name agent) (symbol-name (security agent)))
                       :type "html"
                       :defaults *analysis-results-path*))))
  nil)

;; EOF
