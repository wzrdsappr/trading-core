;;;; utility-functions.lisp

(in-package #:trading-core)

;;; utility-functions methods

(defun extract-initials (keyword)
  (loop for prev-char = nil then char
        for char across (symbol-name keyword)
        when (or (null prev-char) (char= prev-char #\-))
        collecting char into initials
        finally (return (coerce initials 'string))))

(defun avg-list (list &optional (key-fn #'identity))
  "Calculate the average value in a list in an efficient manner."
  (let ((sum 0)
        (len 0))
    (dolist (x list)
      (incf sum (funcall key-fn x))
      (incf len 1))
    (if (> len 0)
      (/ sum len)
      0)))

(defun sub-list (list start-index end-index)
  "Get subset of list without throwing errors if indexes are bad. START-INDEX and END-INDEX are inclusive."
  (if (< end-index start-index)
    nil
    (loop for i = 0 then (1+ i)
          for v in list
          when (<= start-index i end-index)
          collecting v)))

(defmacro case-equal (exp &body clauses)
  "Case macro with test EQUAL instead of standard EQ test."
  (let ((temp (gensym)))
    `(let ((,temp ,exp))
       (cond ,@(mapcar (lambda (clause)
                         (destructuring-bind (keys . clause-forms) clause
                           (cond ((eq keys 'otherwise)
                                  `(t ,@clause-forms))
                                 (t (if (atom keys)
                                      (setq keys (list keys)))
                                    `((member ,temp `,keys
                                              :test #'equal
                                              ,@clause-forms))))))
                       clauses)))))

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

 ;;EOF
