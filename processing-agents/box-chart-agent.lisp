;;;; box-chart-agent.lisp
;;;; This class provides the ability to convert prices into a box chart plot e.g.
;;;; |
;;;; |           X X X            
;;;; |           XOXOXO           
;;;; |       X   XOXOXO           
;;;; |       XOX XO OXO           
;;;; |       XOXOX  OXO           
;;;; |     X XO O   OXO           
;;;; |     XOX      O O           
;;;; |     XOX        OXO         
;;;; |     XOX        OXOX        
;;;; |     XOX        OXOXO       
;;;; |     XO         OXOXO       
;;;; |     X          OXO O  X     
;;;; |     X          OX  OX X     
;;;; |     X          OX  OXOX     
;;;; |O    X          O   OXOX    
;;;; |O  X X              O O     
;;;; |OX XOX                      
;;;; |OXOXOX                       
;;;; |OXOXO                         
;;;; |O OX                          
;;;; |  O                           
;;;; |______________________________

(in-package #:trading-core)

(defclass box-chart-agent (fsm-agent)
  ((box-size :accessor box-size :initform 0)
   (hi :accessor hi :initarg :hi)
   (lo :accessor lo :initarg :lo)
   (column :accessor column :initform 0)))

;;; box-chart-agent methods

(defmethod initialize-instance :after ((a box-chart-agent) &key)
  (assert (> (hi a) (lo a)))
  (setf (box-size a) (- hi lo)))

(defmethod observe ((a box-chart-agent) (e market-update))
  (and (equal (mkt a) (security e))
       (not (member (type-of e) '(bar box)))))

(defmethod initialize ((a box-chart-agent))
  (with-slots (states name name box-size transitions hi lo) a
    (when (null states)
      (push :start states)
      (setf name (format nil "BOX-CHART-AGENT_~A_~A" mkt box-size))
      (setf transitions `((:start . (,(make-instance
                                        'transition
                                        :initial-state :start
                                        :final-state :start
                                        :sensor #'price
                                        :predicate (lambda (p) nil)
                                        :actuator (lambda (p) nil))
                                      ,(make-instance
                                         'transition
                                         :initial-state :start
                                         :final-state :same-cross
                                         :sensor #'price
                                         :predicate (lambda (p) t)
                                         :actuator (lambda (p)
                                                     ;; Set box range for first price event based on specified box size
                                                     (while (> p hi)
                                                       (setf lo hi
                                                             hi (+ hi box-size)))
                                                     (while (< p lo)
                                                       (setf hi lo
                                                             lo (- lo box-size)))
                                                     (emit a (make-instance
                                                               'box
                                                               :timestamp (first (timestamps a))
                                                               :security mkt
                                                               :value (list column hi lo :cross)))
                                                     (logv:format-log "~S START -> SAME-CROSS ~%" name)))
                                      ,(make-instance
                                         'transition
                                         :initial-state :start
                                         :final-state :new-cross-higher
                                         :sensor #'price
                                         :predicate (lambda (p) nil)
                                         :actuator (lambda (p) nil))
                                      ,(make-instance
                                         'transition
                                         :initial-state :start
                                         :final-state :new-cross-right
                                         :sensor #'price
                                         :predicate (lambda (p) nil)
                                         :actuator (lambda (p) nil))
                                      ,(make-instance
                                         'transition
                                         :initial-state :start
                                         :final-state :same-circle
                                         :sensor #'price
                                         :predicate (lambda (p) nil)
                                         :actuator (lambda (p) nil))
                                      ,(make-instance
                                         'transition
                                         :initial-state :start
                                         :final-state :new-circle-lower
                                         :sensor #'price
                                         :predicate (lambda (p) nil)
                                         :actuator (lambda (p) nil))
                                      ,(make-instance
                                         'transition
                                         :initial-state :start
                                         :final-state :new-circle-right
                                         :sensor #'price
                                         :predicate (lambda (p) nil)
                                         :actuator (lambda (p) nil))))
                          (:same-cross . (,(make-instance
                                             'transition
                                             :initial-state :same-cross
                                             :final-state :start
                                             :sensor #'price
                                             :predicate (lambda (p) nil)
                                             :actuator (lambda (p) nil))
                                           ,(make-instance
                                              'transition
                                              :initial-state :same-cross
                                              :final-state :same-cross
                                              :sensor #'price
                                              :predicate (lambda (p) (<= lo p hi))
                                              :actuator (lambda (p)
                                                          (logv:format-log "~S SAME-CROSS -> SAME-CROSS ~%" name)))
                                           ,(make-instance
                                              'transition
                                              :initial-state :same-cross
                                              :final-state :new-cross-higher
                                              :sensor #'price
                                              :predicate (lambda (p) (> p hi))
                                              :actuator (lambda (p)
                                                          (while (> p hi)
                                                            (setf lo hi
                                                                  hi (+ hi box-size))
                                                            (emit a (make-instance
                                                                      'box
                                                                      :timestamp (first (timestamps a))
                                                                      :security mkt
                                                                      :value (list column hi lo :cross))))
                                                          (logv:format-log "~S SAME-CROSS -> NEW-CROSS-HIGHER ~%" name)))
                                           ,(make-instance
                                              'transition
                                              :initial-state :same-cross
                                              :final-state :new-cross-right
                                              :sensor #'price
                                              :predicate (lambda (p) nil)
                                              :actuator (lambda (p) nil))
                                           ,(make-instance
                                              'transition
                                              :initial-state :same-cross
                                              :final-state :same-circle
                                              :sensor #'price
                                              :predicate (lambda (p) nil)
                                              :actuator (lambda (p) nil))
                                           ,(make-instance
                                              'transition
                                              :initial-state :same-cross
                                              :final-state :new-circle-lower
                                              :sensor #'price
                                              :predicate (lambda (p) nil)
                                              :actuator (lambda (p) nil))
                                           ,(make-instance
                                              'transition
                                              :initial-state :same-cross
                                              :final-state :new-circle-right
                                              :sensor #'price
                                              :predicate (lambda (p) (< p lo))
                                              :actuator (lambda (p)
                                                          (incf column)
                                                          (while (< p lo)
                                                            (setf hi lo
                                                                  lo (- lo box-size))
                                                            (emit a (make-instance
                                                                      'box
                                                                      :timestamp (first (timestamps a))
                                                                      :security mkt
                                                                      :value (list column hi lo :circle))))
                                                          (logv:format-log "~S SAME-CROSS -> NEW-CIRCLE-RIGHT ~%" name)))))
                          (:new-cross-higher . (,(make-instance
                                                   'transition
                                                   :initial-state :new-cross-higher
                                                   :final-state :start
                                                   :sensor #'price
                                                   :predicate (lambda (p) nil)
                                                   :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-cross-higher
                                                    :final-state :same-cross
                                                    :sensor #'price
                                                    :predicate (lambda (p) t)
                                                    :actuator (lambda (p)
                                                                (logv:format-log "~S NEW-CROSS-HIGHER -> SAME-CROSS ~%" name)))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-cross-higher
                                                    :final-state :new-cross-higher
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-cross-higher
                                                    :final-state :new-cross-right
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-cross-higher
                                                    :final-state :same-circle
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-cross-higher
                                                    :final-state :new-circle-lower
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-cross-higher
                                                    :final-state :new-circle-right
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil))
                                                 ))
                          (:new-cross-right . (,(make-instance
                                                  'transition
                                                  :initial-state :new-cross-right
                                                  :final-state :start
                                                  :sensor #'price
                                                  :predicate (lambda (p) nil)
                                                  :actuator (lambda (p) nil))
                                                ,(make-instance
                                                   'transition
                                                   :initial-state :new-cross-right
                                                   :final-state :same-cross
                                                   :sensor #'price
                                                   :predicate (lambda (p) t)
                                                   :actuator (lambda (p)
                                                               (logv:format-log "~S NEW-CROSS-RIGHT -> SAME-CROSS ~%" name)))
                                                ,(make-instance
                                                   'transition
                                                   :initial-state :new-cross-right
                                                   :final-state :new-cross-higher
                                                   :sensor #'price
                                                   :predicate (lambda (p) nil)
                                                   :actuator (lambda (p) nil))
                                                ,(make-instance
                                                   'transition
                                                   :initial-state :new-cross-right
                                                   :final-state :new-cross-right
                                                   :sensor #'price
                                                   :predicate (lambda (p) nil)
                                                   :actuator (lambda (p) nil))
                                                ,(make-instance
                                                   'transition
                                                   :initial-state :new-cross-right
                                                   :final-state :same-circle
                                                   :sensor #'price
                                                   :predicate (lambda (p) nil)
                                                   :actuator (lambda (p) nil))
                                                ,(make-instance
                                                   'transition
                                                   :initial-state :new-cross-right
                                                   :final-state :new-circle-lower
                                                   :sensor #'price
                                                   :predicate (lambda (p) nil)
                                                   :actuator (lambda (p) nil))
                                                ,(make-instance
                                                   'transition
                                                   :initial-state :new-cross-right
                                                   :final-state :new-circle-right
                                                   :sensor #'price
                                                   :predicate (lambda (p) nil)
                                                   :actuator (lambda (p) nil))))
                          (:same-circle . (,(make-instance
                                              'transition
                                              :initial-state :same-circle
                                              :final-state :start
                                              :sensor #'price
                                              :predicate (lambda (p) nil)
                                              :actuator (lambda (p) nil))
                                            ,(make-instance
                                               'transition
                                               :initial-state :same-circle
                                               :final-state :same-cross
                                               :sensor #'price
                                               :predicate (lambda (p) nil)
                                               :actuator (lambda (p) nil))
                                            ,(make-instance
                                               'transition
                                               :initial-state :same-circle
                                               :final-state :new-cross-higher
                                               :sensor #'price
                                               :predicate (lambda (p) nil)
                                               :actuator (lambda (p) nil))
                                            ,(make-instance
                                               'transition
                                               :initial-state :same-circle
                                               :final-state :new-cross-right
                                               :sensor #'price
                                               :predicate (lambda (p) (> p hi))
                                               :actuator (lambda (p)
                                                           (incf column)
                                                           (while (> p hi)
                                                             (setf lo hi
                                                                   hi (+ hi box-size))
                                                             (emit a (make-instance
                                                                      'box
                                                                      :timestamp (first (timestamps a))
                                                                      :security mkt
                                                                      :value (list column hi lo :cross))))
                                                           (logv:format-log "~S SAME-CIRCLE -> NEW-CROSS-RIGHT ~%" name)))
                                            ,(make-instance
                                               'transition
                                               :initial-state :same-circle
                                               :final-state :same-circle
                                               :sensor #'price
                                               :predicate (lambda (p) (<= lo p hi))
                                               :actuator (lambda (p)
                                                           (logv:format-log "~S SAME-CIRCLE -> SAME-CIRCLE ~%" name)))
                                            ,(make-instance
                                               'transition
                                               :initial-state :same-circle
                                               :final-state :new-circle-lower
                                               :sensor #'price
                                               :predicate (lambda (p) (< p lo))
                                               :actuator (lambda (p)
                                                           (while (< p lo)
                                                             (setf hi lo
                                                                   lo (- lo box-size))
                                                             (emit a (make-instance
                                                                      'box
                                                                      :timestamp (first (timestamps a))
                                                                      :security mkt
                                                                      :value (list column hi lo :cross))))
                                                           (logv:format-log "~S SAME-CIRCLE -> NEW-CIRCLE-LOWER ~%" name)))
                                            ,(make-instance
                                               'transition
                                               :initial-state :same-circle
                                               :final-state :new-circle-right
                                               :sensor #'price
                                               :predicate (lambda (p) nil)
                                               :actuator (lambda (p) nil))))
                          (:new-circle-lower . (,(make-instance
                                                   'transition
                                                   :initial-state :new-circle-lower
                                                   :final-state :start
                                                   :sensor #'price
                                                   :predicate (lambda (p) nil)
                                                   :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-circle-lower
                                                    :final-state :same-cross
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-circle-lower
                                                    :final-state :new-cross-higher
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-circle-lower
                                                    :final-state :new-cross-right
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-circle-lower
                                                    :final-state :same-circle
                                                    :sensor #'price
                                                    :predicate (lambda (p) t)
                                                    :actuator (lambda (p)
                                                                (logv:format-log "~S NEW-CIRCLE-LOWER -> SAME-CIRCLE ~%" name)))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-circle-lower
                                                    :final-state :new-circle-lower
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-circle-lower
                                                    :final-state :new-circle-right
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil))))
                          (:new-circle-right . (,(make-instance
                                                   'transition
                                                   :initial-state :new-circle-right
                                                   :final-state :start
                                                   :sensor #'price
                                                   :predicate (lambda (p) nil)
                                                   :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-circle-right
                                                    :final-state :same-cross
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-circle-right
                                                    :final-state :new-cross-higher
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-circle-right
                                                    :final-state :new-cross-right
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-circle-right
                                                    :final-state :same-circle
                                                    :sensor #'price
                                                    :predicate (lambda (p) t)
                                                    :actuator (lambda (p)
                                                                (logv:format-log "~S NEW-CIRCLE-RIGHT -> SAME-CIRCLE ~%" name)))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-circle-right
                                                    :final-state :new-circle-lower
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil))
                                                 ,(make-instance
                                                    'transition
                                                    :initial-state :new-circle-right
                                                    :final-state :new-circle-right
                                                    :sensor #'price
                                                    :predicate (lambda (p) nil)
                                                    :actuator (lambda (p) nil)))))))))

(defmethod preprocess ((a box-chart-agent) (e market-update))
  (push 0 (positions a)))

;;EOF
