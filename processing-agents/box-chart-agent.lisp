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
  (setf (box-size a) (- (hi a) (lo a))))

(defmethod observe ((a box-chart-agent) (e market-update))
  (and (equal (security a) (security e))
       (not (member (type-of e) '(bar box)))))

(defmethod initialize ((a box-chart-agent))
  (with-slots (security box-size states name transitions hi lo column) a
    (when (null states)
      (push :start states)
      (setf name (format nil "BOX-CHART-AGENT_~A_~A" security box-size))
      (setf transitions
            `((:start . (,(make-instance
                            'transition
                            :initial-state :start
                            :final-state :start
                            :sensor #'price
                            :predicate #1=(lambda (p)
                                            (declare (ignore p))
                                            nil)
                            :actuator #1#)
                         ,(make-instance
                            'transition
                            :initial-state :start
                            :final-state :same-cross
                            :sensor #'price
                            :predicate #2=(lambda (p)
                                            (declare (ignore p))
                                            t)
                            :actuator (lambda (p)
                                        ;; Set box range for first price event based
                                        ;; on specified box size
                                        (loop while (> p hi)
                                              do (setf lo hi
                                                       hi (+ hi box-size)))
                                        (loop while (< p lo)
                                              do (setf hi lo
                                                       lo (- lo box-size)))
                                        (emit a (make-instance
                                                  'box
                                                  :timestamp (first (timestamps a))
                                                  :security security
                                                  :value (list column hi lo :cross)))))
                         ,(make-instance
                            'transition
                            :initial-state :start
                            :final-state :new-cross-higher
                            :sensor #'price
                            :predicate #1#
                            :actuator #1#)
                         ,(make-instance
                            'transition
                            :initial-state :start
                            :final-state :new-cross-right
                            :sensor #'price
                            :predicate #1#
                            :actuator #1#)
                         ,(make-instance
                            'transition
                            :initial-state :start
                            :final-state :same-circle
                            :sensor #'price
                            :predicate #1#
                            :actuator #1#)
                         ,(make-instance
                            'transition
                            :initial-state :start
                            :final-state :new-circle-lower
                            :sensor #'price
                            :predicate #1#
                            :actuator #1#)
                         ,(make-instance
                            'transition
                            :initial-state :start
                            :final-state :new-circle-right
                            :sensor #'price
                            :predicate #1#
                            :actuator #1#)))
              (:same-cross . (,(make-instance
                                 'transition
                                 :initial-state :same-cross
                                 :final-state :start
                                 :sensor #'price
                                 :predicate #1#
                                 :actuator #1#)
                              ,(make-instance
                                 'transition
                                 :initial-state :same-cross
                                 :final-state :same-cross
                                 :sensor #'price
                                 :predicate (lambda (p) (<= lo p hi))
                                 :actuator #2#)
                              ,(make-instance
                                 'transition
                                 :initial-state :same-cross
                                 :final-state :new-cross-higher
                                 :sensor #'price
                                 :predicate (lambda (p) (> p hi))
                                 :actuator (lambda (p)
                                             (loop while (> p hi)
                                                   do (progn
                                                        (setf lo hi
                                                              hi (+ hi box-size))
                                                        (emit a (make-instance
                                                                  'box
                                                                  :timestamp (first (timestamps a))
                                                                  :security security
                                                                  :value (list column hi lo :cross)))))))
                              ,(make-instance
                                 'transition
                                 :initial-state :same-cross
                                 :final-state :new-cross-right
                                 :sensor #'price
                                 :predicate #1#
                                 :actuator #1#)
                              ,(make-instance
                                 'transition
                                 :initial-state :same-cross
                                 :final-state :same-circle
                                 :sensor #'price
                                 :predicate #1#
                                 :actuator #1#)
                              ,(make-instance
                                 'transition
                                 :initial-state :same-cross
                                 :final-state :new-circle-lower
                                 :sensor #'price
                                 :predicate #1#
                                 :actuator #1#)
                              ,(make-instance
                                 'transition
                                 :initial-state :same-cross
                                 :final-state :new-circle-right
                                 :sensor #'price
                                 :predicate (lambda (p) (< p lo))
                                 :actuator (lambda (p)
                                             (incf column)
                                             (loop while (< p lo)
                                               do (progn
                                                   (setf hi lo
                                                         lo (- lo box-size))
                                                   (emit a (make-instance
                                                             'box
                                                             :timestamp (first (timestamps a))
                                                             :security security
                                                             :value (list column hi lo :circle)))))))))
              (:new-cross-higher . (,(make-instance
                                       'transition
                                       :initial-state :new-cross-higher
                                       :final-state :start
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-cross-higher
                                       :final-state :same-cross
                                       :sensor #'price
                                       :predicate #2#
                                       :actuator #2#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-cross-higher
                                       :final-state :new-cross-higher
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-cross-higher
                                       :final-state :new-cross-right
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-cross-higher
                                       :final-state :same-circle
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-cross-higher
                                       :final-state :new-circle-lower
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-cross-higher
                                       :final-state :new-circle-right
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)))
              (:new-cross-right . (,(make-instance
                                      'transition
                                      :initial-state :new-cross-right
                                      :final-state :start
                                      :sensor #'price
                                      :predicate #1#
                                      :actuator #1#)
                                   ,(make-instance
                                      'transition
                                      :initial-state :new-cross-right
                                      :final-state :same-cross
                                      :sensor #'price
                                      :predicate #2#
                                      :actuator #2#)
                                   ,(make-instance
                                      'transition
                                      :initial-state :new-cross-right
                                      :final-state :new-cross-higher
                                      :sensor #'price
                                      :predicate #1#
                                      :actuator #1#)
                                   ,(make-instance
                                      'transition
                                      :initial-state :new-cross-right
                                      :final-state :new-cross-right
                                      :sensor #'price
                                      :predicate #1#
                                      :actuator #1#)
                                   ,(make-instance
                                      'transition
                                      :initial-state :new-cross-right
                                      :final-state :same-circle
                                      :sensor #'price
                                      :predicate #1#
                                      :actuator #1#)
                                   ,(make-instance
                                      'transition
                                      :initial-state :new-cross-right
                                      :final-state :new-circle-lower
                                      :sensor #'price
                                      :predicate #1#
                                      :actuator #1#)
                                   ,(make-instance
                                      'transition
                                      :initial-state :new-cross-right
                                      :final-state :new-circle-right
                                      :sensor #'price
                                      :predicate #1#
                                      :actuator #1#)))
              (:same-circle . (,(make-instance
                                  'transition
                                  :initial-state :same-circle
                                  :final-state :start
                                  :sensor #'price
                                  :predicate #1#
                                  :actuator #1#)
                               ,(make-instance
                                  'transition
                                  :initial-state :same-circle
                                  :final-state :same-cross
                                  :sensor #'price
                                  :predicate #1#
                                  :actuator #1#)
                               ,(make-instance
                                  'transition
                                  :initial-state :same-circle
                                  :final-state :new-cross-higher
                                  :sensor #'price
                                  :predicate #1#
                                  :actuator #1#)
                               ,(make-instance
                                  'transition
                                  :initial-state :same-circle
                                  :final-state :new-cross-right
                                  :sensor #'price
                                  :predicate (lambda (p) (> p hi))
                                  :actuator (lambda (p)
                                              (incf column)
                                              (loop while (> p hi)
                                                    do (progn
                                                         (setf lo hi
                                                               hi (+ hi box-size))
                                                         (emit a (make-instance
                                                                   'box
                                                                   :timestamp (first (timestamps a))
                                                                   :security security
                                                                   :value (list column hi lo :cross)))))))
                               ,(make-instance
                                  'transition
                                  :initial-state :same-circle
                                  :final-state :same-circle
                                  :sensor #'price
                                  :predicate (lambda (p) (<= lo p hi))
                                  :actuator #2#)
                               ,(make-instance
                                  'transition
                                  :initial-state :same-circle
                                  :final-state :new-circle-lower
                                  :sensor #'price
                                  :predicate (lambda (p) (< p lo))
                                  :actuator (lambda (p)
                                              (loop while (< p lo)
                                                    do (progn
                                                         (setf hi lo
                                                               lo (- lo box-size))
                                                         (emit a (make-instance
                                                                   'box
                                                                   :timestamp (first (timestamps a))
                                                                   :security security
                                                                   :value (list column hi lo :cross)))))))
                               ,(make-instance
                                  'transition
                                  :initial-state :same-circle
                                  :final-state :new-circle-right
                                  :sensor #'price
                                  :predicate #1#
                                  :actuator #1#)))
              (:new-circle-lower . (,(make-instance
                                       'transition
                                       :initial-state :new-circle-lower
                                       :final-state :start
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-circle-lower
                                       :final-state :same-cross
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-circle-lower
                                       :final-state :new-cross-higher
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-circle-lower
                                       :final-state :new-cross-right
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-circle-lower
                                       :final-state :same-circle
                                       :sensor #'price
                                       :predicate #2#
                                       :actuator #2#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-circle-lower
                                       :final-state :new-circle-lower
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-circle-lower
                                       :final-state :new-circle-right
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)))
              (:new-circle-right . (,(make-instance
                                       'transition
                                       :initial-state :new-circle-right
                                       :final-state :start
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-circle-right
                                       :final-state :same-cross
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-circle-right
                                       :final-state :new-cross-higher
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-circle-right
                                       :final-state :new-cross-right
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-circle-right
                                       :final-state :same-circle
                                       :sensor #'price
                                       :predicate #2#
                                       :actuator #2#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-circle-right
                                       :final-state :new-circle-lower
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#)
                                    ,(make-instance
                                       'transition
                                       :initial-state :new-circle-right
                                       :final-state :new-circle-right
                                       :sensor #'price
                                       :predicate #1#
                                       :actuator #1#))))))))

(defmethod preprocess ((a box-chart-agent) (e market-update))
  (push 0 (positions a)))

;;EOF
