;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CIRCULAR-BUFFER; Base: 10 -*-
;;;
;;; (c) copyright 2007-2008 by
;;; Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307 USA.
;;;
;;; -----------------------------------------------------------------------
;;; Code extracted from the SEMI-PRECIOUS library since said library is
;;; not currently available via Quicklisp.  The full library can be found
;;; here: https://github.com/deepfire/semi-precious
;;; Changes from the original version:
;;; 1. Included the package definition in the same file as the rest of the
;;;    code.
;;; 2. Renamed all functions that started with the name CIRCULAR-BUFFER-* to
;;;    CB-* to make the usage more managable.
;;; 2. Converted DO-CIRCULAR-BUFFER macro and CB-SUBLIST-HEADWARDS
;;;    function to use LOOP to remove the dependency on the ITERATE library.


(defpackage #:circular-buffer
  (:nicknames :circbuf)
  (:use :common-lisp :alexandria)
  (:export
   #:circular-buffer
   #:cb-elt
   #:cb-elt-tailwards
   #:cb-shrink
   #:cb-extend
   #:cb-push
   #:cb-clear
   #:cb-size
   #:cb-limit
   #:cb-sublist-headwards
   #:do-circular-buffer))

(in-package :circular-buffer)

(defclass circular-buffer ()
  ((store :accessor store :type vector)
   (head :accessor head :type (integer 0) :initform 0)
   (tail :accessor tail :type (integer 0) :initform 0)
   (granularity :accessor granularity :type (integer 0) :initarg :granularity)
   (charge :accessor charge :type (integer 0) :initform 0))
  (:default-initargs
   :granularity 16))

(defmethod initialize-instance :after ((o circular-buffer) &rest rest &key size granularity)
  (declare (ignore rest) (type (integer 1) size))
  (let ((actual-size (+ (- size (mod size granularity)) granularity)))
    (setf (store o) (make-array actual-size :initial-element nil :adjustable t)
          (charge o) (- actual-size (1+ size)))))

(defun cb-limit (o)
  (- (array-dimension (store o) 0) (charge o)))

(defun cb-size (o)
  (mod (+ (head o) (cb-limit o) (- (tail o))) (cb-limit o)))

(defun cb-clear (o)
  (setf (head o) 0 (tail o) 0)
  (adjust-array (store o) (cb-limit o) :initial-element nil))

(defun cb-prev (x o)
  (mod (1- x) (cb-limit o)))

(defun cb-next (x o)
  (mod (1+ x) (cb-limit o)))

(defun pointer-cap-by-size (o n)
  (min n (cb-size o)))

(defun headwards-pointer (o n)
  (mod (+ (tail o) n) (cb-limit o)))

(defun tailwards-pointer (o n)
  (mod (- (head o) n (- (cb-limit o))) (cb-limit o)))

(defun cb-elt (i o)
  (aref (store o) (headwards-pointer o (pointer-cap-by-size o i))))

(defun cb-elt-tailwards (i o)
  (aref (store o) (tailwards-pointer o (pointer-cap-by-size o (1+ i)))))

(defun (setf cb-elt) (val i o)
  (setf (aref (store o) (headwards-pointer o (pointer-cap-by-size o i))) val))

(defun (setf cb-elt-tailwards) (val i o)
  (setf (aref (store o) (tailwards-pointer o (pointer-cap-by-size o (1+ i)))) val))

(defun cb-shrink-store (o physaddr)
  (replace (store o) (store o) :start1 physaddr :start2 (1+ physaddr))
  (incf (charge o)))

(defun cb-extend-store (o physaddr)
  (when (zerop (charge o))
    (setf (store o) (adjust-array (store o) (+ (array-dimension (store o) 0) (granularity o))))
    (incf (charge o) (granularity o)))
  (decf (charge o))
  (replace (store o) (store o) :start1 (1+ physaddr) :start2 physaddr))

(defun cb-shrink (o n)
  (when (plusp (cb-size o))
    (let ((physaddr (tailwards-pointer o (pointer-cap-by-size o (1+ n)))))
      (cb-shrink-store o physaddr)
      (unless (< (head o) physaddr)
        (decf (head o))))))

(defun cb-extend (o n)
  (let ((physaddr (tailwards-pointer o (pointer-cap-by-size o (1+ n)))))
    (cb-extend-store o physaddr)
    (unless (< (head o) physaddr)
      (incf (head o)))))

(defun cb-push (obj o)
  (declare (type circular-buffer o))
  (setf (aref (store o) (head o)) obj
        (head o) (cb-next (head o) o))
  (when (= (head o) (tail o))
    (setf (aref (store o) (tail o)) nil
          (tail o) (cb-next (tail o) o))))

(defun cb-sublist-headwards (o from &optional (count (1+ from)))
  (declare (type (integer 0) count from))
  (let* ((from (pointer-cap-by-size o (1+ from)))
         (start (tailwards-pointer o from))
         (count (min count from))) ;; cap the desire by start
    (loop for i = start then (cb-next i o)
          for j from 0 below count
          collect (aref (store o) i))))

(defmacro do-circular-buffer ((var o &key start) &body body)
  (with-gensyms (i)
    (once-only (o start)
      `(loop for ,i from (if (numberp ,start) (mod ,start (cb-size ,o)) 0)
               below (cb-size ,o) 
             for ,var = (cb-elt ,i ,o)
             do (progn
                  ,@body)))))

;; EOF
