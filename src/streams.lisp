;;; streams.lisp ---

;; Copyright (C) 2013 Paul Nathan

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; streams.lisp

;;; Goal:
;;; http://clhs.lisp.se/Body/c_stream.htm
;;; Note that we have the idea of System Classes to deal with.
;;;

;;; def!struct doesnt work currently.

;; (in-package :cl)

;;; this is an ad-hoc crappy OO system called: tacky (as opposite of
;;; classy, ha, ha, ha).

(defparameter *tacky-list* nil)

(defun deftacky (typename &rest slotnames)
  (let ((new-object
         (list
          (cons :typename typename)
          (cons :slots
                slotnames)
          (cons :slot-contents
                (list (mapcar #'(lambda (slot)
                                  (cons slot t))
                              slotnames))))))

    (cond
      ;; Already defined
      ((cdr (assoc typename *tacky-list*))
       (setf (cdr (assoc typename *tacky-list*)) new-object))
      ;; New...
      (t
       (setf *tacky-list*
             (acons typename new-object *tacky-list*))))
    new-object))

(defun make-tacky-instance (class-name)
  (make-tacky-instance-skeleton
   (cdr (assoc class-name *tacky-list*))))

(defun make-tacky-instance-skeleton (tacky-class)
  "Returns an object.
Messages to the object can either be slotnames or-
:typename - returns the name of the class"
  (let ((new-object
         (copy-tree tacky-class)))
    #'(lambda (slot &optional value)
        (cond
          ((eq slot :typename)
           (cdr (assoc :typename tacky-class)))
          (value
           (setf (cdr (assoc slot (cadr (assoc :slot-contents
                                              new-object))))
                 value))
          (t
           (cdr (assoc slot (cadr (assoc :slot-contents
                                        new-object)))))))))


(defun slot-reader (object slot)
  (funcall object slot))

(defun slot-writer (object slot value)
  (funcall object slot value))


(defparameter *generic-function-list* nil
  "Contains all the known generic functions. Each generic function is
  actually a dispatching mechanism; a method on a generic function
  attempts to resolve the object being specified")

(defmacro deftacky-generic (name &rest arglist)
  `(if (cdr (assoc `,' ,name *generic-function-list*))
       (setf (cdr (assoc `,' ,name *generic-function-list*))
             `,' ,arglist)
       (setf *generic-function-list*
             (acons `,' ,name `,' ,arglist *generic-function-list*))))


(deftacky-generic horse (rider height))
(deftacky-generic pants (size color))

(defmacro deftacky-method (name args &body body)
  "Adds body as a lambda function to the generic list"
  (mapcar #'cdr ,@args)
  `(lambda (,@(mapcar #'car args)) ,@body ))

(deftacky-method insult-rider ((o horse ))
  (princ "You moron"))

(defun select-appropriate-method-call (name arglist))
