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

(defparameter *tacky-instantiated-classes* nil
  "The list of classes that have been instantiated in the current
  system's life")
(defparameter *tacky-classes* nil
  "The alist of classes that have been defined to be instantiatable;
  includes their class structure")


(defun deftacky-underlying (typename slotnames)
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
      ((cdr (assoc typename *tacky-classes*))
       (setf (cdr (assoc typename *tacky-classes*)) new-object))
      ;; New...
      (t
       (setf *tacky-classes*
             (acons typename new-object *tacky-classes*))))
    new-object))

(defmacro deftacky (typename slotnames)
  `(deftacky-underlying `,' ,typename `,' ,slotnames))

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

(defun make-tacky-instance (class-name)
  (unless (member class-name *tacky-instantiated-classes*)
    (setf *tacky-instantiated-classes*
          (adjoin class-name *tacky-instantiated-classes*)))
  (make-tacky-instance-skeleton
   (cdr (assoc class-name *tacky-classes*))))

(defun slot-reader (object slot)
  (funcall object slot))

(defun slot-writer (object slot value)
  (funcall object slot value))

(defparameter *generic-function-list* nil
  "Contains all the known generic functions. Each generic function is
  actually a dispatching mechanism; a method on a generic function
  attempts to resolve the object being specified")

(defmacro deftacky-generic (name)
  "Define a generic function with name `name`"
  `(if (cdr (assoc `,' ,name *generic-function-list*))
       ;; if name already known?
       (setf (cdr (assoc `,' ,name *generic-function-list*))
             nil)
       ;; otherwise create an
       (setf *generic-function-list*
             (acons `,' ,name nil *generic-function-list*))))

;; defines test objects - both horses and pants have colors
(deftacky horse (rider height color))
(deftacky pants (size color))

(defmacro deftacky-method (name args &body body)
  "Adds body as a lambda function to the generic list"
  (let ((function-body (gensym)))
    `(let ((,function-body
           (lambda (,@(mapcar #'car args))
            ,@body )))
      (if (not (cdr (assoc `,' ,name *generic-function-list*)))
          (setf (cdr (assoc `,' ,name *generic-function-list*))
                (list
                 (pairlis
                  '(:args :function)
                  (list
                   (mapcan #'cdr `,' ,args)
                   ,function-body))))
          (setf (cdr (assoc `,' ,name *generic-function-list*))
                (push
                 (pairlis '(:args :function)
                          (list
                           (mapcan #'cdr `,' ,args)
                           ,function-body))
                 (cdr (assoc `,' ,name *generic-function-list*)))))
      ,function-body)))

(deftacky-generic paint)
(deftacky-method paint ((o horse) (p person))
  (princ "You painted a horse?"))

(deftacky-method paint ((o pants))
  (princ "You painted on your pants!"))

(deftacky-method paint ((o house) (k brush))
  (princ "You painted your house!"))

(defun select-appropriate-method-call (name wanted-arglist)

  (let ((list-of-possibilities
         (cdr (assoc name *generic-function-list*))))
    (dolist (option-set list-of-possibilities)
      (if (equalp (cdr (assoc :args option-set)) wanted-arglist)
          (format t "found it!")))
    ))
