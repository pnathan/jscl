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

;(in-package :cl)

;;; this is an ad-hoc crappy OO system called: tacky (as opposite of
;;; classy, ha, ha, ha).

(defun %deftacky (typename &rest slotnames)
  (list
   (cons :typename typename)
   (cons :slots
         slotnames)
   (cons :slot-contents
         (list (mapcar #'(lambda (slot)
                           (cons slot nil))
                       slotnames)))))

(defun make-tacky (tacky-class)
  (lambda (slot &optional value)
    (if value
        (set (assoc slot))
        (cdr (assoc )))))

(defun send (object))
