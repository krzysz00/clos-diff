;; This file is part of clos-diff.

;; clos-diff is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; clos-diff is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with clos-diff in the file COPYING.  If not, see <http://www.gnu.org/licenses/>.

;; Copyright (C) 2010 Krzysztof Drewniak <krzysdrewniak (at) gmail (dot) com>

(in-package :clos-diff)

;;;; Internals of a diff list:

;;;; A diff is a list of n elements. The first element shall be a list of
;;;; two elements. Element 1 of that sublist shall be the type of object
;;;; neded to recieve the diff. Element 2 shall be the type of the object
;;;; the diff was created from. If Elm. 1 is equal to NIL, an object of
;;;; the appropriate type (Elm. 2) will be created.

;;;; The remaining items shall be lists, which shall be evaluated by
;;;; `diff-eval' accorging to the first element of the list. The remaining
;;;; elements of the list are arguments to the function specied at the top
;;;; of the list.

;;;; The allowed functions are
;;;;  * sv (2 args) Set the slot in arg1 to arg2
;;;;  * ii (2 args) Set the slot in arg1 to the object returned by
;;;;    allocate-instance called with arg2.
;;;;  * recur (2 args) Resucse apply on the value of arg1 with the diff in
;;;;    arg2
;;;; Any other function will throw an error

(defun get-slots (object)
  (mapcar #'closer-mop:slot-definition-name 
	  (closer-mop:class-slots (class-of object))))

(defun diff-nil (obj &optional type)
  (let ((diff (list (list type (type-of obj)))))
    (loop for slot in (get-slots obj) do
	 (let ((sv (slot-value obj slot)))
	   (if (typep sv 'standard-object)
	       (progn
		 (push `(ii ,slot ,(type-of sv)) diff)
		 (push `(recur ,slot ,(diff-nil sv (type-of sv))) diff))
	       (push `(sv ,slot ,sv) diff))))
    (setf diff (nreverse diff))))

(defun diff (old new &key (test #'equalp))
  (when (equal old nil) (return-from diff (diff-nil new)))
  (unless (eql (type-of old) (type-of new)) (error "Must diff objects of the same type."))
  (let ((diff (list (list (type-of new) (type-of old)))))
    (loop for slot in (get-slots new) do
	 (let ((svo (slot-value old slot)) (svn (slot-value new slot)))
	   (unless (funcall test svo svn)
	     (if (typep svn 'standard-object)
		 (push `(recur ,slot ,(diff svo svn :test test)) diff)
		 (push `(sv ,slot ,svn) diff)))))
    (setf diff (nreverse diff))))
