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

(defun eval-diff (obj phrase)
  (destructuring-bind (func arg1 arg2) phrase
      (ecase func
	(sv (setf (slot-value obj arg1) arg2))
	(ii (setf (slot-value obj arg1) (allocate-instance arg2)))
	(recur (setf (slot-value obj arg1)
		     (apply-diff arg2 (slot-value obj arg1)))))))

(defun apply-diff (diff &optional obj)
  (let ((top (pop diff)))
    (unless obj (setf obj (allocate-instance (first top))))
    (unless (equal (type-of obj) (first top)) (error "Object type mismatch.")))
  (loop for i in diff do
       (eval-diff obj i))
  obj)

