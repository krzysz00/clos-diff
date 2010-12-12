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

(defpackage clos-diff-system 
  (:use :cl :asdf))

(in-package :clos-diff-system)

(defsystem "clos-diff"
    :depends-on ("closer-mop")
    :name "clos-diff"
    :version "something"
    :author "Krzysztof Drewniak <krzysdrewniak@gmail.com>"
    :licence "GNU GPL version 3 or any larter version"
    :serial t
    :components ((:file "package")
		  (:file "diff")
		  (:file "apply")))
