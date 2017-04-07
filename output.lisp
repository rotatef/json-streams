;;;;  json-streams
;;;;
;;;;  Copyright (C) 2017 Thomas Bakketun <thomas.bakketun@copyleft.no>
;;;;
;;;;  This library is free software: you can redistribute it and/or modify
;;;;  it under the terms of the GNU Lesser General Public License as published
;;;;  by the Free Software Foundation, either version 3 of the License, or
;;;;  (at your option) any later version.
;;;;
;;;;  This library is distributed in the hope that it will be useful,
;;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;  GNU General Public License for more details.
;;;;
;;;;  You should have received a copy of the GNU General Public License
;;;;  along with this library.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:json-streams)


(defun call-with-json-output (target options function)
  (if target
      (with-open-json-stream (*json-stream* (apply #'make-json-output-stream target options))
        (funcall function))
      (with-output-to-string (out)
        (with-open-json-stream (*json-stream* (apply #'make-json-output-stream out options))
          (funcall function)))))


(defmacro with-json-output ((&optional target &rest options) &body body)
  `(call-with-json-output ,target (list ,@options) (lambda () ,@body)))


(defmacro with-json-array (&body body)
  `(progn
     (json-write :begin-array *json-stream*)
     ,@body
     (json-write :end-array *json-stream*)))


(defmacro with-json-object (&body body)
  `(progn
     (json-write :begin-object *json-stream*)
     ,@body
     (json-write :end-object *json-stream*)))


(defmacro with-json-member (key &body body)
  `(progn
     (json-write (json-encode-key ,key *json-stream*) *json-stream*)
     ,@body))


(defun json-output-member (key value)
  (json-write (json-encode-key key *json-stream*) *json-stream*)
  (json-output-value value))


(defun json-output-value (value)
  (etypecase value
    ((or string real)
     (json-write value *json-stream*))
    (sequence
     (with-json-array
       (map nil #'json-output-value value)))
    (hash-table
     (with-json-object
       (maphash #'json-output-member value)))))


(defun json-output-boolean (value)
  (json-write (if value :true :false) *json-stream*))


(defun json-output-null ()
  (json-write :null *json-stream*))


(defun json-output-alist (alist)
  (with-json-object
    (loop for (key . value) in alist do
          (json-output-member key value))))


(defun json-output-plist (plist)
  (with-json-object
    (loop for (key value) on plist by #'cddr do
          (json-output-member key value))))
