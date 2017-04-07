;;;;  json-streams
;;;;
;;;;  Copyright (C) 2013 Thomas Bakketun <thomas.bakketun@copyleft.no>
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


(defun json-stringify (value &optional target &rest options)
  (call-with-json-output target options
                         (lambda ()
                           (json-stringify-single value))))


(defun json-stringify-multiple (value &optional target &rest options)
  (call-with-json-output target (list* :multiple t options)
                         (lambda ()
                           (map nil #'json-stringify-single value))))


(defun json-stringify-single (value)
  (etypecase value
    ((or string real)
     (json-output-value value))
    ((member t nil)
     (json-output-boolean value))
    ((member :null)
     (json-output-null))
    (json-array
     (with-json-array
       (map nil #'json-stringify-single (cdr value))))
    (json-object
     (with-json-object
       (loop for (key . value) in (cdr value) do
         (with-json-member key
           (json-stringify-single value)))))))
