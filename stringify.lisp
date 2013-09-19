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
  (if target
      (with-open-json-stream (jstream (apply #'make-json-output-stream target
                                             options))
        (json-stringify-single value jstream))
      (with-output-to-string (out)
        (apply #'json-stringify value out options))))


(defun json-stringify-multiple (values &optional target &rest options)
  (if target
      (with-open-json-stream (jstream (apply #'make-json-output-stream target
                                             :multiple t
                                             options))
        (dolist (value values)
          (json-stringify-single value jstream)))
      (with-output-to-string (out)
        (apply #'json-stringify-multiple values out options))))


(defun json-stringify-single (value jstream)
  (etypecase value
    ((or string real)
     (json-write value jstream))
    ((member t)
     (json-write :true jstream))
    ((member nil)
     (json-write :false jstream))
    ((member :null)
     (json-write :null jstream))
    (json-array
     (json-write :begin-array jstream)
     (loop for item in (cdr value) do (json-stringify-single item jstream))
     (json-write :end-array jstream))
    (json-object
     (json-write :begin-object jstream)
     (loop for (k . v) in (cdr value) do
           (json-stringify-single k jstream)
           (json-stringify-single v jstream))
     (json-write :end-object jstream))))
