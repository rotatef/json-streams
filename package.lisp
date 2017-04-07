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

(defpackage #:json-streams
  (:use :common-lisp)
  (:export
   #:json-parse
   #:json-parse-multiple

   #:json-stringify
   #:json-stringify-multiple

   #:json-stream
   #:json-input-stream
   #:json-output-stream

   #:json-close
   #:with-open-json-stream

   #:json-error
   #:json-parse-error
   #:json-write-error

   #:make-json-output-stream
   #:json-write

   #:make-json-input-stream
   #:json-stream-position
   #:json-read

   #:json-object
   #:json-array
   #:json-string

   #:most-positive-json-integer
   #:most-negative-json-integer

   #:call-with-json-output
   #:with-json-output
   #:with-json-array
   #:with-json-object
   #:with-json-member
   #:json-output-member
   #:json-output-value
   #:json-output-boolean
   #:json-output-null
   #:json-output-alist
   #:json-output-plist))
