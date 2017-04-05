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


(defun make-json-output-stream (stream &key close-stream multiple indent escape-non-ascii (duplicate-key-check t))
  (make-instance 'json-output-stream
                 :stream stream
                 :close-stream close-stream
                 :duplicate-key-check duplicate-key-check
                 :multiple multiple
                 :indent indent
                 :escape-non-ascii escape-non-ascii))


(defun write-indent ()
  (with-slots (stream indent level) *json-stream*
    (when indent
      (terpri stream)
      (dotimes (i level)
        (princ #\Tab stream)))))


(defun write-number (number)
  (with-slots (stream level) *json-stream*
    (unless (or (integerp number)
                (floatp number))
      (%json-error "Number must be integer or float, got ~D" number))
    (when (and (integerp number)
               (not (<= +most-negative-json-integer+ number +most-positive-json-integer+)))
      (%json-error "Integer ~D too large" number))
    (multiple-value-bind (quotient remainder)
        (floor number)
      (if (zerop remainder)
          (princ quotient stream)
          (write-float number stream)))))


(defun write-unicode (stream code-point)
  (flet ((write-escape (value)
           (format stream "\\u~4,'0X" value)))
    (cond ((<= code-point #xFFFF)
           (write-escape code-point))
          (t
           (write-escape (+ (ldb (byte 10 10) (- code-point #x10000)) #xD800))
           (write-escape (+ (ldb (byte 10 0) (- code-point #x10000)) #xDC00))))))


(defun write-escaped-string (string)
  (with-slots (stream level escape-non-ascii) *json-stream*
    (princ #\" stream)
    (map nil (lambda (char)
               (when (and (integerp char) (< char 128))
                 (setf char (code-char char)))
               (case char
                 (#\" (princ "\\\"" stream))
                 (#\\ (princ "\\\\" stream))
                 (#\Backspace (princ "\\b" stream))
                 (#\Page (princ "\\f" stream))
                 (#\Newline (princ "\\n" stream))
                 (#\Return (princ "\\r" stream))
                 (#\Tab (princ "\\t" stream))
                 (otherwise
                  (if (integerp char)
                      (format stream "\\u~4,'0X" char)
                      (if (or (char< char #\Space)
                              (and escape-non-ascii
                                   (> (char-code char) 127)))
                          (write-unicode stream (char-code char))
                          (princ char stream))))))
         (if (stringp string)
             string
             (cdr string)))
    (princ #\" stream)))


(defun json-write (token *json-stream*)
  (with-slots (state-stack multiple stream indent level) *json-stream*
    (let ((token-type (etypecase token
                        (keyword token)
                        (null nil)
                        (real :number)
                        (json-string :string))))
      (labels
          ((write-begin-object ()
             (begin-object)
             (push-state :beginning-of-object)
             (princ #\{ stream)
             (incf level))
           (write-object-key (first)
             (check-key token)
             (unless first
               (princ #\, stream))
             (write-indent)
             (write-escaped-string token)
             (princ #\: stream)
             (when indent
               (princ #\Space stream))
             (switch-state :after-object-key))
           (write-end-object ()
             (end-object)
             (decf level)
             (write-indent)
             (princ #\} stream)
             (pop-state))
           (write-begin-array ()
             (push-state :beginning-of-array)
             (princ #\[ stream)
             (incf level))
           (write-array-item (first)
             (unless first
               (princ #\, stream))
             (write-indent)
             (switch-state :after-array-item)
             (write-value))
           (write-end-array ()
             (decf level)
             (write-indent)
             (princ #\] stream)
             (pop-state))
           (write-value ()
             (push-state :value)
             (reprocess))
           (reprocess ()
             (ecase (car state-stack)
               (:before-json-text
                (unless multiple
                  (switch-state :after-json-text))
                (ecase* token-type
                  (:begin-object (write-begin-object))
                  (:begin-array (write-begin-array))
                  ((:true :false :null :string :number) (write-value))
                  (:eof
                   (unless multiple
                     (%json-error "Empty JSON text"))
                   (switch-state :eof))))
               (:after-json-text
                (ecase* token-type
                  (:eof (switch-state :eof))))
               (:beginning-of-object
                (ecase* token-type
                  (:end-object (write-end-object))
                  (:string (write-object-key t))))
               (:after-object-key
                (switch-state :after-object-value)
                (write-value))
               (:after-object-value
                (ecase* token-type
                  (:end-object (write-end-object))
                  (:string (write-object-key nil))))
               (:value
                (pop-state)
                (ecase* token-type
                  (:begin-object (write-begin-object))
                  (:begin-array (write-begin-array))
                  (:false (princ "false" stream))
                  (:null (princ "null" stream))
                  (:true (princ "true" stream))
                  (:string (write-escaped-string token))
                  (:number (write-number token))))
               (:beginning-of-array
                (case token-type
                  (:end-array (write-end-array))
                  (otherwise (write-array-item t))))
               (:after-array-item
                (case token-type
                  (:end-array (write-end-array))
                  (otherwise (write-array-item nil))))
               (:eof
                (ecase* token-type
                  (:eof))))))
        (reprocess))))
  token)


(defmethod %json-close ((*json-stream* json-output-stream))
  (json-write :eof *json-stream*))
