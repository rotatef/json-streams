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


(deftype json-object ()
  '(cons (member :object) t))

(deftype json-array ()
  '(cons (member :array) t))

(deftype json-string ()
  '(or string
      (cons (member :string) t)))


(when (= char-code-limit #x10000)
  (push 'utf-16-strings *features*))


(defconstant +most-positive-json-integer+
  (expt 2 53))


(defconstant +most-negative-json-integer+
  (- (expt 2 53)))


(defvar *json-stream*)


(defclass json-stream ()
  ((stream :initarg :stream)
   (close-stream :initarg :close-stream)
   (state-stack :initform '(:before-json-text))
   (duplicate-key-check :initarg :duplicate-key-check)
   (key-check-stack :initform nil)))


(defclass json-input-stream (json-stream)
  ((multiple :initarg :multiple)
   (use-ratios :initarg :use-ratios)
   (max-exponent :initarg :max-exponent)
   (raw-strings :initarg :raw-strings)
   (current-char :initform nil)
   (position :initarg :position :reader json-stream-position)
   (newlines :initform '())
   (string-mode :initform nil)))


(defclass json-output-stream (json-stream)
  ((multiple :initarg :multiple)
   (escape-non-ascii :initarg :escape-non-ascii)
   (indent :initarg :indent)
   (level :initform 0)))


(defgeneric %json-close (json-stream))


(defun json-close (json-stream &key abort)
  (with-slots (stream close-stream)
      json-stream
    (unless abort
      (%json-close json-stream))
    (when close-stream
      (close stream :abort abort))))


(defmacro with-open-json-stream ((var stream) &body body)
  (let ((errorsp (gensym "ERRORSP")))
    `(let ((,var ,stream)
           (,errorsp t))
       (unwind-protect
            (multiple-value-prog1 ,@body
              (setf ,errorsp nil))
         (json-close ,var :abort ,errorsp)))))

(defun begin-object ()
  (with-slots (duplicate-key-check key-check-stack)
      *json-stream*
    (when duplicate-key-check
      (push (make-hash-table :test #'equal) key-check-stack))))

(defun end-object ()
  (with-slots (duplicate-key-check key-check-stack)
      *json-stream*
    (when duplicate-key-check
      (pop key-check-stack))))

(defun check-key (key)
  (with-slots (duplicate-key-check key-check-stack)
      *json-stream*
    (when duplicate-key-check
      (when (gethash key (car key-check-stack))
        (%json-error "Duplicate key ~S in object" key))
      (setf (gethash key (car key-check-stack)) t))))

(defmacro state-stack ()
  `(slot-value *json-stream* 'state-stack))

(defun push-state (new-state)
  (push new-state (state-stack)))

(defun pop-state ()
  (pop (state-stack)))

(defun switch-state (new-state)
  (pop-state)
  (push-state new-state))

(define-condition json-error (error)
  ((stream :initform *json-stream* :initarg :stream :reader json-error-stream)
   (message :initarg :message :reader json-error-message)))


(define-condition json-parse-error (json-error)
  ()
  (:report (lambda (c stream)
             (format stream "JSON parse error at line ~D column ~D: ~A"
                     (1+ (length (slot-value (json-error-stream c) 'newlines)))
                     (- (slot-value (json-error-stream c) 'position)
                        (or (car (slot-value (json-error-stream c) 'newlines)) 0))
                     (json-error-message c)))))

(define-condition json-write-error (json-error)
  ()
  (:report (lambda (c stream)
             (format stream "JSON write error: ~A." (json-error-message c)))))


(defun %json-error (message &rest args)
  (switch-state :eof)
  (error (etypecase *json-stream*
           (json-input-stream 'json-parse-error)
           (json-output-stream 'json-write-error))
         :message (apply #'format nil message args)))


(defmacro ecase* (keyform &body cases)
  (let ((key (gensym "KEY")))
    `(let ((,key ,keyform))
       (case ,key
         ,@cases
         (otherwise (%json-error "Expected one of ~S, got ~S in state ~S."
                                ',(mapcar #'car cases)
                                ,key
                                (slot-value *json-stream* 'state-stack)))))))
