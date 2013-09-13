(in-package #:json-streams)


(defclass json-output-stream (json-stream)
  ((manyp :initarg :manyp)
   (escape-non-ascii :initarg :escape-non-ascii)
   (indent :initarg :indent)
   (level :initform 0)))


(defun make-json-output-stream (stream &key manyp indent escape-non-ascii)
  (make-instance 'json-output-stream
                 :stream stream
                 :manyp manyp
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
    (multiple-value-bind (quotient remainder)
        (floor number)
      (if (and (zerop remainder)
               (< (abs number) (expt 2 53)))
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
    (loop for char across string do
          (case char
            (#\" (princ "\\\"" stream))
            (#\\ (princ "\\\\" stream))
            (#\Backspace (princ "\\b" stream))
            (#\Page (princ "\\f" stream))
            (#\Newline (princ "\\n" stream))
            (#\Return (princ "\\r" stream))
            (#\Tab (princ "\\t" stream))
            (otherwise
             (if (or (char< char #\Space)
                     (and escape-non-ascii
                          (> (char-code char) 127)))
                 (write-unicode stream (char-code char))
                 (princ char stream)))))
    (princ #\" stream)))


(defun json-write (token *json-stream*)
  (with-slots (state-stack stream indent level) *json-stream*
    (let ((token-type (etypecase token
                        (keyword token)
                        (null nil)
                        (real :number)
                        (string :string))))
      (labels
          ((write-begin-object ()
             (push-state :beginning-of-object)
             (princ #\{ stream)
             (incf level))
           (write-object-key (first)
             (unless first
               (princ #\, stream))
             (write-indent)
             (write-escaped-string token)
             (princ #\: stream)
             (when indent
               (princ #\Space stream))
             (switch-state :after-object-key))
           (write-end-object ()
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
                (switch-state :after-json-text)
                (ecase* token-type
                  (:begin-object (write-begin-object))
                  (:begin-array (write-begin-array))))
               (:after-json-text
                (ecase* token-type
                  (:begin-object (write-begin-object))
                  (:begin-array (write-begin-array))
                  (:eof (switch-state :closed))))
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
               (:closed
                (json-error "Writing to closed json-stream: ~S" token)))))
        (reprocess))))
  token)
