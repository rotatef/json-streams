(in-package #:json-streams)


(defvar *json-output-stream*)


(defclass json-output-stream ()
  ((stream :initarg :stream)
   (manyp :initarg :manyp)
   (only-ascii :initform t)
   (indent :initform nil :initarg :indent)
   (level :initform 0)
   (state-stack :initform '(:before-json-text))))


(defun make-json-output-stream (stream &key manyp)
  (make-instance 'json-output-stream
                 :stream stream
                 :manyp manyp))


(defun write-indent ()
  (with-slots (stream indent level) *json-output-stream*
    (when indent
      (terpri stream)
      (dotimes (i level)
        (princ " " stream)))))

(defun write-number (number)
  (with-slots (stream level) *json-output-stream*
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
  (with-slots (stream level only-ascii) *json-output-stream*
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
                     (and only-ascii
                          (> (char-code char) 127)))
                 (write-unicode stream (char-code char))
                 (princ char stream)))))
    (princ #\" stream)))

(defun json-write (token *json-output-stream*)
  (with-slots (state-stack stream indent level) *json-output-stream*
    (let ((token-type (etypecase token
                        (keyword token)
                        (null nil)
                        (real :number)
                        (string :string))))
      (labels
          ((push-state (new-state)
             (push new-state state-stack))
           (pop-state ()
             (pop state-stack))
           (switch-state (new-state)
             (setf (car state-stack) new-state))
           (do-begin-object ()
             (push-state :beginning-of-object)
             (princ "{" stream)
             (incf level))
           (do-object-key (first)
             (unless first
               (princ "," stream))
             (write-indent)
             (write-escaped-string token)
             (princ ":" stream)
             (when indent
               (princ " " stream))
             (switch-state :after-object-key))
           (do-end-object ()
             (decf level)
             (write-indent)
             (princ "}" stream)
             (pop-state))
           (do-begin-array ()
             (push-state :beginning-of-array)
             (princ "[" stream)
             (incf level))
           (do-array-item (first)
             (unless first
               (princ "," stream))
             (write-indent)
             (switch-state :after-array-item)
             (do-value))
           (do-end-array ()
             (decf level)
             (write-indent)
             (princ "]" stream)
             (pop-state))
           (do-value ()
             (push-state :value)
             (process))
           (process ()
             (ecase (car state-stack)
               (:before-json-text
                (switch-state :after-json-text)
                (ecase token-type
                  (:begin-object (do-begin-object))
                  (:begin-array (do-begin-array))))
               (:after-json-text
                (ecase token-type
                  (:begin-object (do-begin-object))
                  (:begin-array (do-begin-array))
                  ((nil) :end-of-file)))
               (:beginning-of-object
                (ecase token-type
                  (:end-object (do-end-object))
                  (:string (do-object-key t))))
               (:after-object-key
                (switch-state :after-object-value)
                (do-value))
               (:after-object-value
                (ecase token-type
                  (:end-object (do-end-object))
                  (:string (do-object-key nil))))
               (:value
                (pop-state)
                (ecase token-type
                  (:begin-object (do-begin-object))
                  (:begin-array (do-begin-array))
                  (:false (princ "false" stream))
                  (:null (princ "null" stream))
                  (:true (princ "true" stream))
                  (:string (write-escaped-string token))
                  (:number (write-number token))))
               (:beginning-of-array
                (case token-type
                  (:end-array (do-end-array))
                  (otherwise (do-array-item t))))
               (:after-array-item
                (case token-type
                  (:end-array (do-end-array))
                  (otherwise (do-array-item nil)))))))
        (process))))
  token)
