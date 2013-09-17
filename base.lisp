(in-package #:json-streams)


(when (= char-code-limit #x10000)
  (push 'utf-16-strings *features*))


(defvar *json-stream*)


(defclass json-stream ()
  ((stream :initarg :stream)
   (close-stream :initarg :close-stream)
   (state-stack :initform '(:before-json-text))
   (duplicate-key-check :initarg :duplicate-key-check)
   (key-check-stack :initform nil)
   (position :initform 0 :initarg :position :reader json-stream-position)
   (newlines :initform nil)))


(defgeneric %json-close (json-stream))


(defun json-close (json-stream)
  (with-slots (stream close-stream)
      json-stream
    (%json-close json-stream)
    (when close-stream
      (close stream))))


(defmacro with-open-json-stream ((var stream) &body body)
  `(let ((,var ,stream))
     (unwind-protect
          (progn ,@body)
       (json-close ,var))))

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

(defun find-line-num (json-stream)
  (with-slots (newlines position)
      json-stream
    (loop for line from 1
          for newline in newlines
          )))

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
