(in-package #:json-streams)


(when (= char-code-limit #xFFFF)
  (push 'utf-16-strings *features*))


(defvar *json-stream*)


(defclass json-stream ()
  ((stream :initarg :stream)
   (state-stack :initform '(:before-json-text))))

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
   (message :initarg :message :reader json-error-message))
  (:report (lambda (c stream)
             (format stream "~A at ~S"
                     (json-error-message c)
                     (when (typep (json-error-stream c) 'json-input-stream)
                       (slot-value (json-error-stream c)
                                   'position))))))


(defun json-error (message &rest args)
  (error 'json-error :message (apply #'format nil message args)))


(defmacro ecase* (keyform &body cases)
  (let ((key (gensym "KEY")))
    `(let ((,key ,keyform))
       (case ,key
         ,@cases
         (otherwise (json-error "Expected one of ~S, got ~S in state ~S"
                                ',(mapcar #'car cases)
                                ,key
                                (slot-value *json-stream* 'state-stack)))))))
