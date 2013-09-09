(in-package #:json-streams)

(defun parse-array (jstream)
  (coerce (loop for value = (parse-json* jstream)
                until (eql :end-array value)
                collect value)
          'vector))

(defun parse-object (jstream)
  (let ((object (make-hash-table :test #'equal)))
    (loop for key = (parse-json* jstream)
          until (eql :end-object key)
          do (setf (gethash key object) (parse-json* jstream)))
    object))

(defun parse-json* (jstream)
  (let ((token (json-read jstream)))
    (case token
      (:true t)
      (:false nil)
      (:begin-array (parse-array jstream))
      (:begin-object (parse-object jstream))
      (otherwise token))))

(defun parse-json (source)
  (let ((jstream (make-instance 'json-input-stream :stream source)))
    (parse-json* jstream)))

(defun write-json* (value jstream)
  (etypecase value
    ((or string real)
     (json-write value jstream))
    ((member t)
     (json-write :true jstream))
    ((member nil)
     (json-write :false jstream))
    ((member :null)
     (json-write :null jstream))
    (vector
     (json-write :begin-array jstream)
     (loop for item across value do (write-json* item jstream))
     (json-write :end-array jstream))
    (hash-table
     (json-write :begin-object jstream)
     (maphash (lambda (k v)
                (write-json* k jstream)
                (write-json* v jstream))
              value)
     (json-write :end-object jstream))))

(defun write-json (value target)
  (let ((jstream (make-instance 'json-output-stream :stream target)))
    (write-json* value jstream)))
