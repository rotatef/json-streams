(in-package #:json-streams)


(defun json-stringify (value &optional target &rest options)
  (if target
      (with-open-json-stream (jstream (apply #'make-json-output-stream target options))
        (json-stringify-single value jstream))
      (with-output-to-string (out)
        (apply #'json-stringify value out options))))


(defun json-stringify-multiple (values &optional target &rest options)
  (if target
      (with-open-json-stream (jstream (apply #'make-json-output-stream target :multiple t options))
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
    (vector
     (json-write :begin-array jstream)
     (loop for item across value do (json-stringify-single item jstream))
     (json-write :end-array jstream))
    (hash-table
     (json-write :begin-object jstream)
     (maphash (lambda (k v)
                (json-stringify-single k jstream)
                (json-stringify-single v jstream))
              value)
     (json-write :end-object jstream))))
