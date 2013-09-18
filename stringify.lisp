(in-package #:json-streams)


(defun json-stringify (value &optional target &rest options)
  (if target
      (with-open-json-stream (jstream (apply #'make-json-output-stream target
                                             :duplicate-key-check nil
                                             options))
        (json-stringify-single value jstream))
      (with-output-to-string (out)
        (apply #'json-stringify value out options))))


(defun json-stringify-multiple (values &optional target &rest options)
  (if target
      (with-open-json-stream (jstream (apply #'make-json-output-stream target
                                             :duplicate-key-check nil
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
