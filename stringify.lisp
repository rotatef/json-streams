(in-package #:json-streams)


(defun json-stringify (value &optional target &rest options)
  (labels ((output-json (value jstream)
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
                (loop for item across value do (output-json item jstream))
                (json-write :end-array jstream))
               (hash-table
                (json-write :begin-object jstream)
                (maphash (lambda (k v)
                           (output-json k jstream)
                           (output-json v jstream))
                         value)
                (json-write :end-object jstream)))))
    (cond
      (target
       (output-json value (apply #'make-json-output-stream target options))
       value)
      (t
       (with-output-to-string (out)
         (output-json value (apply #'make-json-output-stream out options)))))))
