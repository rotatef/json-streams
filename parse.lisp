(in-package #:json-streams)


(defun json-parse (source &rest options)
  (with-open-json-stream (jstream (apply #'make-json-input-stream source options))
    (values (parse-single jstream)
            (slot-value jstream 'position))))


(defun json-parse-multiple (source &rest options)
  (with-open-json-stream (jstream (apply #'make-json-input-stream source :multiple t options))
    (loop for value = (parse-single jstream)
          until (eql :eof value)
          collect value)))


(defun parse-single (jstream)
  (labels ((parse-array (jstream)
             (coerce (loop for value = (parse-value jstream)
                           until (eql :end-array value)
                           collect value)
                     'vector))
           (parse-object (jstream)
             (let ((object (make-hash-table :test #'equal)))
               (loop for key = (parse-value jstream)
                     until (eql :end-object key)
                     do (setf (gethash key object) (parse-value jstream)))
               object))
           (parse-value (jstream)
             (let ((token (json-read jstream)))
               (case token
                 (:true t)
                 (:false nil)
                 (:begin-array (parse-array jstream))
                 (:begin-object (parse-object jstream))
                 (otherwise token)))))
    (values (parse-value jstream)
            (slot-value jstream 'position))))
