(in-package :json-streams-tests)


(defun run-all-tests ()
  (quickcheck
    (random-test)
    (run-test-files)))

(define (a-json-array)
  (coerce (generate (a-list a-json-value)) 'vector))

(define (a-json-object)
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) in (generate (a-list (a-tuple #'a-string a-json-value)))
          do (setf (gethash key object) value))
    object))

(define (a-double-float)
  (coerce (generate a-real) 'double-float))

(define (a-number)
  (generate (a-member an-integer a-double-float)))

(define (a-json-text)
  (pick-weighted
    (1 (generate a-json-array))
    (1 (generate a-json-object))))

(define (a-json-value)
  (pick-weighted
    (10 :null)
    (10 (generate a-boolean))
    (10 (generate a-number))
    (10 (generate #'a-string))
    (1 (generate (a-member a-json-array a-json-object)))))


(generate a-json-object)


(defun json-equal (json1 json2)
  (cond ((and (vectorp json1) (vectorp json2))
         (and (= (length json1) (length json2))
              (every #'json-equal json1 json2)))
        ((and (hash-table-p json1) (hash-table-p json2))
         (and (= (hash-table-count json1) (hash-table-count json2))
              (loop for key being each hash-key in json1 using (hash-value value)
                    always (json-equal value (gethash key json2 '#:default)))))
        (t
         (equal json1 json2))))


(defun random-test ()
  (for-all ((json a-json-text))
    (is json-equal json (json-parse (princ (json-stringify json))))
    (terpri) (terpri)))

(defun run-test-files (&optional (dir (asdf:system-relative-pathname :json-streams "tests/")))
  (dolist (file (directory (merge-pathnames "*/*.json" dir)))
    (run-test-file file)))


(defun read-all-tokens (in)
  (let ((jstream (make-json-input-stream in)))
    (loop for token = (handler-case (json-read jstream)
                        (json-parse-error () :error))
          collect token
          until (member token '(:eof :error)))))

(defmacro with-open-file* ((stream pathname &rest options) &body body)
  (let ((binary (gensym "BINARY-STREAM")))
    `(with-open-file (,binary ,pathname :element-type 'flex:octet ,@options)
       (with-open-stream (,stream (flex:make-flexi-stream ,binary :external-format '(:utf-8 :eol-style :lf)))
         ,@body))))


(defun run-test-file (file)
  (let ((expected-tokens
         (with-open-file* (in (make-pathname :type "lisp" :defaults file))
           (read in)
           (read in))))
    (with-open-file* (in file)
      (loop for token-num from 0
            for expected in expected-tokens
            for got in (read-all-tokens in)
            do (named (format nil "~A ~D" (pathname-name file) token-num)
                 (is= expected got))))))
