(in-package :json-streams-tests)


(defconstant +utf-8+
  #-CLISP :utf-8
  #+CLISP charset:utf-8)


(defun run-all-tests ()
;  (quickcheck (test-parser))
 (quickcheck (run-test-files)))


(defun test-parser ()
  (with-open-json-stream (in (make-json-input-stream "[{},1,2,true]"))
    (loop for expected in '(:begin-array :eof)
          for got = (json-read in)
          do (is= expected got)
          until (eql :eof got))))


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
