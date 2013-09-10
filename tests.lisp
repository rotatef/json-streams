(in-package #:json-streams)

(defun run-tests (&optional (dir (asdf:system-relative-pathname :json-streams "tests/")))
  (dolist (file (directory (merge-pathnames "*/*.json" dir)))
    (run-test file)))

(defun read-raw-tokens (in)
  (let ((jstream (make-json-input-stream in)))
    (loop repeat 1000
          for token = (handler-case (multiple-value-list (read-raw-token jstream))
                        (json-parse-error () (list :error)))
          collect (car token)
          until (member (car token) '(:eof :error)))))

(defun read-tokens (in)
  (let ((jstream (make-json-input-stream in)))
    (loop repeat 1000
          for token = (handler-case (multiple-value-list (json-read jstream))
                        (json-parse-error () (list :error)))
          collect (car token)
          until (member (car token) '(:eof :error)))))

(defun expect-equal (a b)
  (unless (equal a b)
    (let ((pos (mismatch a b :test #'equal)))
      (cerror "Ignore" "Mismatch at ~S, expected ~&~S got~&~S"
              pos (elt a pos) (elt b pos)))))

(defun run-test (file)
  (let* ((name (pathname-name file))
         (expect-fail (eql 0 (search "fail" name)))
         (expected (with-open-file (in (make-pathname :type "lisp" :defaults file))
                     (list (read in)
                           (read in)))))
    (format t "~&~A ~A" name expect-fail)
    (with-simple-restart (abort "Abort this test")
      (with-open-file (in file)
        (expect-equal (first expected) (read-raw-tokens in)))
      (with-open-file (in file)
        (expect-equal (second expected) (read-tokens in))))))
