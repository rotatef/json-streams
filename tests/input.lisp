;;;;  json-streams
;;;;
;;;;  Copyright (C) 2013 Thomas Bakketun <thomas.bakketun@copyleft.no>
;;;;
;;;;  This library is free software: you can redistribute it and/or modify
;;;;  it under the terms of the GNU Lesser General Public License as published
;;;;  by the Free Software Foundation, either version 3 of the License, or
;;;;  (at your option) any later version.
;;;;
;;;;  This library is distributed in the hope that it will be useful,
;;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;  GNU General Public License for more details.
;;;;
;;;;  You should have received a copy of the GNU General Public License
;;;;  along with this library.  If not, see <http://www.gnu.org/licenses/>.

(in-package :json-streams-tests)


(defun run-all-tests ()
  (quickcheck
    (random-test)
    (run-test-files)
    (json-test-suite-test-parsing)))

(define (a-json-array)
  (cons :array (generate (a-list a-json-value))))

(define (a-json-object)
  (cons :object
        (remove-duplicates
         (loop for (key value) in (generate (a-list (a-tuple #'a-string a-json-value)))
               collect (cons key value))
         :key #'car
         :test #'string=)))

(define (a-double-float)
  (coerce (generate a-real) 'double-float))

(define (a-number)
  (generate (a-member an-integer a-double-float)))

(define (a-json-text)
  (pick-weighted
    (5 (generate a-json-array))
    (5 (generate a-json-object))
    (1 (generate a-json-value))))

(define (a-json-value)
  (pick-weighted
    (10 :null)
    (10 (generate a-boolean))
    (10 (generate a-number))
    (10 (generate #'a-string))
    (1 (generate (a-member a-json-array a-json-object)))))

(defun random-test ()
  (for-all ((json a-json-text))
    (is= json (json-parse (princ (json-stringify json))))
    (terpri) (terpri)))


(defun run-test-files (&optional (dir (asdf:system-relative-pathname :json-streams "tests/")))
  (dolist (file (directory (merge-pathnames "*/*.json" dir)))
    (run-test-file file)))


(defun read-all-tokens (in options)
  (with-open-json-stream (jstream (apply #'make-json-input-stream in options))
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
  (with-open-file* (in (make-pathname :type "lisp" :defaults file))
    (loop for options = (read in nil)
          for expected-tokens = (read in nil)
          while expected-tokens
          do
          (with-open-file* (in file)
            (loop for token-num from 0
                  for expected in expected-tokens
                  for got in (read-all-tokens in options)
                  do (named (format nil "~A ~S ~D" options (pathname-name file) token-num)
                       (is= expected got)))))))



(defun json-test-suite-test-file (pathname)
  (with-open-file* (in pathname)
    (let* ((tokens (read-all-tokens in '(:duplicate-key-check nil)))
           (errorsp (not (not (find :error tokens)))))
      (values (ecase (char (pathname-name pathname) 0)
                (#\y (not errorsp))
                (#\n errorsp)
                (#\i t))
              tokens
              (unless errorsp
                (with-output-to-string (out)
                  (with-open-json-stream (json (make-json-output-stream out :duplicate-key-check nil))
                    (dolist (token tokens)
                      (json-write token json)))))))))

(defparameter *json-test-suite-skip-tests*
  '("i_string_iso_latin_1"
    "i_string_not_in_unicode_range"
    "i_string_overlong_sequence_2_bytes"
    "i_string_overlong_sequence_6_bytes"
    "i_string_overlong_sequence_6_bytes_null"
    "i_string_UTF-16LE_with_BOM"
    "i_string_UTF-8_invalid_sequence"
    "i_string_UTF8_surrogate_U+D800"
    "i_string_invalid_utf-8"
    "i_string_lone_utf8_continuation_byte"
    "i_string_truncated-utf-8"
    "i_string_utf16BE_no_BOM"
    "i_string_utf16LE_no_BOM"
    "n_array_a_invalid_utf8"
    "n_array_invalid_utf8"
    "n_number_invalid-utf-8-in-bigger-int"
    "n_number_invalid-utf-8-in-exponent"
    "n_number_invalid-utf-8-in-int"
    "n_number_real_with_invalid_utf8_after_e"
    "n_object_pi_in_key_and_trailing_comma"
    "n_string_invalid-utf-8-in-escape"
    "n_string_invalid_utf8_after_escape"
    "n_structure_incomplete_UTF8_BOM"
    "n_structure_lone-invalid-utf-8"
    "n_structure_single_eacute"))

(defun json-test-suite-test-parsing (&optional (dir (asdf:system-relative-pathname :json-streams "tests/JSONTestSuite/test_parsing/")))
  (loop for file in (directory (merge-pathnames "*.json" dir))
        for name = (pathname-name file)
        when (and (not (find name *json-test-suite-skip-tests* :test #'string=))
                  (with-simple-restart (contiune "Test ~A" name)
                    (not (json-test-suite-test-file file))))
          collect file))
