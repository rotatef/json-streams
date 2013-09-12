(in-package #:json-streams)


(defvar *json-input-stream*)


(defclass json-input-stream (json-stream)
  ((manyp :initarg :manyp)
   (max-exponent :initarg :max-exponent)
   (current-char :initform nil)
   (position :initform 0)
   (newlines :initform '())
   (string-mode :initform nil)))


(defun make-json-input-stream (stream &key manyp (max-exponent 308))
  (make-instance 'json-input-stream
                 :stream stream
                 :manyp manyp
                 :max-exponent max-exponent))


(defun current-position ()
  (slot-value *json-stream* 'position))


(defun peek-next-char ()
  (with-slots (stream) *json-stream*
    (peek-char nil stream nil)))


(defun current-char ()
  (with-slots (current-char) *json-stream*
    (unless current-char
      (read-next-char))
    current-char))


(defun read-next-char ()
  (with-slots (stream current-char position newlines) *json-stream*
    (setf current-char (read-char stream nil))
    (when current-char
      (incf position)
      (when (eql #\Newline current-char)
	(pushnew position newlines)))
    current-char))


(defun unread-current-char ()
  (with-slots (stream current-char position) *json-stream*
    (when current-char
      (decf position)
      (unread-char current-char stream))))


(defun skip-space ()
  (loop while (member (read-next-char) '(#\Space #\Tab #\Newline #\Return)))
  (unread-current-char))


(defun expect-string (start string token)
  (loop for expect across string
        for char = (read-next-char)
        unless (eql char expect)
        do (json-error "Expected ~S (as part of ~S), got ~S" expect string char))
  (values token start (current-position)))


(defun valid-unescaped-char-p (char)
  (let ((code (char-code char)))
    (or (<= #x20 code #x21)
        (<= #x23 code #x5b)
        (<= #x5d code #x10ffff))))

(defun read-string-chars ()
  (coerce (loop for char = (read-next-char)
                while (valid-unescaped-char-p char)
                collect char
                finally (unread-current-char))
          'string))


(defun read-4hexdig ()
  (loop repeat 4
        for weight = #x1000 then (/ weight 16)
        for char = (read-next-char)
        for digit = (and char (digit-char-p char 16))
        do (unless digit
             (json-error "Invalid char ~S in Unicode escape" char))
        sum (* digit weight)))


(defun read-escaped (start)
  (read-next-char)
  (let ((escaped (read-next-char)))
    (if (eql #\u escaped)
        (values (read-4hexdig) start (current-position))
        (values escaped start (current-position)))))


(defun read-integer (&key fractional-p)
  (loop for number = 0 then (+ (* number 10) digit)
	for size from 0
	for char = (read-next-char)
	for digit = (and char (digit-char-p char))
	while digit
	finally (progn (unread-current-char)
		       (unless (plusp size)
			 (json-error "Expected digit, got ~S" char))
		       (return (if fractional-p
				   (/ number (expt 10 size))
				   number)))))


(defun read-number (start)
  (let* ((sign 1)
	 (integer-part 0)
	 (fraction-part 0)
	 (exponent-sign 1)
	 (exponent 0))
    (when (eql #\- (peek-next-char))
      (read-next-char)
      (setf sign -1))
    (if (eql #\0 (peek-next-char))
        (read-next-char)
        (setf integer-part (read-integer)))
    (when (eql #\. (peek-next-char))
      (read-next-char)
      (setf fraction-part (read-integer :fractional-p t)))
    (when (member (peek-next-char) '(#\e #\E))
      (read-next-char)
      (case (read-next-char)
        (#\+)
        (#\-
         (setf exponent-sign -1))
        (otherwise
         (unread-current-char)))
      (setf exponent (read-integer)))
    (with-slots (max-exponent) *json-stream*
      (when (> exponent max-exponent)
        (json-error "Exponent ~A is too large (or small) (max-exponent is ~A)" exponent max-exponent)))
    (values (* sign
               (+ integer-part fraction-part)
               (expt 10 (* exponent-sign exponent)))
            start (current-position))))


(defun read-raw-token (&optional (*json-stream* *json-stream*))
  (with-slots (string-mode) *json-stream*
    (let ((start-pos (current-position)))
      (cond
        (string-mode
         (case (peek-next-char)
           ((nil) (values :eof start-pos (current-position)))
           (#\" (read-next-char)
                (setf string-mode nil)
                (values :string-delimiter start-pos (current-position)))
           (#\\ (read-escaped start-pos))
           (otherwise
            (unless (valid-unescaped-char-p (peek-next-char))
              (json-error "Invalid char in string: ~S " (read-next-char)))
            (values (read-string-chars) start-pos (current-position)))))
        (t
         (skip-space)
         (case (peek-next-char)
           ((nil) (values :eof start-pos (current-position)))
           (#\{ (read-next-char) (values :begin-object start-pos (current-position)))
           (#\} (read-next-char) (values :end-object start-pos (current-position)))
           (#\[ (read-next-char) (values :begin-array start-pos (current-position)))
           (#\] (read-next-char) (values :end-array start-pos (current-position)))
           (#\: (read-next-char) (values :name-separator start-pos (current-position)))
           (#\, (read-next-char) (values :value-separator start-pos (current-position)))
           (#\f (expect-string start-pos "false" :false))
           (#\n (expect-string start-pos "null" :null))
           (#\t (expect-string start-pos "true" :true))
           ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
            (read-number start-pos))
           (#\" (read-next-char)
                (setf string-mode t)
                (values :string-delimiter start-pos (current-position)))
           (otherwise (json-error "Unexpected character ~S" (read-next-char)))))))))


(defun parse-string (start)
  (let (token start2 end)
    (values
     (with-output-to-string (string)
       (loop
        (multiple-value-setq (token start2 end)
          (read-raw-token))
        (cond
          ((eql :string-delimiter token)
           (return))
          ((stringp token)
           (princ token string))
          ((characterp token)
           (case token
             ((#\" #\\ #\/) (princ token string))
             (#\b (princ #\Backspace string))
             (#\f (princ #\Page string))
             (#\n (princ #\Newline string))
             (#\r (princ #\Return string))
             (#\t (princ #\Tab string))
             (otherwise (json-error "Invalid escape \\~A" token))))
          ((integerp token)
           (cond
             ((<= #xDC00 token #xDFFF)
              (json-error "Invalid unicode escape ~4,'0X" token))
             ((<= #xD800 token #xDBFF)
              (let ((lead token)
                    (tail (read-raw-token)))
                (unless (and (integerp tail) (<= #xDC00 tail #xDFFF))
                  (json-error "Invalid surrogate pair ~4,'0X ~4,'0X" lead tail))
                (princ (code-char (+ #x10000
                                     (ash (- lead #xD800) 10)
                                     (- tail #xDC00)))
                       string)))
             (t
              (princ (code-char token) string))))
          (t
           (json-error "Invalid token ~S in string." token)))))
     start end)))

(defmacro def-state-machine (&body state-cases)
  `(let ((token-type (etypecase token
                       (keyword token)
                       (null nil)
                       (real :number)
                       (string :string))))
     (labels ((reprocess ()
                (ecase (car (state-stack))
                  ,@state-cases)))
      (reprocess))))

(defmacro tokentypecase (&body cases)
  `(case token-type
     ,@cases
     ,@(unless (assoc 'otherwise cases)
               `((otherwise (json-error "Expected one of ~S in state ~S"
                                              ',(mapcar #'car cases)
                                              (car (state-stack))))))))

(defun read-token (&optional start (*json-stream* *json-stream*))
  (with-slots (manyp stream) *json-stream*
    (multiple-value-bind (token start2 end)
        (read-raw-token)
      (unless start
        (setf start start2))
      (def-state-machine
          (:before-json-text
           (unless manyp
             (switch-state :after-json-text))
           (tokentypecase
             (:begin-object
              (push-state :begin-object)
              (values token start end))
             (:begin-array
              (push-state :before-first-array-item)
              (values token start end))))

          (:begin-object
           (tokentypecase
             (:end-object
              (pop-state)
              (values token start end))
             (:string-delimiter
              (switch-state :after-object-key)
              (parse-string start))))

          (:after-object-key
           (tokentypecase
             (:name-separator
              (switch-state :after-object-value)
              (push-state :value)
              (read-token start))))

          (:after-object-value
           (tokentypecase
             (:value-separator
              (switch-state :before-object-key)
              (read-token start))
             (:end-object
              (pop-state)
              (values token start end))))

          (:before-object-key
           (tokentypecase
             (:string-delimiter
              (switch-state :after-object-key)
              (parse-string start))))

          (:before-first-array-item
           (tokentypecase
             (:end-array
              (pop-state)
              (values token start end))
             (otherwise
              (switch-state :after-array-item)
              (push-state :value)
              (reprocess))))

          (:after-array-item
           (tokentypecase
             (:value-separator
              (push-state :value)
              (read-token start))
             (:end-array
              (pop-state)
              (values token start end))))

          (:value
           (pop-state)
           (tokentypecase
             (:begin-object
              (push-state :begin-object)
              (values token start end))
             (:begin-array
              (push-state :before-first-array-item)
              (values token start end))
             (:string-delimiter
              (parse-string start))
             ((:false :null :true)
              (values token start end))
             (:number
              (values token start end))))

          (:after-json-text
           (tokentypecase
             (:eof
              (values :eof start end))))))))


(defun json-read (json-input-stream)
  (read-token nil json-input-stream))
