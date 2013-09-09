(in-package #:json-streams)

(defvar *json-input-stream*)


(defclass json-input-stream ()
  ((stream :initarg :stream)
   (manyp :initarg :manyp)
   (max-exponent :initarg :max-exponent)
   (current-char :initform nil)
   (position :initform 0)
   (newlines :initform '())
   (string-mode :initform nil)
   (state-stack :initform '(:before-json-text))))


(defun make-json-input-stream (stream &key manyp (max-exponent 308))
  (make-instance 'json-input-stream
                 :stream stream
                 :manyp manyp
                 :max-exponent max-exponent))


(defun current-position ()
  (slot-value *json-input-stream* 'position))


(defun peek-next-char ()
  (with-slots (stream) *json-input-stream*
    (peek-char nil stream nil)))


(defun current-char ()
  (with-slots (current-char) *json-input-stream*
    (unless current-char
      (read-next-char))
    current-char))


(defun read-next-char ()
  (with-slots (stream current-char position newlines) *json-input-stream*
    (setf current-char (read-char stream nil))
    (when current-char
      (incf position)
      (when (eql #\Newline current-char)
	(pushnew position newlines)))
    current-char))


(defun unread-current-char ()
  (with-slots (stream current-char position) *json-input-stream*
    (when current-char
      (decf position)
      (unread-char current-char stream))))


(defun skip-space ()
  (loop while (member (read-next-char) '(#\Space #\Tab #\Newline #\Return)))
  (unread-current-char))


(defun expect-string (start string token)
  (loop for expect across string
        unless (eql (read-next-char) expect)
        do (setf token :error))
  (if (eql :error token)
      (values token start (current-position)
              :unexpected-character (current-char))
      (values token start (current-position))))


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
             (return :invalid-unicode-escape))
        sum (* digit weight)))


(defun read-escaped (start)
  (read-next-char)
  (let ((escaped (read-next-char)))
    (if (eql #\u escaped)
        (let ((code-point (read-4hexdig)))
          (values (if (integerp code-point)
                      code-point
                      :error)
                  start (current-position)))
        (values (if (find escaped "\"\\/bfnrt")
                    escaped
                    :error)
                start (current-position)))))


(defun read-integer (&key fractional-p)
  (loop for number = 0 then (+ (* number 10) digit)
	for size from 0
	for char = (read-next-char)
	for digit = (and char (digit-char-p char))
	while digit
	finally (progn (unread-current-char)
		       (unless (plusp size)
			 (throw 'error :expected-digit))
		       (return (if fractional-p
				   (/ number (expt 10 size))
				   number)))))


(defun read-number (start)
  (let* ((sign 1)
	 (integer-part 0)
	 (fraction-part 0)
	 (exponent-sign 1)
	 (exponent 0)
	 (errorp
	  (catch 'error
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
             nil)))
    (cond
      (errorp
       (values :error start (current-position)
               errorp (current-char)))
      ((> exponent (slot-value *json-input-stream* 'max-exponent))
       (values :error start (current-position)
               :exponent-to-large exponent))
      (t (values (* sign
                    (+ integer-part fraction-part)
                    (expt 10 (* exponent-sign exponent)))
                 start (current-position))))))


(defun read-raw-token (&optional (*json-input-stream* *json-input-stream*))
  (with-slots (string-mode) *json-input-stream*
    (let ((start-pos (current-position)))
      (cond
        (string-mode
         (case (peek-next-char)
           ((nil) (values :eof start-pos (current-position)))
           (#\" (read-next-char)
                (setf string-mode nil)
                (values :quote start-pos (current-position)))
           (#\\ (read-escaped start-pos))
           (otherwise
            (if (valid-unescaped-char-p (peek-next-char))
                (values (read-string-chars) start-pos (current-position))
                (values :error start-pos (current-position)
                        :invalid-char-in-string (read-next-char))))))
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
                (values :quote start-pos (current-position)))
           (otherwise (values :error start-pos (current-position)
                              :unexpected-char (read-next-char)))))))))


(defun parse-string (start)
  (let (token start2 end)
    (values
     (with-output-to-string (string)
       (loop
        (multiple-value-setq (token start2 end)
          (read-raw-token))
        (cond
          ((eql :quote token)
           (return))
          ((stringp token)
           (princ token string))
          ((characterp token)
           (ecase token
             ((#\" #\\ #\/) (princ token string))
             (#\b (princ #\Backspace string))
             (#\f (princ #\Page string))
             (#\n (princ #\Newline string))
             (#\r (princ #\Return string))
             (#\t (princ #\Tab string))))
          ((integerp token)
           (cond
             ((<= #xDC00 token #xDFFF)
              (error "Invalid unicode escape ~4,'0X" token))
             ((<= #xD800 token #xDBFF)
              (let ((lead token)
                    (tail (read-raw-token)))
                (assert (and (integerp tail) (<= #xDC00 tail #xDFFF)) ()
                        "Invalid surrogate pair ~4,'0X ~4,'0X" lead tail)
                (princ (code-char (+ #x10000
                                     (ash (- lead #xD800) 10)
                                     (- tail #xDC00)))
                       string)))
             (t
              (princ (code-char token) string))))
          (t
           (error "Invalid token ~S in string." token)))))
     start end)))


(defun read-token (&optional start (*json-input-stream* *json-input-stream*))
  (with-slots (state-stack manyp stream) *json-input-stream*
    (multiple-value-bind (token start2 end)
        (read-raw-token)
      (unless start
        (setf start start2))
      (labels
          ((process-token ()
             (ecase (car state-stack)
               (:before-json-text
                (unless manyp
                  (pop state-stack)
                  (push :after-json-text state-stack))
                (ecase token
                  (:begin-object
                   (push :begin-object state-stack)
                   (values token start end))
                  (:begin-array
                   (push :before-first-array-item state-stack)
                   (values token start end))))

               (:begin-object
                (ecase token
                  (:end-object
                   (pop state-stack)
                   (values token start end))
                  (:quote
                   (setf (car state-stack) :after-object-key)
                   (parse-string start))))

               (:after-object-key
                (ecase token
                  (:name-separator
                   (setf (car state-stack) :after-object-value)
                   (push :value state-stack)
                   (read-token start))))

               (:after-object-value
                (ecase token
                  (:value-separator
                   (setf (car state-stack) :before-object-key)
                   (read-token start))
                  (:end-object
                   (pop state-stack)
                   (values token start end))))

               (:before-object-key
                (ecase token
                  (:quote
                   (setf (car state-stack) :after-object-key)
                   (parse-string start))))

               (:before-first-array-item
                (case token
                  (:end-array
                   (pop state-stack)
                   (values token start end))
                  (otherwise
                   (setf (car state-stack) :after-array-item)
                   (push :value state-stack)
                   (process-token))))

               (:after-array-item
                (ecase token
                  (:value-separator
                   (push :value state-stack)
                   (read-token start))
                  (:end-array
                   (pop state-stack)
                   (values token start end))))

               (:value
                (pop state-stack)
                (case token
                  (:begin-object
                   (push :begin-object state-stack)
                   (values token start end))
                  (:begin-array
                   (push :before-first-array-item state-stack)
                   (values token start end))
                  (:quote
                   (parse-string start))
                  ((:false :null :true)
                   (values token start end))
                  (otherwise
                   (assert (numberp token) () "Expeted number, got ~S" token)
                   (values token start end))))

               (:after-json-text
                (assert (eql :eof token) () "Extra data after JSON-text ~S" token)
                (values :eof start end)))))

        (process-token)))))


(defun json-read (json-input-stream)
  (read-token nil json-input-stream))
