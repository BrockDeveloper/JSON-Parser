;;;; Author: 886155 Andrea Broccoletti
;;;; Author: 886261 Damiano Pellegrini


;;;; json-parsing.l


;;; read a char from a stream, if present.
(defun json-read-char (stream char &key (ignore-ws nil))
  "Read a char from a stream if present."
  (when (char-equal (peek-char ignore-ws stream) char)
    (read-char stream)
  )
)


;;; read the string \"true\" preceded by any whitespaces from a stream.
(defun json-read-true (stream)
  (json-read-char stream #\t :ignore-ws t)
  (json-read-char stream #\r)
  (json-read-char stream #\u)
  (json-read-char stream #\e)
)


;;; Read  the string \"false\" preceded by any whitespaces from a stream.
;;; It returns false but removes the char from the stream.
(defun json-read-false (stream)
  (json-read-char stream #\f :ignore-ws t)
  (json-read-char stream #\a)
  (json-read-char stream #\l)
  (json-read-char stream #\s)
  (json-read-char stream #\e)
  'false
)


;;; Read  the string \"null\" preceded by any whitespaces from a stream.
;;; It returns nil but removes the char from the stream.
(defun json-read-null (stream)
  (json-read-char stream #\n :ignore-ws t)
  (json-read-char stream #\u)
  (json-read-char stream #\l)
  (json-read-char stream #\l)
  nil
)


;;; Read a JSON string from a stream.
;;; out-stream is a class so it's passed by reference
(defun json-read-digits-h (stream out-stream)
  (let ((*c* (peek-char nil stream nil)))
    ; Decl
    (unless *c*
      (return-from json-read-digits-h T))

    ; If not a digit return
    (unless (digit-char-p *c*)
      (return-from json-read-digits-h T)
    )


    ; Char is digit write to out stream
    (write-char (read-char stream) out-stream)

    (json-read-digits-h stream out-stream)
  )
)


;;; Read a sequence of digits from a stream as a string
(defun json-read-digits (stream)
  (with-output-to-string (out)
    (json-read-digits-h stream out)
  )
)


;;; Read a JSON number from a stream
(defun json-read-number (stream)
  (read-from-string (with-output-to-string (out)
    ; Reads the minus sign if present
    (when (json-read-char stream #\- :ignore-ws t) (write-char #\- out))
    ; Reads the integer part
    (write-string (json-read-digits stream) out)

    ; Reads the decimal part if present
    (when (char-equal (peek-char nil stream nil) #\.)
      (json-read-char stream #\.)
      (write-char #\. out)
      (write-string (json-read-digits stream) out)
    )

    ; Reads the exponent part if present
    (when (equal (peek-char nil stream nil) #\e)
      (json-read-char stream #\e) (write-char #\e out)
      (when (json-read-char stream #\+) (write-char #\+ out))
      (when (json-read-char stream #\-) (write-char #\- out))
      (write-string (json-read-digits stream) out)
    )
  ))
)


;;; Reads an escape sequence (after \)
(defun json-read-escape (stream)
  "Reads an escape sequence from a stream (after a \\)"
  (let ((*c* (read-char stream nil)))
    ; Decl
    (unless *c*
      (error "expected escape sequence"))

    (case *c*
      ; (#\" #\")
      ; (#\\ #\\)
      ; (#\/ #\/)
      (#\n #\linefeed)
      (#\t #\tab)
      (#\f #\page) ; #\formfeed is #\page
      (#\b #\backspace)
      (#\r #\return)

      ; unicode
      ; uABCD -> A = 0xA000
      ;          B = 0x0B00
      ;          C = 0x00C0
      ;          D = 0x000D
      ;  bitwise + = 0xABCD -> code to char
      (#\u (let ((*mostSB* (digit-char-p (read-char stream) 16))
                  (*moreSB* (digit-char-p (read-char stream) 16))
                  (*lessSB* (digit-char-p (read-char stream) 16))
                  (*leastSB* (digit-char-p (read-char stream) 16))
                )
              ; Decl
              (code-char (logior
                            (ash *mostSB* 12)
                            (ash *moreSB*  8)
                            (ash *lessSB*  4)
                            (ash *leastSB*  0)
                          )
              )
          )
      )

      (otherwise
        *c*)
    )
  )
)


;;; Read a JSON string from a stream until a char is found.
;;; out-stream is a class so it's passed by reference
(defun json-read-string-h (stream out-stream &key until (escapes nil))

  (let ((*c* (read-char stream nil)))
    ; Decl
    (unless *c*
      (error "unexpected eof"))

    ; Stop reading, return
    (when (char-equal *c* until)
      (return-from json-read-string-h T)
    )
    ; Got an escape read it or else write the read character
    (if (and (char-equal *c* #\\) escapes)
      (write-char (json-read-escape stream) out-stream)
      (write-char *c* out-stream)
    )

    (json-read-string-h stream out-stream :until until :escapes escapes)
  )
)


;;; Read a JSON string from a stream
(defun json-read-string (stream &key (escapes t))

  ; Starts with " and removes from stream
  (json-read-char stream #\" :ignore-ws t)

  (with-output-to-string (out)
    (json-read-string-h stream out :until #\" :escapes escapes)
  )
)


;;; Read the content of a JSON array from a stream
(defun json-read-array-h (stream out-list)
  ; Read a value and push to list
  (push (json-read-value stream) out-list)

  ; End recursion if there is a ] and return reverse of list
  (when (json-read-char stream #\] :ignore-ws t)
    (return-from json-read-array-h (reverse out-list))
  )

  ; Recurse if there is a comma and remove it
  (when (json-read-char stream #\, :ignore-ws t)
    (json-read-array-h stream out-list)
  )
)


;;; Read a JSON array from a stream
(defun json-read-array (stream)
  "Reads a JSON array from a stream"
  ; Starts with [ and removes from stream
  (json-read-char stream #\[ :ignore-ws t)

  ; If empty return empty list and remove ]
  (when (json-read-char stream #\] :ignore-ws t)
    (return-from json-read-array nil)
  )

  (json-read-array-h stream (list 'jsonarray))
)

;;; Read the content of a JSON object from a stream
(defun json-read-object-h (stream out-list)
  ; Read a value and push to list

  ; Read key
  (let ((*key* (json-read-string stream)))
    ; Read :
    (json-read-char stream #\: :ignore-ws t)
    ; Read value
    (let ((*value* (json-read-value stream)))
      ; Push to list
      (push (list *key* *value*) out-list)
    )
  )

  ; End recursion if there is a ] and return reverse of list
  (when (json-read-char stream #\} :ignore-ws t)
    (return-from json-read-object-h (reverse out-list))
  )

  ; Recurse if there is a comma and remove it
  (when (json-read-char stream #\, :ignore-ws t)
    (json-read-object-h stream out-list)
  )
)


;;; Read a JSON object from a stream
(defun json-read-object (stream)
  "Reads a JSON object from a stream"
  ; Starts with { and removes from stream
  (json-read-char stream #\{ :ignore-ws t)

  ; If empty return empty list and remove }
  (when (json-read-char stream #\} :ignore-ws t)
    (return-from json-read-object nil)
  )

  (json-read-object-h stream (list 'jsonobj))
)

;;; Read a JSON value from a stream
(defun json-read-value (stream)
  (case (peek-char t stream)
    (#\t (json-read-true stream))
    (#\f (json-read-false stream))
    (#\n (json-read-null stream))
    (#\" (json-read-string stream))
    (#\[ (json-read-array stream))
    (#\{ (json-read-object stream))
    (otherwise (json-read-number stream))
  )
)

;;; Writes a JSON value to a stream
(defun json-write-true (stream)
  (write-string "true" stream)
)

;;; Writes a JSON false to a stream
(defun json-write-false (stream)
  "Writes a JSON false to a stream"
  (write-string "false" stream)
)

;;; Writes a JSON null to a stream
(defun json-write-null (stream)
  (write-string "null" stream)
)

;;; Writes a JSON number to a stream
(defun json-write-number (stream value)
  (unless (numberp value) (return-from json-write-number nil))
  (write-string (format nil "~a" value) stream)
)

;;; Writes a JSON string to a stream
(defun json-write-string (stream value)
  (unless (stringp value) (return-from json-write-string nil))
  (format stream "\"~a\"" value)
)

;;; Writes the content of a JSON array to a stream
(defun json-write-array-h (stream vals)
  (let ((value (first vals))
        (rest (rest vals)))
      ;  Decl
    (json-write-value stream value)
    (when (not (null rest))
      (format stream ",")
      (json-write-array-h stream rest)
    )
  )
)

;;; Writes a JSON array to a stream
(defun json-write-array (stream arr)
  (unless (listp arr) (return-from json-write-array nil))
  (when (equal (first arr) 'jsonarray)
    (let ((vals (rest arr)))
      (format stream "[")
      (json-write-array-h stream vals)
      (format stream "]")
    )
  )
)

;;; Writes the content of a JSON object to a stream
(defun json-write-object-h (stream vals)
  (let ((key (first (first vals)))
        (value (second (first vals)))
        (rest (rest vals)))
      ;  Decl
    (json-write-string stream key)
    (format stream ":")
    (json-write-value stream value)
    (when (not (null rest))
      (format stream ",")
      (json-write-object-h stream rest)
    )
  )
)

;;; Writes a JSON object to a stream
(defun json-write-object (stream obj)
  (unless (listp obj) (return-from json-write-object nil))
  (when (equal (first obj) 'jsonobj)
    (let ((vals (rest obj)))
      (format stream "{")
      (json-write-object-h stream vals)
      (format stream "}")
    )
  )
)

;;; Writes a JSON value to a stream
(defun json-write-value (stream value)
  (when (numberp value)
    (return-from json-write-value (json-write-number stream value))
  )
  (when (stringp value)
    (return-from json-write-value (json-write-string stream value))
  )
  (when (listp value)
    (return-from json-write-value (case (first value)
      ('jsonarray (json-write-array stream value))
      ('jsonobj (json-write-object stream value))
    ))
  )
  (when (null value)
    (return-from json-write-value (json-write-null stream))
  )
  (when (equal value 'false)
    (return-from json-write-value (json-write-false stream))
  )
  (when value
    (return-from json-write-value (json-write-true stream))
  )
)

;;; Parses a JSON string
(defun jsonparse (string)
  (with-input-from-string (in string)
    (json-read-value in)
  )
)

;;; Reads a JSON file
(defun jsonread (path)
  (with-open-file (in path :direction :input)
    (json-read-value in)
  )
)

;;; Dumps a JSON object to a file
(defun jsondump (json path)
  (with-open-file (out path :direction :output)
    (json-write-value out json)
  )
)

;;; Access a JSON object
(defun jsonaccess (json &rest indices)
  ; if indices is empty return json
  (when (null (first indices)) (return-from jsonaccess json))
  ; if not a list, return nil
  (unless (listp json) (return-from jsonaccess nil))

  (let ((index (first indices))
        (idxs (rest indices))
        (discrim (first json))
        (value (rest json)))
    (case discrim
      ('jsonobj (when (stringp index)
        (apply #'jsonaccess
          (first (rest (assoc
                          index
                          value
                          :test (lambda (a b)
                                  (string-equal a b))
                       )
                 )
          )
          idxs
        )
      ))
      ('jsonarray (when (integerp index)
        (jsonaccess (nth index value) idxs)
      ))
      (otherwise nil)
  ))
)
