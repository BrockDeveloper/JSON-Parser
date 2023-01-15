;
; JSON parser
; 
; Author: Broccoletti Andrea 886155
; Author: Damiano Pellegrini 886261
;
(defpackage :json
  (:export
  )
)

(in-package :json)

(defun json-read-char (stream char &key (ignore-ws nil))
  "Read a char from a stream if present."
  (when (char-equal (peek-char ignore-ws stream) char)
    (read-char stream)
    t
  )
)

(defun json-read-true (stream)
  "Read the string \"true\" preceded by any whitespaces from a stream."
  (json-read-char stream #\t :ignore-ws t)
  (json-read-char stream #\r)
  (json-read-char stream #\u)
  (json-read-char stream #\e)
)

; It returns nil but removes the char from the stream,
; seems like it fails but it doesn't
(defun json-read-false (stream)
  "Read the string \"false\" preceded by any whitespaces from a stream."
  (json-read-char stream #\f :ignore-ws t)
  (json-read-char stream #\a)
  (json-read-char stream #\l)
  (json-read-char stream #\s)
  (json-read-char stream #\e)
  nil
)

; It returns nil but removes the char from the stream,
; seems like it fails but it doesn't
(defun json-read-null (stream)
  "Read the string \"null\" preceded by any whitespaces from a stream."
  (json-read-char stream #\n :ignore-ws t)
  (json-read-char stream #\u)
  (json-read-char stream #\l)
  (json-read-char stream #\l)
  nil
)

; out-stream is a class so it's passed by reference
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

(defun json-read-digits (stream)
  "Reads a sequence of digits from a stream as a string"
  (with-output-to-string (out)
    (json-read-digits-h stream out)
  )
)

(defun json-read-number (stream)
  "Reads a JSON number from a stream"
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

; Reads an escape sequence (after \)
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
      (#\f #\page) ; #\formfeed non esiste è #\page
      (#\b #\backspace)
      (#\r #\return)

      ; Conversione unicode
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

; out-stream is a class so it's passed by reference
(defun json-read-string-h (stream out-stream &key until (escapes nil))
  "Reads a JSON string from a stream until a char is found"

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

(defun json-read-string (stream &key (escapes t))
  "Reads a JSON string from a stream"

  ; Starts with " and removes from stream
  (json-read-char stream #\" :ignore-ws t)

  (with-output-to-string (out)
    (json-read-string-h stream out :until #\" :escapes escapes)
  )
)

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

(defun json-read-array (stream)
  "Reads a JSON array from a stream"
  ; Starts with [ and removes from stream
  (json-read-char stream #\[ :ignore-ws t)

  ; If empty return empty list and remove ]
  (when (json-read-char stream #\] :ignore-ws t) (return-from json-read-array (list 'jsonarray)))

  (json-read-array-h stream (list 'jsonarray))
)

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

(defun json-read-object (stream)
  "Reads a JSON object from a stream"
  ; Starts with { and removes from stream
  (json-read-char stream #\{ :ignore-ws t)

  ; If empty return empty list and remove }
  (when (json-read-char stream #\} :ignore-ws t) (return-from json-read-object (list 'jsonobj)))

  (json-read-object-h stream (list 'jsonobj))
)

(defun json-read-value (stream)
  "Reads a JSON value from a stream"
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

(defun json-write-true (stream)
  "Writes a JSON true to a stream"
  (write-string "true" stream)
)

(defun json-write-false (stream)
  "Writes a JSON false to a stream"
  (write-string "false" stream)
)

(defun json-write-null (stream)
  "Writes a JSON null to a stream"
  (write-string "null" stream)
)

(defun json-write-number (stream value)
  "Writes a JSON number to a stream"
  (unless (numberp value) (return-from json-write-number nil))
  (write-string (format nil "~a" value) stream)
)

(defun json-write-string (stream value)
  "Writes a JSON string to a stream"
  (unless (stringp value) (return-from json-write-string nil))
  (format stream "\"~a\"" value)
)

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

(defun json-write-array (stream arr)
  "Writes a JSON array to a stream"
  (unless (listp arr) (return-from json-write-array nil))
  (when (equal (first arr) 'jsonarray)
    (let ((vals (rest arr)))
      (format stream "[")
      (json-write-array-h stream vals)
      (format stream "]")
    )
  )
)

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

(defun json-write-object (stream obj)
  "Writes a JSON object to a stream"
  (unless (listp obj) (return-from json-write-object nil))
  (when (equal (first obj) 'jsonobj)
    (let ((vals (rest obj)))
      (format stream "{")
      (json-write-object-h stream vals)
      (format stream "}")
    )
  )
)

(defun json-write-value (stream value)
  "Writes a JSON value to a stream"
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

(defun jsonparse (string)
  "Parses a JSON string"
  (with-input-from-string (in string)
    (json-read-value in)
  )
)

(defun jsonread (path)
  "Reads a JSON file"
  (with-open-file (in path :direction :input)
    (json-read-value in)
  )
)

(defun jsondump (json path)
  "Dumps a JSON object to a file"
  (with-open-file (out path :direction :output)
    (json-write-value out json)
  )
)

(defun jsonaccess (json &rest indices)
  ; Se gli indici sono finiti ritorno la roba passata
  (when (null (first indices)) (return-from jsonaccess json))
  ; Se non è una lista ritorno nil
  (unless (listp json) (return-from jsonaccess nil))

  (let ((index (first indices))
        (idxs (rest indices))
        (discrim (first json))
        (value (rest json)))
    (case discrim
      ('jsonobj (when (stringp index)
        (apply #'jsonaccess (first (rest (assoc index value :test (lambda (a b) (string-equal a b))))) idxs)
      ))
      ('jsonarray (when (integerp index)
        (apply #'jsonaccess (nth index value) idxs)
      ))
      (otherwise nil)
  ))
)

; ; Read Tests
; (format t "number: ~s~%" (jsonparse "31.415e-1"))
; (format t "empty string: ~s~%" (jsonparse "      \"\""))
; (format t "string: ~s~%" (jsonparse "      \"sono una stringa\""))
; (format t "string con escapes: ~s~%" (jsonparse "      \"sono\\tuna\\fstringa\\ncon escape\""))
; (format t "empty array: ~s~&" (jsonparse "  [  ]"))
; (format t "~s~&" (jsonparse "[\"name\", true, null, 31.415e-1]"))
; (format t "empty object: ~s~&" (jsonparse "  {  }"))
; (format t "~s~&" (jsonparse "{\"name\": \"value\", \"true\": true, \"null\": {\"null\": [\"ciao\", true, 31.415e-1]}}"))
; (format t "access_dami-array: ~s~&" (jsonaccess (jsonparse "[3, 4, 5]") 1))
; (format t "access_dami-oggetto: ~s~&" (jsonaccess (jsonparse "{\"name\": \"value\", \"true\": true, \"null\": {\"null\": [\"ciao\", true, 31.415e-1]}}") "null"))
; (format t "access_pdf: ~s~&" (jsonaccess (jsonparse "{\"name\": \"Zaphod\", \"heads\": [[\"Head1\"],[\"Head2\"]]}") "heads" 1 0))

; ; Write Tests
; (format t "encoded: ~s~%" (with-output-to-string (out) (json-write-value out (jsonparse "{\"name\": \"value\", \"true\": true, \"null\": {\"null\": [\"ciao\", true, 31.415e-1]}}"))))

; ; Read/Write File Tests
; (jsondump (jsonread "test.json") "test1.json")
