(defpackage :json
    (:export
    ))

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

(defun json-read-digits (stream)
  "Reads a sequence of digits from a stream"
  (with-output-to-string (out)
    (loop
      (let ((c (peek-char nil stream)))
        ; Decl
        ; exits loop if not a digit
        (unless (digit-char-p c)
          (return out)
        )

        (write-char (read-char stream) out)
      )
    )
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
    (when (json-read-char stream #\.)
      (write-char #\. out)
      (write-string (json-read-digits stream) out)
    )

    ; Reads the exponent part if present
    (when (json-read-char stream #\e) (write-char #\e out)
      (when (json-read-char stream #\+) (write-char #\+ out))
      (when (json-read-char stream #\-) (write-char #\- out))
      (write-string (json-read-digits stream) out)
    )
  ))
)

; Reads an escape sequence (after \)
(defun json-read-escape (stream)
  "Reads an escape sequence from a stream (after a \\)"
  (let ((c (read-char stream nil)))
    ; Decl
    (unless c
      (error "expected escape sequence"))

    (case c
      ; (#\" #\")
      ; (#\\ #\\)
      ; (#\/ #\/)
      (#\n #\linefeed)
      (#\t #\tab)
      (#\f "\f")
      (#\b #\backspace)
      (#\r #\return)

      ; Conversione unicode
      ; uABCD -> A = 0xA000
      ;          B = 0x0B00
      ;          C = 0x00C0
      ;          D = 0x000D
      ;  bitwise + = 0xABCD -> code to char
      (#\u (let ((mostSB (digit-char-p (read-char stream) 16))
                  (moreSB (digit-char-p (read-char stream) 16))
                  (lessSB (digit-char-p (read-char stream) 16))
                  (leastSB (digit-char-p (read-char stream) 16))
                )
              ; Decl
              (code-char (logior
                            (ash mostSB 12)
                            (ash moreSB  8)
                            (ash lessSB  4)
                            (ash leastSB  0)
                          )
              )
          )
      )

      (otherwise
        c)
    )
  )
)

(defun json-read-string (stream)
  "Reads a JSON string from a stream"
  ; Starts with " and removes from stream
  (json-read-char stream #\" :ignore-ws t)

  (with-output-to-string (out)
    ; Decl
    (loop
      (let ((c (read-char stream nil)))
        ; Decl
        (unless c
          (error "expected \" or any other character")
        )

        (case c
          ; End of string
          (#\" (return))
          ; Escape sequence
          (#\\ (write-char (json-read-escape stream) out))
          (otherwise (write-char c out))
        )
      )
    )
  )
)

(defun json-read-array (stream)
  "Reads a JSON array from a stream"
  ; Starts with [ and removes from stream
  (json-read-char stream #\[ :ignore-ws t)

  ; If empty return empty list and remove ]
  (when (json-read-char stream #\] :ignore-ws t) (return-from json-read-array nil))
  ; (when (char-equal (peek-char t stream) #\]) (return-from json-read-array nil))
  (loop
    for V = (json-read-value stream)
    collect V
    into Vs
    
    ; Skip comma and repeat
    while (json-read-char stream #\, :ignore-ws t)

    ; There are no other values
    finally (return (when (json-read-char stream #\] :ignore-ws t) Vs))
  )
)

(defun json-read-object (stream)
  "Reads a JSON object from a stream"
  ; Starts with { and removes from stream
  (json-read-char stream #\{ :ignore-ws t)

  ; If empty return empty list and remove }
  (when (json-read-char stream #\} :ignore-ws t) (return-from json-read-object nil))
  (loop
    for key = (json-read-string stream)
    for sep = (json-read-char stream #\: :ignore-ws t)
    for value = (json-read-value stream)
    collect (list key value)
    into Vs
    
    ; Skip comma and repeat
    while (json-read-char stream #\, :ignore-ws t)

    ; There are no other values
    finally (return (when (json-read-char stream #\} :ignore-ws t) Vs))
  )
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

(format t "empty array: ~s~%" (json-read-value (make-string-input-stream "  [  ]")))
(format t "~s~%" (json-read-value (make-string-input-stream "[\"name\", true, null]")))
(format t "empty object: ~s~%" (json-read-value (make-string-input-stream "  {  }")))
(format t "~s" (json-read-value (make-string-input-stream "{\"name\": \"value\", \"true\": true, \"null\": {\"null\": [\"ciao\", true, 31.415e-1]}}")))
