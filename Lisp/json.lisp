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

; Reads an escape sequence (after \)
(defun json-read-escape (stream)
  (let ((c (read-char stream nil)))
    ; Decl
    (unless c
      (error "JSON error: unexpected eof"))

    (case c
      (#\" #\")
      (#\\ #\\)
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
  ; Starts with " and removes from stream
  (json-read-char stream #\" :ignore-ws t)

  (with-output-to-string (out)
    ; Decl
    (loop
      (let ((c (read-char stream nil)))
        ; Decl
        (unless c
          (error "JSON error: unexpected eof")
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
