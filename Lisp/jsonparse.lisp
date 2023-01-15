;;;; -*- Mode: Lisp -*-
;;;; jsonparse.l


;;;; Author: 886155 Andrea Broccoletti
;;;; Author: 886261 Damiano Pellegrini


;;; Legge un carattere da uno stream, se presente.
(defun json-read-char (stream char &key (ignore-ws nil))
  (when (char-equal (peek-char ignore-ws stream) char)
    (read-char stream)))


;;; Legge la stringa \"true\" preceduta da eventuali spazi
;;; bianchi da stream.
;;; Restituisce true ma rimuove il carattere dallo stream.
(defun json-read-true (stream)
  (json-read-char stream #\t :ignore-ws t)
  (json-read-char stream #\r)
  (json-read-char stream #\u)
  (json-read-char stream #\e))


;;; Legge la stringa \"false\" preceduta da eventuali spazi
;;; bianchi da stream.
;;; Restituisce false ma rimuove il carattere dallo stream.
(defun json-read-false (stream)
  (json-read-char stream #\f :ignore-ws t)
  (json-read-char stream #\a)
  (json-read-char stream #\l)
  (json-read-char stream #\s)
  (json-read-char stream #\e)
  'false)


;;; Legge la stringa \"null\" preceduta da eventuali spazi bianchi
;;; da stream.
;;; Restituisce nil ma rimuove il carattere dallo stream.
(defun json-read-null (stream)
  (json-read-char stream #\n :ignore-ws t)
  (json-read-char stream #\u)
  (json-read-char stream #\l)
  (json-read-char stream #\l)
  nil)


;;; Legge una stringa JSON da uno stream.
;;; out-stream e' una classe quindi viene passato per riferimento.
(defun json-read-digits-h (stream out-stream)
  (let ((*c* (peek-char nil stream nil)))
    (unless *c*
      (return-from json-read-digits-h T))

    ; Se non e' una cifra, termina
    (unless (digit-char-p *c*)
      (return-from json-read-digits-h T))

    ; Char e' una cifra, scrive sull'output stream
    (write-char (read-char stream) out-stream)

    (json-read-digits-h stream out-stream)))


;;; Legge una sequenza di cifre da uno stream come stringa.
(defun json-read-digits (stream)
  (with-output-to-string (out)
                         (json-read-digits-h stream out)))


;;; Legge un numero JSON da uno stream.
(defun json-read-number (stream)
  (read-from-string
   (with-output-to-string
     (out)

     ; Legge il segno meno, se presente
     (when (json-read-char stream #\- :ignore-ws t)
       (write-char #\- out))

     ; Legge la aprte intera
     (write-string (json-read-digits stream) out)

     ; Legge la parte decimale, se presente
     (when (char-equal (peek-char nil stream nil) #\.)
       (json-read-char stream #\.)
       (write-char #\. out)
       (write-string (json-read-digits stream) out))

     ; Legge la parte esponenziale, se presente
     (when (equal (peek-char nil stream nil) #\e)
       (json-read-char stream #\e) (write-char #\e out)
       (when (json-read-char stream #\+) (write-char #\+ out))
       (when (json-read-char stream #\-) (write-char #\- out))
       (write-string (json-read-digits stream) out)))))


;;; Legge una sequenza di escape (dopo \)
(defun json-read-escape (stream)
  (let ((*c* (read-char stream nil)))
    (unless *c*
      (error "expected escape sequence"))

    (case *c*
          (#\n #\linefeed)
          (#\t #\tab)
          (#\f #\page)
          (#\b #\backspace)
          (#\r #\return)

          (#\u (let ((*mostSB* (digit-char-p
                                (read-char stream) 16))
                     (*moreSB* (digit-char-p
                                (read-char stream) 16))
                     (*lessSB* (digit-char-p
                                (read-char stream) 16))
                     (*leastSB* (digit-char-p
                                 (read-char stream) 16)))
                 (code-char (logior
                             (ash *mostSB* 12)
                             (ash *moreSB*  8)
                             (ash *lessSB*  4)
                             (ash *leastSB*  0)))))

          (otherwise *c*))))


;;; Legge una stringa JSON da uno stream fino a un carattere.
;;; out-stream e' una classe quindi viene passato per riferimento.
(defun json-read-string-h (stream out-stream &key until (escapes nil))
  (let ((*c* (read-char stream nil)))
    (unless *c*
      (error "unexpected eof"))

    ; Ferma la lettura
    (when (char-equal *c* until)
      (return-from json-read-string-h T))

    ; Legge un escape o scrive il carettere letto
    (if (and (char-equal *c* #\\) escapes)
        (write-char (json-read-escape stream) out-stream)
      (write-char *c* out-stream))

    (json-read-string-h stream out-stream :until until :escapes escapes)))


;;; Legge una stringa JSON da uno stream.
(defun json-read-string (stream &key (escapes t))
  ; Inizia con " e rimuove dallo stream
  (json-read-char stream #\" :ignore-ws t)

  (with-output-to-string (out)
                         (json-read-string-h
                          stream
                          out
                          :until #\"
                          :escapes escapes)))


;;; Legge un array JSON da uno stream fino a un carattere.
(defun json-read-array-h (stream out-list)
  ; Legge un valore e lo aggiunge alla lista
  (push (json-read-value stream) out-list)

  ; Termina se c'e' un ] e ritorna la lista al contrario
  (when (json-read-char stream #\] :ignore-ws t)
    (return-from json-read-array-h (reverse out-list)))

  ; Ricorsione se c'e' una virgola e la rimuove
  (when (json-read-char stream #\, :ignore-ws t)
    (json-read-array-h stream out-list)))


;;; Legge un array JSON da uno stream.
(defun json-read-array (stream)
  ; inizia con [ e rimuove dallo stream
  (json-read-char stream #\[ :ignore-ws t)

  ; Se vuoto ritorna una lista vuota e rimuove ]
  (when (json-read-char stream #\] :ignore-ws t)
    (return-from json-read-array (list 'jsonarray)))

  (json-read-array-h stream (list 'jsonarray)))


;;; Legge un oggetto JSON da uno stream fino a un carattere.
(defun json-read-object-h (stream out-list)
  ; Legge la key
  (let ((*key* (json-read-string stream)))
    ; Legge :
    (json-read-char stream #\: :ignore-ws t)
    ; Legge il valore
    (let ((*value* (json-read-value stream)))
      ; Inserisce nella lista
      (push (list *key* *value*) out-list)))

  ; Termina se c'e' un } e ritorna la lista al contrario
  (when (json-read-char stream #\} :ignore-ws t)
    (return-from json-read-object-h (reverse out-list)))

  ; Ricorsione se c'e' una virgola e la rimuove
  (when (json-read-char stream #\, :ignore-ws t)
    (json-read-object-h stream out-list)))


;;; Legge un oggetto JSON da uno stream.
(defun json-read-object (stream)
  ; Inizia con { e rimuove dallo stream
  (json-read-char stream #\{ :ignore-ws t)

  ; Se vuoto ritorna una lista vuota e rimuove }
  (when (json-read-char stream #\} :ignore-ws t)
    (return-from json-read-object (list 'jsonobj)))

  (json-read-object-h stream (list 'jsonobj)))


;;; Legge un valore JSON da uno stream.
(defun json-read-value (stream)
  (case (peek-char t stream)
        (#\t (json-read-true stream))
        (#\f (json-read-false stream))
        (#\n (json-read-null stream))
        (#\" (json-read-string stream))
        (#\[ (json-read-array stream))
        (#\{ (json-read-object stream))
        (otherwise (json-read-number stream))))


;;; Scrive un valore JSON su uno stream.
(defun json-write-true (stream)
  (write-string "true" stream))


;;; Scrive il valore false su uno stream.
(defun json-write-false (stream)
  (write-string "false" stream))


;;; Scrive il valore null su uno stream.
(defun json-write-null (stream)
  (write-string "null" stream))


;;; Scrive un numero JSON su uno stream.
(defun json-write-number (stream value)
  (unless (numberp value) (return-from json-write-number nil))
  (write-string (format nil "~a" value) stream))


;;; Scrive una stringa JSON su uno stream.
(defun json-write-string (stream value)
  (unless (stringp value) (return-from json-write-string nil))
  (format stream "\"~a\"" value))


;;; Scrive il contenuto di un array JSON su uno stream.
(defun json-write-array-h (stream vals)
  (let ((value (first vals))
        (rest (rest vals)))
    (json-write-value stream value)

    (when (not (null rest))
      (format stream ",")
      (json-write-array-h stream rest))))


;;; Scrive un array JSON su uno stream.
(defun json-write-array (stream arr)
  (unless (listp arr) (return-from json-write-array nil))

  (when (equal (first arr) 'jsonarray)
    (let ((vals (rest arr)))
      (format stream "[")
      (json-write-array-h stream vals)
      (format stream "]"))))


;;; Scrive il contenuto di un oggetto JSON su uno stream.
(defun json-write-object-h (stream vals)
  (let ((key (first (first vals)))
        (value (second (first vals)))
        (rest (rest vals)))
    (json-write-string stream key)
    (format stream ":")
    (json-write-value stream value)

    (when (not (null rest))
      (format stream ",")
      (json-write-object-h stream rest))))


;;; Scrive un oggetto JSON su uno stream.
(defun json-write-object (stream obj)
  (unless (listp obj) (return-from json-write-object nil))

  (when (equal (first obj) 'jsonobj)
    (let ((vals (rest obj)))
      (format stream "{")
      (json-write-object-h stream vals)
      (format stream "}"))))


;;; Scrive un valore JSON su uno stream.
(defun json-write-value (stream value)
  (when (numberp value)
    (return-from json-write-value (json-write-number stream value)))

  (when (stringp value)
    (return-from json-write-value (json-write-string stream value)))

  (when (listp value)
    (return-from json-write-value (case (first value)
                                        ('jsonarray
                                         (json-write-array stream value))
                                        ('jsonobj
                                         (json-write-object stream value)))))

  (when (null value)
    (return-from json-write-value (json-write-null stream)))

  (when (equal value 'false)
    (return-from json-write-value (json-write-false stream)))

  (when value
    (return-from json-write-value (json-write-true stream))))


;;; Parse di una stringa JSON.
(defun jsonparse (string)
  (with-input-from-string (in string)
                          (json-read-value in)))


;;; Legge un file JSON.
(defun jsonread (path)
  (with-open-file (in path :direction :input)
                  (json-read-value in)))


;;; Scrive un oggetto JSON su un file.
(defun jsondump (json path)
  (with-open-file (out path :direction :output)
                  (json-write-value out json)))


;;; Accede ad un oggetto JSON, restituendo il valore corrispondente.
(defun jsonaccess (json &rest indices)
  ; Se gli indici sono vuoti, restituisce il JSON.
  (when (null (first indices)) (return-from jsonaccess json))
  ; Se non e' una lista, restituisce nil.
  (unless (listp json) (return-from jsonaccess nil))

  (let ((index (first indices))
        (idxs (rest indices))
        (discrim (first json))
        (value (rest json)))
    (case discrim
          ('jsonobj (when (stringp index)
                      (apply #'jsonaccess
                             (first (rest
                                     (assoc
                                      index
                                      value
                                      :test (lambda (a b)
                                              (string-equal a b)))))
                             idxs)))
          ('jsonarray (when (integerp index)
                        (apply #'jsonaccess (nth index value) idxs)))
          (otherwise nil))))


;;;;	end of file -- jsonparse.lisp --
