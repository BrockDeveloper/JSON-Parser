# JSON PARSER - Common Lisp


## Autori
- Andrea Broccoletti [mat. 886155]
- Damiano Pellegrini [mat. 886261]


## Descrizione
libreria, scritta in Lisp, che consente di costruire una struttura dati che 
rappresenta degli oggetti JSON, a partire dalla loro rappresentazione come 
stringhe.


## Specifiche integrative
- La funzione jsonparse riceve una stringa JSON. Questa viene trattata come UNICODE
  stream, e il parsing viene effettuato carattere per carattere, leggendo questi
  dallo stream associato alla stringa.
- Implementata la gestione dei caratteri UNICODE


## Struttura dati
La sintassi della struttura dati che rappresenta l'oggetto JSON in Common Lisp
è stata definita ricorsivamente nel modo seguente.

Object = '(' jsonobj memberd ')'
Object = '(' jsonarray elements ')'
members = pair*
pair = '(' attribute value ')'
attribute = <stringa Common Lisp>
number = <numero Common Lisp>
value = string | number | Object
elements = value*


## utilizzo del Parser
il parser fornisce due funzioni:

JSONPARSE
Accetta in ingresso una stringa.
Produce una struttura così come sopra definita.

JSONPARSE
Accetta un oggetto JSON, così come prodotto dal JSONPARSE e una serie di campi.
Recupera l'oggetto corrispondente.
n.b. Un campo rappresentato da N>=0 rappresenta un indice di un array JSON.


## Input/Output su file
Questa libreria fornisce anche due funzioni per la lettura e la scrittura
su file.

JSONREAD
(jsonread filename)
Apre il file, leggendo una stringa JSON e generando il corrispondente
oggetto JSON.

JSONDUMP
(jsondump JSON filename)
Scrive l'oggetto JSON sul file in sintassi JSON.


## Esempi di utilizzo
Considerando preventivamente:
(defparameter x (jsonparse "{\"nome\" : \"Arthur\",
                             \"cognome\" : \"Dent\"}"))

E che i seguenti sono nella forma:
<comando>
<risposta>

e.g. 1
(jsonaccess x "cognome")
"Dent"

e.g. 2
(jsonaccess (jsonparse
            "{\"name\" : \"Zaphod\",
              \"heads\" : [[\"Head1\"], [\"Head2\"]]}")
          "heads" 1 0)
"Head2"

e.g. 3
(jsonparse "[1, 2, 3]")
(JSONARRAY 1 2 3)