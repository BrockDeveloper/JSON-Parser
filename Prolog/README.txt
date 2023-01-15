# JSON PARSER - SWI Prolog


## Autori
- Andrea Broccoletti [mat. 886155]
- Damiano Pellegrini [mat. 886261]


## Descrizione
Libreria, scritta in Prolog, che consente di costruire una struttura dati che 
rappresenta degli oggetti JSON, a partire dalla loro rappresentazione come 
stringhe.


## Specifiche integrative
- La stringa JSON fornita al jsonparse, viene convertita in atomo e poi
  in termine. Se viene fornito un atomo, viene convertito in termine.
  Il termine così ottenuto viene passato al parse, che si occupa di ricostruire
  l'oggetto Prolog corrispondente alla stringa JSON, attraverso una serie di
  unificazioni. Il predicato è invertibile, ovvero fornito un oggetto JSON, è
  in grado di ricostruire la stringa JSON corrispondente.
- In particolare, tutti i predicati sono invertibili.
- Implementata la gestione dei caratteri UNICODE


## Struttura dati
La sintassi della struttura dati che rappresenta l'oggetto JSON in Prolog
è stata definita ricorsivamente nel modo seguente.

Object = jsonobj(Members)
Object = jsonarray(Elements)
Members = []
Members = [Pair | MoreMembers]
Pair = (Attribute, Value)
Attribute = <string SWI Prolog>
Number = <numero Prolog>
Value = <string SWI Prolog> | Number | Object
Elements = []
Elements = [Value | MoreElements]


## Utilizzo del Parser
il parser fornisce due funzioni:

JSONPARSE
?- jsonparse(JSONString, Object).
Risulta vero se JSONString (una stringa SWI Prolog o un atomo Prolog) può venire
scorporata come stringa, numero, o nei termini composti così come sopra 
descritte.

JSONACCESS
?- jsonaccess(Jsonobj, Fields, Result)
Risulta vero quando Result è recuperabile seguendo la catena di campi presenti
in Fields (una lista) a partire da Jsonobj. Un campo rappresentato da N>=0
corrisponde a un indice di un array JSON.


## Input/Output su file
Questa libreria fornisce anche due funzioni per la lettura e la scrittura
su file.

JSONREAD
?- jsonread(FileName, JSON).
Apre il file e ha successo se riesce a costruire un oggetto JSON.

JSONDUMP
?- jsondump(JSON, FileName).
Scrive l'oggetto JSON sul file in sintassi JSON.


## Esempi di utilizzo
Considerando i seguenti nella forma:
<comando>
<risposta>*

e.g. 1
?- jsonparse('{"nome" : "Arthur", "cognome" : "Dent"}', O),
jsonaccess(O, ["nome"], R).
O = jsonobj([(”nome”, ”Arthur”), (”cognome”, ”Dent”)])
R = ”Arthur”

e.g. 2
?- jsonparse(’[]’, X).
X = jsonarray([]).
