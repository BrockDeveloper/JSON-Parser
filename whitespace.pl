
% jsonParse
%
% Parse di un value JSON qualunque:
% - può essere un oggetto a sua volta
% - oppure una array
% - oppure una stringa
% - oppure un numero
%

% jsonParse caso booleano negativo
%
jsonParse(JsonString, false) :-
    strip(JsonString, JsonStringStripped),
    JsonStringStripped == "false".

% jsonParse caso booleano positivo
%
jsonParse(JsonString, true) :-
    strip(JsonString, JsonStringStripped),
    JsonStringStripped == "true".

% jsonParse caso valore numerico
%
jsonParse(JsonString, Number) :-
    strip(JsonString, JsonStringStripped),
    number_string(Number, JsonStringStripped).







% Strip degli whitespace
%
strip([' ' | String], StringOut) :-
    strip(String, StringOut),
    !.
strip(String, String).