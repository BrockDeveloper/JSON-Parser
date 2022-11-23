% wrap, converte la stringa in ingresso in un array di caratteri
%
eval(String) :-
    string_chars(String, CharString),
    whitespace(CharString, CharStringOut),
    is_number(CharStringOut).


% gestione degli whitespace
%
whitespace([' ' | String], StringOut) :-
    whitespace(String, StringOut),
    !.
whitespace(String, String).