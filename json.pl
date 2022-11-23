/** <json> Libreria Parsing JSON

Libreria per il parsing del formato JSON

@author 886155 Andrea Broccoletti
@author 886261 Damiano Pellegrini
@version 1.0
*/

% Object = jsonobj(Members)
% Object = jsonarray(Elements)
% Members = [] or
% Members = [Pair | MoreMembers]
% Pair = (Attribute, Value)
% Attribute = <string SWI Prolog>
% Number = <numero Prolog>
% Value = <string SWI Prolog> | Number | Object
% Elements = [] or
% Elements = [Value | MoreElements]


%! json_bool(+JSON:list, -false) is semidet
%
json_bool(JSON, false) :-
    strip(JSON, JSONStripped),
    JSONStripped == "false",
    !.

%! json_bool(+JSON:list, -[]) is semidet
%
json_bool(JSON, []) :-
    JSONStripped == "null".

%! json_bool(+JSON:list, -true) is semidet
%
json_bool(JSON, true) :-
    JSONStripped == "true".


json_number(JSONChars, Number) :-
    number_string(Number, JSONStripped).

%! json_parse(+JSON:string, -Object) is semidet
%
%  Se JSON è stringa converto in lista di caratteri
json_parse(JSON, Object) :-
    string(JSON),
    string_chars(JSON, JSONChars),
    json_parse(JSONChars, Object).

%! json_parse(+JSON:atom, -Object) is semidet
%
%  Se JSON è atomo converto in lista di caratteri
json_parse(JSON, Object) :-
    atom(JSON),
    atom_chars(JSON, JSONChars),
    json_parse(JSONChars, Object).

%! json_parse(+JSON:chars, -Object) is semidet
%
%  Se JSON è lista di caratteri, la analizzo
json_parse(JSONChars, Object) :-
    fail. % TODO

%!
%
json_string(['"'], []) :-
    !.
json_string(['"' | Cs], O) :-
    json_string(Cs, O),
    !.
json_string([C | Cs], [C | Os]) :-
    json_string(Cs, Os),
    !.

% Strip degli whitespace
%
strip([' ' | String], StringOut) :-
    strip(String, StringOut),
    !.
strip(['\r' | String], StringOut) :-
    strip(String, StringOut),
    !.
strip(['\n' | String], StringOut) :-
    strip(String, StringOut),
    !.
strip(['\t' | String], StringOut) :-
    strip(String, StringOut),
    !.
strip(String, String).