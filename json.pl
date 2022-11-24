/** <json> Libreria Parsing JSON
 
 Libreria per il parsing del formato JSON
 
 @author 886155 Andrea Broccoletti
 @author 886261 Damiano Pellegrini
 @version 1.0
 */
:- module(json, []).

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

%? Documentazione su come documentare:
% https://www.swi-prolog.org/pldoc/man?section=modes

%! remove_last(+Remove:atom, +From:list, -Out:list) is det
remove_last(C, [C | Is], [], Is) :- !.
remove_last(C, [I | Is], [I | Os], Rs) :-
    remove_last(C, Is, Os, Rs).

%! json_null. is det
%
%  Valore nullo
json_null.
is_json_null(Val) :- Val == json_null.

%! json_bool(+JSONChars:list, -false) is semidet
%
json_bool(['f', 'a', 'l', 's', 'e' | Rest], false, Rest) :-
    !.

%! json_bool(+JSONChars:list, -[]) is semidet
%
json_bool(['n','u','l','l' | Rest], json_null, Rest) :-
    !.

%! json_bool(+JSONChars:list, -true) is semidet
%
json_bool(['t','r','u','e' | Rest], true, Rest) :-
    !.

%! json_number(+JSONChars:list, -Number:float) is det
%  Parsing di un numero
%  \-? (0 | ([1-9][0-9]*)) (\.[0-9]*)? ([+-]?[eE][0-9]*)?
%  Se leggo tutti i caratteri finche c'è un numero?
json_number(JSONChars, Number) :-
    number_chars(Number, JSONChars),
    !.

%! json_string(+Chars:list, ?Atom:atom) is det
json_string(['"' | Chars], Atom, Rest) :-
    var(Atom),
    remove_last('"', Chars, Tmp, Rest),
    atom_chars(Atom, Tmp).

%! json_string(-Chars:list, +Atom:atom) is det
% json_string(['"' | Chars], Atom) :-
%     atom(Atom),
%     atom_chars(Atom, Tmp),
%     append(Tmp, ['"'], Chars).

json_value(Chars, Value, Rest) :-
    json_string(Chars, Value, Rest),
    !.
% json_value(Chars, Value, Rest) :-
%     json_number(Chars, Value, Rest),
%     !.
% json_value(Chars, Value, Rest) :-
%     json_object(Chars, Value, Rest),
%     !.
json_value(Chars, Value, Rest) :-
    json_array(Chars, Value, Rest),
    !.
json_value(Chars, Value, Rest) :-
    json_bool(Chars, Value, Rest),
    !.

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


%! json_object(+JSONChars:list, -Object:jsonobj) is semidet
% TODO 
json_pair(Chars, (Key, Value), Rest) :-
    json_string(Chars, Key, AfterKey),
    json_ws(AfterKey, [':' | BWS]),
    json_ws(BWS, AWS),
    json_value(AWS, Value, Rest).

%! json_ws(JSONChars:list, -JSONChars:list) is det
%
% Remove whitespace from list.
%
json_ws([' ' | Chars], Rest) :-
    json_ws(Chars, Rest),
    !.

json_ws([0'\r | Chars], Rest) :-
    json_ws(Chars, Rest),
    !.

json_ws([0'\n | Chars], Rest) :-
    json_ws(Chars, Rest),
    !.

json_ws([0'\t | Chars], Rest) :-
    json_ws(Chars, Rest),
    !.

json_ws(Chars, Chars).


%!
%
% brackets([']'|Str], Str, []) :- !.
% brackets(['[',']'|Str], Str, json_array([])) :- !.
% brackets(['['|Str], Out, json_array([Element|Other])) :-
%     strip(Str, Out1),
%     value(Out1, Out2, Element),
%     strip(Out2, Out3),
%     brackets(Out3, Out, Other),
% brackets([','|Str], Out, [Element|Other]) :-
%     strip(Str, Out1),
%     value(Out1, Out2, Element),
%     strip(Out2, Out3),
%     brackets(Out3, Out, Other).

json_array(['[' | Chars], jsonarray([]), Rest) :-
    json_ws(Chars, [']' | Rest]),
    !.
json_array(['[' | Chars], jsonarray([Value]), Rest) :-
    json_ws(Chars, W1),
    json_value(W1, Value, [']' | Rest]),
    !.
json_array(['[' | Chars], jsonarray([Value|Others]), Rest) :-
    json_ws(Chars, W1),
    json_value(W1, Value, AV),
    json_ws(AV, WS2),
    json_array(WS2, Others, Rest),
    !.
json_array([',' | Chars], jsonarray([Value|Others]), Rest) :-
    json_ws(Chars, W1),
    json_value(W1, Value, AV),
    json_ws(AV, WS2),
    json_array(WS2, Others, Rest),
    !.