/** <json> Libreria Parsing JSON
 
 Libreria per il parsing del formato JSON
 
 @author 886155 Andrea Broccoletti
 @author 886261 Damiano Pellegrini
 @version 1.0
 */
:- module(json, []).

jbool(true, true).
jbool(false, false).
jbool(null, null).

jstring(S, S) :-
    string(S).
jnumber(N, N) :-
    number(N).

jvalue(S, Val) :-
    jbool(S, Val).
jvalue(S, Val) :-
    jarray(S, Val).
jvalue(S, Val) :-
    jobject(S, Val).
jvalue(S, Val) :-
    jnumber(S, Val).
jvalue(S, Val) :-
    jstring(S, Val).


jarray([], jsonarray([])) :- !.
jarray([X|Xs], jsonarray([Out|Outs])) :-
    jvalue(X, Out),
    jarray(Xs, jsonarray(Outs)).

jpair(Name: Value, (Key, Out)) :-
    jstring(Name, Key),
    jvalue(Value, Out).

jobject({}, jsonobject([])) :- !.
jobject({X}, jsonobject([Out])) :-
    jpair(X, Out),
    !.
jobject({X}, jsonobject([Out|Outs])) :-
    X =.. [',', X1, X2],
    jpair(X1, Out),
    jobject({X2}, jsonobject(Outs)).

json_parse(X, Out) :-
    string(X),
    atom_string(Atom, X),
    json_parse(Atom, Out).

json_parse(X, Out) :- 
    atom(X),
    term_to_atom(Term, X),
    jvalue(Term, Out),
    !.