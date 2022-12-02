/** <json> Libreria Parsing JSON
 
 Libreria per il parsing del formato JSON
 
 @author 886155 Andrea Broccoletti
 @author 886261 Damiano Pellegrini <damiano1.pellegrini@gmail.com>
 @version 1.0
 */
:- module(json, [jsonparse/2, jsonaccess/3]).

jbool(true, true).
jbool(false, false).
jbool(null, null).

jstring(S, S) :- string(S).
jnumber(N, N) :- number(N).

jvalue(S, Val) :- jbool(S, Val).
jvalue(S, Val) :- jarray(S, Val).
jvalue(S, Val) :- jobject(S, Val).
jvalue(S, Val) :- jnumber(S, Val).
jvalue(S, Val) :- jstring(S, Val).

jarray([], jsonarray([])) :- !.
jarray([X|Xs], jsonarray([Out|Outs])) :-
    jvalue(X, Out),
    jarray(Xs, jsonarray(Outs)).

jpair(Name: Value, (Key, Out)) :-
    jstring(Name, Key),
    jvalue(Value, Out).

jobject({}, jsonobj([])) :- !.
jobject({X}, jsonobj([Out])) :-
    jpair(X, Out),
    !.
jobject({X}, jsonobj([Out|Outs])) :-
    X =.. [',', X1, X2],
    jpair(X1, Out),
    jobject({X2}, jsonobj(Outs)).

jsonparse(X, Out) :-
    string(X),
    var(Out),
    atom_string(Atom, X),
    jsonparse(Atom, Out).

jsonparse(X, Out) :- 
    atom(X),
    var(Out),
    term_to_atom(Term, X),
    jvalue(Term, Out),
    !.

jsonparse(Out, X) :-
    var(Out),
    nonvar(X),
    jvalue(Term, X),
    term_to_atom(Term, Out),
    !.

jsonaccess(jsonobj(T), [], jsonobj(T)) :- !.
jsonaccess(O, Key, R) :- 
    string(Key),
    jsonaccess(O,[Key],R),
    !.
jsonaccess(jsonobj([(Key, Value) | _]), [Key], Value) :- !.
jsonaccess(jsonobj([(Key, Value) | _]), [Key | Keys], Out) :-
    jsonaccess(Value, Keys, Out),
    !.
jsonaccess(jsonobj([_ | Ps]), [Key | Keys], Out) :-
    jsonaccess(jsonobj(Ps), [Key | Keys], Out),
    !.

jsonaccess(jsonarray([Value | _]), [0], Value) :- !.
jsonaccess(O, N, R) :-
    number(N),
    jsonaccess(O, [N], R),
    !.
jsonaccess(jsonarray([Value | _]), [0 | Idx], Out) :-
    jsonaccess(Value, Idx, Out),
    !.
jsonaccess(jsonarray([_ | Vs]), [Id | Idx], Out) :-
    Id > 0,
    NxId is Id - 1,
    jsonaccess(jsonarray(Vs), [NxId | Idx], Out),
    !.
