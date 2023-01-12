%%%% Author: 886155 Andrea Broccoletti
%%%% Author: 886261 Damiano Pellegrini

%%%% -*- Mode: Prolog -*-
%%%% jsonparse.pl


%! jbool(+S, -Val) is det
%  jbool/2 is true if Val is the boolean value represented by S.
%
jbool(true, true).


%! jbool(+S, -Val) is det
%  jbool/2 is true if Val is the boolean value represented by S.
%
jbool(false, false).

jbool(null, null).


%! jstring(+S, -Val) is det
%  jstring/2 is true if Val is the string represented by S.
%
jstring(S, S) :- string(S).


%! jnumber(+S, -Val) is det
%  jnumber/2 is true if Val is the number represented by S.
%
jnumber(N, N) :- number(N).


%! jvalue(+S, -Val) is det
%  jvalue/2 is true if Val is the value represented by S.
%
jvalue(S, Val) :- jbool(S, Val).
jvalue(S, Val) :- jarray(S, Val).
jvalue(S, Val) :- jobject(S, Val).
jvalue(S, Val) :- jnumber(S, Val).
jvalue(S, Val) :- jstring(S, Val).


%! jarray(+S, -Val) is det
%  jarray/2 is true if Val is the array represented by S.
%
jarray([], jsonarray([])) :- !.

jarray([X|Xs], jsonarray([Out|Outs])) :-
    jvalue(X, Out),
    jarray(Xs, jsonarray(Outs)).


%! jpair(+S, -Val) is det
%  jpair/2 is true if Val is the pair represented by S.
%
jpair(Name: Value, (Key, Out)) :-
    jstring(Name, Key),
    jvalue(Value, Out).


%! jobject(+S, -Val) is det
%  jobject/2 is true if Val is the object represented by S.
%
jobject({}, jsonobj([])) :- !.

jobject({X}, jsonobj([Out])) :-
    jpair(X, Out),
    !.

jobject({X}, jsonobj([Out|Outs])) :-
    X =.. [',', X1, X2],
    jpair(X1, Out),
    jobject({X2}, jsonobj(Outs)).


%! jsonparse(+S, -Val) is det
%  jsonparse/2 is true if Val is the value represented by S.
%
jsonparse(X, Out) :-
    string(X),
    atom_string(Atom, X),
    jsonparse(Atom, Out).

jsonparse(X, Out) :- 
    atom(X),
    catch(
        term_to_atom(Term, X),
        error(syntax_error(cannot_start_term), _),
        fail
    ),
    jvalue(Term, Out),
    !.

jsonparse(Out, X) :-
    nonvar(X),
    jvalue(Term, X),
    term_to_atom(Term, Out),
    !.

%! jsonaccess(+Obj, +Key, -Val) is det
%  jsonaccess/3 is true if Val is the value of Obj at Key.
%
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


%! jsonread(+File, -Obj) is det
%  jsonread/2 is true if Obj is the value represented by the file File.
%
jsonread(File, Obj) :-
    access_file(File, read),
    open(File, read, Stream),
    read_string(Stream, _, String),
    close(Stream),
    jsonparse(String, Obj),
    !.


%! jsonwrite(+File, +Obj) is det
%  jsonwrite/2 is true if Obj is written to the file File.
%
jsondump(Obj, File) :-
    access_file(File, write),
    open(File, write, Stream),
    jsonparse(String, Obj),
    write(Stream, String),
    close(Stream),
    !.