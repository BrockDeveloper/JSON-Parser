%%%% Author: 886155 Andrea Broccoletti
%%%% Author: 886261 Damiano Pellegrini


%%%% -*- Mode: Prolog -*-
%%%% jsonparse.pl


%! jbool(S, Val)
%  jbool e' vero se Val e' il valore booleano rappresentato da S.
%
jbool(true, true).

jbool(false, false).

jbool(null, null).


%! jstring(S, Val) 
%  jstring e' vero se Val e' la stringa rappresentata da S.
%
jstring(S, S) :- string(S).


%! jnumber(S, Val)
%  jnumber e' vero se Val e' il numero rappresentato da S.
%
jnumber(N, N) :- number(N).


%! jvalue(S, Val)
%  jvalue e' vero se Val e' il valore rappresentato da S.
%  Un valore può essere un oggetto, un array, una stringa, 
%  un numero o un booleano.
%
jvalue(S, Val) :- jbool(S, Val).
jvalue(S, Val) :- jarray(S, Val).
jvalue(S, Val) :- jobject(S, Val).
jvalue(S, Val) :- jnumber(S, Val).
jvalue(S, Val) :- jstring(S, Val).


%! jarray(S, Val)
%  jarray e' vero se Val e' l'array rappresentato da S.
%  Val e' nella forma jsonarray([Val1, Val2, ..., Valn]).
%
jarray([], jsonarray([])) :- !.

jarray([X | Xs], jsonarray([Out | Outs])) :-
    jvalue(X, Out),
    jarray(Xs, jsonarray(Outs)).


%! jpair(S, Val)
%  jpair e' vero se Val e' la coppia rappresentata da S.
%  S e' nella forma Name: Value.
%  Val e' nella forma (Key, Out) dove Key e' la stringa Name
%  e Out e' il valore Value.
%
jpair(Name: Value, (Key, Out)) :-
    jstring(Name, Key),
    jvalue(Value, Out).


%! jobject(S, Val)
%  jobject e' vero se Val e' l'oggetto rappresentato da S.
%  Val e' nella forma jsonobj([(Key1, Val1), ..., (Keyn, Valn)]).
%
jobject({}, jsonobj([])) :- !.

jobject({X}, jsonobj([Out])) :-
    jpair(X, Out),
    !.

jobject({X}, jsonobj([Out|Outs])) :-
    X =.. [',', X1, X2],
    jpair(X1, Out),
    jobject({X2}, jsonobj(Outs)).


%! jsonparse(S, Val)
%  jsonparse e' vero se Val e' l'oggetto Prolog rappresentato 
%  dalla stringa Json S.
%
jsonparse(X, Out) :-
    string(X),
    atom_string(Atom, X),
    jsonparse(Atom, Out).

jsonparse(X, Out) :- 
    atom(X),
    catch(
        % Se la stringa non e' un json valido, 
        % viene lanciata un'eccezione.
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


%! jsonaccess(Obj, Key, Val)
%! jsonaccess(Obj, Keys, Val)
%  jsonaccess e' vero se Val e' il valore di Obj alla chiave Key,
%  oppure se Val e' il valore di Obj seguendo la sequenza di chiavi Keys.
%  
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


%! jsonread(File, Obj)
%  jsonread e' vero se Obj e' il valore rappresentato dalla stringa
%  Json contenuta nel file File.
%
jsonread(File, Obj) :-
    access_file(File, read),
    open(File, read, Stream),
    read_string(Stream, _, String),
    close(Stream),
    jsonparse(String, Obj),
    !.


%! jsondump(File, Obj)
%  jsondump e' vero se Obj e' scritto nel file File come stringa Json.
%
jsondump(Obj, File) :-
    access_file(File, write),
    open(File, write, Stream),
    jsonparse(String, Obj),
    write(Stream, String),
    close(Stream),
    !.
