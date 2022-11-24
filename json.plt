:- begin_tests(json).
:- use_module(library(json)).

test(is_null, true) :-
    json_is_null(json_null).

test(is_null, fail) :-
    json_is_null([]).

test(is_true) :-
    atom_chars(true, Chars),
    json_bool(Chars, Atom),
    assertion(Atom == true).
    
% test(is_true_rev) :-
%     json_bool(Chars, true).

% test(is_null, true(Atom == json_null)) :-
%     atom_chars(null, Chars),
%     json_bool(Chars, Atom).
% test(is_null_rev, true(atom_chars(null, Chars))) :-
%     json_bool(Chars, json_null).

% test(is_true, true) :-
% test(is_null, true) :-

% test(parse1) :-
%     jsonparse('{"nome" : "Arthur", "cognome" : "Dent"}', O),
%     jsonaccess(O, ["nome"], R).

:- end_tests(json).