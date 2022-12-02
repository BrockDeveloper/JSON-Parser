:- begin_tests(json).
:- use_module(json).

test(jsonobj_access_empty_keys) :-
    jsonaccess(jsonobj(M), [], jsonobj(M)).
test(jsonarray_access_empty_keys, [fail]) :-
    jsonaccess(jsonarray(_), [], _).

test(assignment_example_1) :-
    jsonparse('{"nome" : "Arthur", "cognome" : "Dent"}', O),
    jsonaccess(O, ["nome"], R),
    assertion(O == jsonobj([("nome", "Arthur"), ("cognome", "Dent")])),
    assertion(R == "Arthur").

test(assignment_example_2) :-
    jsonparse('{"nome" : "Arthur", "cognome" : "Dent"}', O),
    jsonaccess(O, "nome", R),
    assertion(O == jsonobj([("nome", "Arthur"), ("cognome", "Dent")])),
    assertion(R == "Arthur").

test(assignment_example_3) :-
    jsonparse('{"nome" : "Zaphod",\n "heads" : ["Head1", "Head2"]}', Z),
    jsonaccess(Z, ["heads", 1], R),
    assertion(Z == jsonobj([("nome", "Zaphod"), ("heads", jsonarray(["Head1", "Head2"]))])),
    assertion(R == "Head2").

test(parse_gibberish, [fail]) :-
    jsonparse('[}', _).

test(parse_array_access_fail, [fail]) :-
    jsonparse('[1,2,3]', O),
    jsonaccess(O, [3], _).

test(partial_inst_1, [fail]) :-
    jsonparse('{"nome" : "Arthur", "cognome" : "Dent"}', jsonobj([jsonarray(_) | _])).
test(partial_inst_2) :-
    jsonparse('{"nome": "Arthur", "cognome": "Dent"}', jsonobj([("nome", N) | _])),
    assertion(N == "Arthur").
test(partial_inst_3) :-
    jsonparse('{"nome": "Arthur", "cognome": "Dent"}', J),
    jsonaccess(J, ["cognome"], R),
    assertion(R == "Dent").

test(jsondump) :-
    jsonparse('[1,2,3]', O),
    jsondump(O, 'test.json').

test(jsonread) :-
    jsonread('test.json', O),
    jsonparse('[1,2,3]', O).

test(jsonparse_empty_obj) :-
    jsonparse('{}', jsonobj([])),
    jsonparse(' {}', jsonobj([])),
    jsonparse('{ }', jsonobj([])),
    jsonparse(' { }', jsonobj([])).

test(jsonparse_empty_array) :-
    jsonparse('[]', jsonarray([])),
    jsonparse(' []', jsonarray([])),
    jsonparse('[ ]', jsonarray([])),
    jsonparse(' [ ]', jsonarray([])).

:- end_tests(json).