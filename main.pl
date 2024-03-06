
% copyright: Frank Schwidom, 2024, License : GPL-3.0-only 

% including both libraries avoids autoload problems
:- use_module(library(apply)).
:- use_module(library(yall)).

mi_cutline_transform( REK, TERM1, TERM2) :- true
, mi_cutline_transform_rules( REK, TERM1, TERM2)
% , writeln( TERM2)
.

mi_cutline_transform_rules( _REK, !, !) :- ! .
mi_cutline_transform_rules( REK, (TI1, TI2), (TO1, TO2)) :- !
 , mi_cutline_transform_rules( REK, TI1, TO1)
 , mi_cutline_transform_rules( REK, TI2, TO2)
 .

mi_cutline_transform_rules( REK, (TI1; TI2), (TO1; TO2)) :- !
 , mi_cutline_transform_rules( REK, TI1, TO1)
 , mi_cutline_transform_rules( REK, TI2, TO2)
 .

mi_cutline_transform_rules( REK, TERM1, TERM2) :- !
 , TERM2=..[REK, TERM1]
 .


% this predicate must be filled with all predicates which may be interpreted by the meta interpreter
:- dynamic mi_clause_register/1 .

mi_clause_register(mi).
mi_clause_register(mi_cutline_transform).
mi_clause_register(mi_cutline_transform_rules).
mi_clause_register(p1).
mi_clause_register(p2).

mi_clause( HEAD, BODY) :- HEAD=..[P|_], mi_clause_register( P) -> clause(HEAD,BODY) ; false.

mi(true) :- !.

mi(TERM) :- ( TERM=(_,_) ; TERM=(_;_) ), !, mi_cutline_transform( mi, TERM, TERM2), call( TERM2).

mi(G) :- true 
 , findall( (G, B), mi_clause( G, B), L) 
 % , writeln(l-L)
 , ( false
    ; ( [] == L, call(G))
    ; ( [] \== L
     , F=G
     , reverse( L, L2)
     % , writeln(l2 - L2)
     , foldl( {F}/[(G, A),B,C]>>(C=(F=G,A;B)), L2, false, TERM) % possible autoload problems here
     % , writeln("###")
     % , writeln(TERM)
     , mi( TERM)
    )
   )
.

p1(a).
p1(b).

p2(a) :- !, false.
p2(b).

run_test( GOAL, RESULT, L1, L2) :- true
, findall( GOAL, GOAL, L1)
, findall( GOAL, mi( GOAL), L2)
% , findall( GOAL, mi( mi( GOAL)), L2) % works
% , findall( GOAL, mi( mi( mi( mi( mi( mi( GOAL)))))), L2) % works
, (L1 =@= L2 -> RESULT = true ; RESULT = false)
.

/*
(ins)?- wrap_term( mi, 0, true, RES).
RES = true.

(ins)?- wrap_term( mi, 3, true, RES).
RES = mi(mi(mi(true))).

*/

wrap_term( ATOM, DEPTH, PARAMETER, RESULT) :- 
( 0 == DEPTH -> RESULT = PARAMETER ; (
 DEPTH2 is DEPTH - 1, P2 =..[ATOM, PARAMETER], wrap_term( ATOM, DEPTH2, P2, RESULT)
))
.

run_test_with_depth( GOAL, DEPTH, RESULT, L1, L2) :- true
, findall( GOAL, GOAL, L1)
, wrap_term( mi, DEPTH, GOAL, GOAL2)
, findall( GOAL, GOAL2, L2)
, (L1 =@= L2 -> RESULT = true ; RESULT = false)
.

test001( CODE ) :- CODE= p1(_).
test002( CODE ) :- CODE= p2(_).
test003( CODE ) :- CODE= (A=1;A=2).
test004( CODE ) :- CODE= ((A=1;A=2),(B=1;B=2)).
test005( CODE ) :- CODE= ((A=1;A=2),!).
test006( CODE ) :- CODE= (!,(A=1;A=2)).
test007( CODE ) :- CODE= ((A=1;A=2),!,(B=1;B=2)).
test008( CODE ) :- CODE= (true,!,(B=1;B=2)).

run_all_tests :- true
, TESTS= [test001, test002, test003, test004, test005, test006, test007, test008]
, forall( ( member(TEST, TESTS), CALL=..[TEST,CODE])
   , (call( CALL), call(run_test(CODE, RES, _, _)), writeln( ( TEST, RES))))
.

run_all_tests_with_depth( DEPTH) :- true
, TESTS= [test001, test002, test003, test004, test005, test006, test007, test008]
, forall( ( member(TEST, TESTS), CALL=..[TEST,CODE])
   , (call( CALL), call(run_test_with_depth(CODE, DEPTH, RES, _, _)), writeln( ( TEST, RES))))
.

% mi( mi( mi, ...)) % 300 times
run_all_tests_with_depth_300 :- run_all_tests_with_depth(300).
