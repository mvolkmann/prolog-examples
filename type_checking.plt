% To run these tests, enter `swipl type-checking.plt`

:- consult(type_checking).
:- begin_tests(type_checking).

test(integer) :- demo(5, T), T == integer.

test(float) :- demo(1.2, T), T == float.

% See rational_syntax flag to enable 2/3 to be treated as rational.
test(rational) :- demo(2r3, T), T == rational.

test(number) :- demo2(1.2, T), T == number.

test(string) :- demo("hello", T), T == string.

test(atom) :- demo(a, T), T == atom.

test(atomic) :- demo3(true, T), T == atomic.
test(atomic) :- demo3(5, T), T == atomic.
test(atomic) :- demo3(1.2, T), T == atomic.
test(atomic) :- demo3("Hello", T), T == atomic.

test(compound) :- demo3(2 + 3, T), T == compound.
test(compound) :- demo3(a(b), T), T == compound.
test(compound) :- demo3(a-b, T), T == compound.
test(compound) :- demo3([a, b], T), T == compound.

test(ground) :- demo4(5, T), T == ground.
test(ground) :- V = 5, demo4(V, T), T == ground.

% The next line suppresses warning about singleton variable V.
:-style_check(-singleton).
test(ground) :- demo4(V, T), T \== ground.

:- end_tests(type_checking).
:- run_tests.
:- halt.
