% THIS IS A WORK IN PROGRESS!
% See http://csci.viu.ca/~wesselsd/courses/csci330/code/prolog/grammars.html
:- use_module(library(dcg/basics)).

math_operator(op(plus)) --> [plus].
math_operator(op(times)) --> [times].

variable(v(a)) --> [a].
variable(v(b)) --> [b].
variable(v(c)) --> [c].
% variable(v(V)) --> atom(V).

expression(expr(V)) --> variable(V).
expression(expr(Op, L, R)) --> expression(L), math_operator(Op), expression(R).

/*
add(a(O1, O2)) --> operand(O1), [+], operand(O2).
multiply(m(O1, O2)) --> operand(O1), [*], operand(O2).

operand(o(O)) --> integer(O).
operand(o(O)) --> add(O).
operand(o(O)) --> multiply(O).

formula(f(F)) --> add(F).
formula(f(F)) --> multiply(F).
*/

