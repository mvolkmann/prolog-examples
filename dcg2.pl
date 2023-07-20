/*
expr --> term.
expr --> term, operator, expr.

term --> [Num], {integer(Num)}.

operator --> [+].
operator --> [-].

parse(Input, Result) :-
    phrase(expr, Input, Result).

% Example 1: "2 + 3 - 1" should parse to [2,+,3,-,1]
% ?- parse([2,+,3,-,1], Result).
% Result = [2, +, 3, -, 1]

% Example 2: "5 - 2 + 8" should parse to [5,-,2,+,8]
% ?- parse([5,-,2,+,8], Result).
% Result = [5, -, 2, +, 8]
*/

/*
% DCG for parsing arithmetic expressions
% expr(Expr, Rest) parses an arithmetic expression, returning Expr and the remaining input Rest.

% Rule for a number
expr(Num) --> number(Num).

% Rule for an expression enclosed in parentheses
expr(Expr) --> "(", expr(Expr), ")".

% Rule for addition
expr(AddExpr) --> expr(Left), "+", expr(Right), {AddExpr is Left + Right}.

% Rule for multiplication
expr(MulExpr) --> expr(Left), "*", expr(Right), {MulExpr is Left * Right}.

% Rule for parsing a number (non-empty sequence of digits)
number(Num) --> digit(D), digits(Ds), {number_codes(Num, [D|Ds])}.

% Rules for parsing digits
digits([D|Ds]) --> digit(D), digits(Ds).
digits([]) --> [].

% Rule for parsing a single digit
digit(D) --> [D], {code_type(D, digit)}.

% Predicate to parse an expression
parse(Expression, Result) :-
    phrase(expr(Result), Expression).
*/

% Define the DCG rules
expression --> term, ("+" ; "-"), expression.
expression --> term.

term --> integer.
integer --> digit, integer.
integer --> digit.
digit --> [D], { code_type(D, digit) }.

% Helper predicate to parse the expression
parse_expression(Input, Expression) :-
    phrase(expression, Input, Expression).
