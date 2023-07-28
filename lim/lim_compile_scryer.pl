:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(format)). % Why not picked up from ~/.scryerrc?
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(strings). % my module

compile(InFile, OutFile) :- 
  once(phrase_from_file(program(P), InFile)),
  format("P = ~w~n", [P]),
  % portray_clause adds a period at the end of the term output
  % which is required to read the term back in.
  phrase_to_file(portray_clause_(P), OutFile).

% This matches any single digit.
% The char_type predicate is defined in the charsio library.
digit(D) --> [D], { char_type(D, decimal_digit) }.

% This matches any non-empty list of digits.
digits([D|Ds]) --> digit(D), digits_(Ds).

% This matches any list of digits including an empty list.
digits_([D|Ds]) --> digit(D), digits_(Ds).
digits_([]) --> [].

% This matches any non-empty list of digits and converts it to an integer.
integer(I) --> digits(Ds), { number_chars(I, Ds) }.

eol --> "\r\n" | "\n".

letter(C) --> [C], { char_type(C, alpha) }.
lower(C) --> [C], { char_type(C, lower) }.
upper(C) --> [C], { char_type(C, upper) }.
letter_or_digit(C) --> letter(C) | digit(C).

assign(a(I, M)) -->
  id(I), ws, "=", ws, math(M),
  { format("assign: I = ~w, M = ~w~n", [I, M]) }.

assign(a(I, V)) -->
  id(I), ws, "=", ws, value(V),
  { format("assign: I = ~w, V = ~w~n", [I, V]) }.

call_args(Args) --> ws, value(V), ws, { Args = [V] }.
call_args(Args) -->
  ws, value(V), ws, ",", ws, call_args(Vs), { Args = [V|Vs] }.

not_eol(C) --> [C], { C \== '\n' }.
to_eol([C|Cs]) --> not_eol(C), to_eol(Cs).
to_eol([]) --> [].
comment([]) --> "#", to_eol(_).

constant(k(V)) --> integer(V).

def_args([]) --> ws.
def_args(Args) --> ws, id(A), ws, { Args = [A] }.
def_args(Args) -->
  ws, id(A), ws, ",", ws, def_args(As), { Args = [A|As] }.

fn_call(c(Name, Args)) -->
  id(Name), "(", call_args(Args), ")",
  { format("fn_call: Name = ~w, Args = ~w~n", [Name, Args]) }.

% To use this, enter something like the following:
% once(phrase(fn_def(F), `fn foo(a, b)\nc = a * b\nprint c\nend`)).
fn_def(f(Name, Args, Statements)) -->
  "fn ", id(Name), "(", def_args(Args), ")", ws, eol,
  statements(Statements),
  ws, "end",
  { format("fn_def: Op = ~w, V1 = ~w, V2 = ~w~n", [Name, Args, Statements]) }.

id_([]) --> [].
id_(I) --> letter_or_digit(C), id_(T), { I = [C|T] }.
id(I) --> letter(C), id_(T), { atom_chars(I, [C|T]) }.

math(m(Op, V1, V2)) -->
  value(V1), ws, operator(Op), ws, value(V2),
  { format("math: Op = ~w, V1 = ~w, V2 = ~w~n", [Op, V1, V2]) }.

operator(+) --> "+".
operator(-) --> "-".
operator(*) --> "*".
operator(/) --> "/".

print(p(V)) -->
  "print", ws, value(V),
  { format("print: V = ~w~n", [V]) }.

program(pr(Ss)) --> statements(Ss).

return(r(V)) -->
  "return ", value(V),
  { format("return: V = ~w~n", [V]) }.

statement(S) -->
  assign(S) | comment(S) | fn_call(S) | fn_def(S) | print(S) | return(S).
statement_line([]) --> ws, eol.
statement_line(S) --> ws, statement(S), ws, eol.
statements(Stmts) --> statement_line(S), { Stmts = [S] }.
statements(Stmts) --> statement_line(S), statements(Ss), { Stmts = [S|Ss] }.
% TODO: Remove underscores in statements list from comments.

% TODO: Adding math(V) to value(V) introduces left recursion
% TODO: which results in an endless loop!
value(V) --> constant(V) | id(V) | fn_call(V).
% value(V) --> constant(V) | id(V) | fn_call(V) | math(V).

% ws is zero or more whitespace characters such as a space, tab, or newline.
ws --> ws1, ws.
ws --> [].
% ws1 --> [C], { char_type(C, whitespace) }.
ws1 --> " ".

run :-
  argv([InFile|_]),
  phrase(filename_extension(Name, _), InFile),
  append(Name, ".limb", OutFile),
  compile(InFile, OutFile),
  format("created ~s~n", [OutFile]).
