% NOTE: It seems important for double_quotes to be set to
%       codes for the grammar rules in dcg/basics to work!
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

% From dcg/basics:
% digit(C) --> [C], { code_type(C, digit) }.
letter(C) --> [C], { code_type(C, alpha) }.
lower(C) --> [C], { code_type(C, lower) }.
upper(C) --> [C], { code_type(C, upper) }.
letter_or_digit(C) --> letter(C) | digit(C).

assignment(assign(I, V)) --> identifier(I), ws, "=", ws, value(V).
assignment(assign(I, M)) --> identifier(I), ws, "=", ws, math(M).

call_args(Args) --> ws, value(V), ws, { Args = [V] }.
call_args(Args) -->
  ws, value(V), ws, ",", ws, call_args(Vs), { Args = [V|Vs] }.

comment --> whites, "#", string_without("\n", _).

def_args(Args) --> ws, identifier(A), ws, { Args = [A] }.
def_args(Args) -->
  ws, identifier(A), ws, ",", ws, def_args(As), { Args = [A|As] }.

fn_call(call(Name, Args)) --> identifier(Name), "(", call_args(Args), ")".
fn_call(call(Name, Args)) --> identifier(Name), "(", call_args(Args), ")".

% To use this, enter something like the following:
% once(phrase(fn_def(F), "fn foo(a, b)\nc = a * b\nprint c\nend")).
fn_def(fn(Name, Args, Statements)) -->
  "fn ", identifier(Name), "(", def_args(Args), ")", ws,
  statements(Statements),
  ws, "end", ws.

identifier_([]) --> [].
% identifier_(id(I)) --> letter_or_digit(C), identifier_(T), { I = [C|T] }.
% identifier(id(I)) --> letter(C), identifier_(T), { atom_codes(I, [C|T]) }.
identifier_(I) --> letter_or_digit(C), identifier_(T), { I = [C|T] }.
identifier(I) --> letter(C), identifier_(T), { atom_codes(I, [C|T]) }.

math(math(O, V1, V2)) --> value(V1), ws, operator(O), ws, value(V2).

operator(+) --> "+".
operator(-) --> "-".
operator(*) --> "*".
operator(/) --> "/".

print(print(V)) --> "print", ws, value(V).

% To use this, enter something like the following:
% once(phrase(program(P), "fn multiply(a, b)\n  c = a * b\n  return c\nend\nmultiply(2, 3)\nprint 6")).
% once(phrase_from_file(program(P), "dcg4.txt")).
program(program(Ss)) --> statements(Ss), eol.

return(return(V)) --> "return ", value(V).

statement(S) -->
  assignment(S) | comment | fn_call(S) | fn_def(S) | print(S) | return(S).
statement_line(S) --> whites, statement(S), whites.
statements(Stmts) --> statement_line(S), { Stmts = [S] }.
statements(Stmts) --> statement_line(S), eol, statements(Ss), { Stmts = [S|Ss] }.
% TODO: Remove underscores in statements list from comments.

value(V) --> identifier(V) | integer(V) | fn_call(V).

% white is a space or tab.
% eol is \n, \r\n, or end of input.
ws1 --> white | eol.
% ws matches zero or more ws1 characters.
ws --> [].
ws --> ws1, ws.

% Example: compile('dcg4.txt', 'dcg4.pb').
compile(InFile, OutFile) :- 
  once(phrase_from_file(program(P), InFile)),
  % format('P = ~w~n', [P]),
  Options = [type(binary)],
  open(OutFile, write, Stream, Options),
  fast_write(Stream, P),
  close(Stream).

:- initialization
  current_prolog_flag(argv, Argv),
  [InFile|_] = Argv,
  split_string(InFile, '.', '', [Name|_]),
  string_concat(Name, '.limb', OutFile),
  compile(InFile, OutFile),
  format('created ~w~n', OutFile),
  halt.