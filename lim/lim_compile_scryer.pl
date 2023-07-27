:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(format)). % Why not picked up from ~/.scryerrc?
:- use_module(library(pio)).
:- initialization(consult(strings)).

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

ws --> [C], { char_type(C, whitespace) }, ws.
ws --> [].

assignment(assign(I, V)) --> id(I), ws, "=", ws, value(V).
assignment(assign(I, M)) --> id(I), ws, "=", ws, math(M).

call_args(Args) --> ws, value(V), ws, { Args = [V] }.
call_args(Args) -->
  ws, value(V), ws, ",", ws, call_args(Vs), { Args = [V|Vs] }.

comment --> ws, "#", seq(_), eol.

constant(const(V)) --> integer(V).

def_args(Args) --> ws, id(A), ws, { Args = [A] }.
def_args(Args) -->
  ws, id(A), ws, ",", ws, def_args(As), { Args = [A|As] }.

fn_call(call(Name, Args)) --> id(Name), "(", call_args(Args), ")".
fn_call(call(Name, Args)) --> id(Name), "(", call_args(Args), ")".

% To use this, enter something like the following:
% once(phrase(fn_def(F), `fn foo(a, b)\nc = a * b\nprint c\nend`)).
fn_def(fn(Name, Args, Statements)) -->
  "fn ", id(Name), "(", def_args(Args), ")", ws,
  statements(Statements),
  ws, "end", ws.

id_([]) --> [].
id_(I) --> letter_or_digit(C), id_(T), { I = [C|T] }.
id([C|T]) --> letter(C), id_(T).

math(math(O, V1, V2)) --> value(V1), ws, operator(O), ws, value(V2).

operator(+) --> "+".
operator(-) --> "-".
operator(*) --> "*".
operator(/) --> "/".

print(print(V)) --> "print", ws, value(V).

% To use this, enter something like the following:
% once(phrase(program(P), `fn multiply(a, b)\n  c = a * b\n  return c\nend\nmultiply(2, 3)\nprint 6`)).
% once(phrase_from_file(program(P), "dcg4.txt")).
program(program(Ss)) --> statements(Ss). % , eol.

return(return(V)) --> "return ", value(V).

statement(S) -->
  assignment(S) | comment | fn_call(S) | fn_def(S) | print(S) | return(S).
statement_line(S) --> ws, statement(S), ws.
statements(Stmts) --> statement_line(S), { Stmts = [S] }.
statements(Stmts) --> statement_line(S), eol, statements(Ss), { Stmts = [S|Ss] }.
% TODO: Remove underscores in statements list from comments.

value(V) --> constant(V) | id(V) | fn_call(V).

% Example: compile('dcg4.txt', 'dcg4.pb').
compile(InFile, OutFile) :- 
  once(phrase_from_file(program(P), InFile)),
  % format('P = ~w~n', [P]),
  Options = [type(binary)],
  open(OutFile, write, Stream, Options),
  fast_write(Stream, P),
  close(Stream).

/*
:- initialization((
  '$toplevel':argv(Args),
  [InFile|_] = Args,
  phrase(filename_extension(Name, _), InFile),
  OutFile = [Name|".limb"],
  format("OutFile = ~w~n", [OutFile]),
  compile(InFile, OutFile),
  format('created ~w~n', OutFile),
  halt
)).
*/
