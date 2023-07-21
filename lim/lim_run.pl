% NOTE: It seems important for double_quotes to be set to
%       codes for the grammar rules in dcg/basics to work!
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

run(InFile) :-
  format('run: InFile = ~w~n', InFile),
  Options = [type(binary)],
  open(InFile, read, Stream, Options),
  fast_read(Stream, P),
  format('run: P = ~w~n', [P]),
  close(Stream),
  execute(P).

% P = program([fn(multiply,[a,b],[assign(c,math(*,a,b)),return(c)]),assign(product,call(multiply,[2,3])),print(product)]),
execute(P) :-
  format('execute: P = ~w~n', [P]),
  Vtable = _{},
  eval(Vtable, P).

eval(Vtable, assign(Name, Value)) :-
  format('assigning ~w to ~w~n', [Value, Name]),
  V = lookup(Value),
  Vtable.put(Name, V).

eval(Vtable, call(Name, Args)) :-
  format('calling function ~w with arguments ~w~n', [Name, Args]),
  Stmts = Vtable.get(Name),
  maplist(eval, Stmts).

eval(Vtable, fn(Name, Args, Stmts)) :-
  format('defining function ~w with arguments ~w~n', [Name, Args]),
  Vtable.put(Name, Stmts).

eval(Vtable, program(Stmts)) :-
  writeln('evaluating program'),
  maplist(eval(Vtable), Stmts).

eval(Vtable, print(Value)) :-
  lookup(Vtable, Value, V),
  format('printing ~w~n', [V]).

eval(Vtable, return(Value)) :-
  format('returning ~w~n', [Value]),
  Stack = Vtable.get(stack_, []),
  NewStack = [Value | Stack],
  Vtable.put(NewStack).

lookup(Vtable, call(_, _), Value) :-
  Stack = Vtable.get(stack_, []),
  length(Stack, Length),
  (Length == 0 ->
    writeln('stack is empty!'),
    Value = 0;
    % Pop the first item from the stack.
    [Value|Tail] = Stack,
    Vtable.put(stack_, Tail)
  ).

lookup(Vtable, id(Name), Value) :- Value = Vtable.get(Name).

lookup(_, integer(I), Value) :- Value = I.

lookup(Vtable, math(Operator, LHS, RHS), Value) :-
  lookup(Vtable, LHS, L),
  lookup(Vtable, RHS, R),
  format('math ~w ~w ~w~n', L, Operator, R),
  Value = 19. % TODO: Do the calculation.

:- initialization
  current_prolog_flag(argv, [InFile|_]),
  run(InFile),
  halt.
