% NOTE: It seems important for double_quotes to not be set to
%       chars for the grammar rules in dcg/basics to work!
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

run(InFile) :-
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
  format('eval assign: Name = ~w, Value = ~w~n', [Value, Name]),
  V = lookup(Value),
  format('eval assign: V = ~w~n', [Value]),
  Vtable.put(Name, V).

eval(Vtable, call(Name, Args)) :-
  format('eval call: Name = ~w, Args = ~w~n', [Name, Args]),
  Stmts = Vtable.get(Name),
  maplist(eval, Stmts).

eval(Vtable, fn(Name, Args, Stmts)) :-
  format('eval fn: Name = ~w, Args = ~w~n', [Name, Args]),
  Vtable.put(Name, Stmts).

eval(Vtable, program(Stmts)) :-
  writeln('eval program'),
  maplist(eval(Vtable), Stmts).

eval(Vtable, print(Value)) :-
  format('eval print: Value = ~w~n', [Value]),
  lookup(Vtable, Value, V),
  format('eval: V = ~w~n', [V]),
  writeln(V).

eval(Vtable, return(Value)) :-
  format('eval return: Value = ~w~n', [Value]),
  Stack = Vtable.get(stack_, []),
  NewStack = [Value | Stack],
  Vtable.put(NewStack).

lookup(Vtable, call(_, _), Value) :-
  format('lookup call'),
  Stack = Vtable.get(stack_, []),
  length(Stack, Length),
  (Length == 0 ->
    writeln('stack is empty!'),
    Value = 0;
    % Pop the first item from the stack.
    [Value|Tail] = Stack,
    Vtable.put(stack_, Tail)
  ).

lookup(Vtable, id(Name), Value) :-
  format('lookup id: Name = ~w~n', [Name]),
  Value = Vtable.get(Name).

% lookup(_, integer(I), Value) :-
lookup(_, V, Value) :-
  format('lookup generic: V = ~w~n', [V]),
  Value = V.

lookup(Vtable, math(Operator, LHS, RHS), Value) :-
  format('lookup math: Operator = ~w~n', [Operator]),
  format('lookup math: LHS = ~w~n', [LHS]),
  format('lookup math: RHS = ~w~n', [RHS]),
  lookup(Vtable, LHS, L),
  lookup(Vtable, RHS, R),
  format('math ~w ~w ~w~n', L, Operator, R),
  Value = 19. % TODO: Do the calculation.

:- initialization
  current_prolog_flag(argv, [InFile|_]),
  run(InFile),
  halt.
