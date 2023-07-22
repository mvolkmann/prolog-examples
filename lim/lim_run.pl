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
  nb_setval(vtable, vtable{}),
  eval(P).

eval(assign(Name, Value)) :-
  format('eval assign: Name = ~w, Value = ~w~n', [Name, Value]),
  lookup(Value, V),
  format('eval assign: V = ~w~n', [V]),
  vtable_put(Name, V).

eval(call(Name, Args)) :-
  format('eval call: Name = ~w, Args = ~w~n', [Name, Args]),
  vtable_get(Name, Stmts),
  maplist(eval, Stmts).

eval(fn(Name, Args, Stmts)) :-
  format('eval fn: Name = ~w, Args = ~w~n', [Name, Args]),
  vtable_put(Name, Stmts).

eval(program(Stmts)) :-
  writeln('eval program'),
  maplist(eval, Stmts).

eval(print(Value)) :-
  format('eval print: Value = ~w~n', [Value]),
  lookup(Value, V),
  format('eval: V = ~w~n', [V]),
  writeln(V).

eval(return(Value)) :-
  format('eval return: Value = ~w~n', [Value]),
  vtable_get(stack_, Stack),
  vtable_put(stack_, [Value | Stack]).

lookup(call(_, _), Value) :-
  format('lookup call'),
  vtable_get(stack_, Stack),
  length(Stack, Length),
  (Length == 0 ->
    writeln('stack is empty!'),
    Value = 0;
    % Pop the first item from the stack.
    [Value|Tail] = Stack,
    vtable_put(stack_, Tail)
  ).

lookup(const(Value), V) :-
  format('lookup const: Value = ~w~n', [Value]),
  V = Value.

lookup(id(Name), Value) :-
  format('lookup id: Name = ~w~n', [Name]),
  vtable_get(Name, Value).

lookup(math(Operator, LHS, RHS), Value) :-
  format('lookup math: Operator = ~w~n', [Operator]),
  format('lookup math: LHS = ~w~n', [LHS]),
  format('lookup math: RHS = ~w~n', [RHS]),
  lookup(LHS, L),
  lookup(RHS, R),
  format('math ~w ~w ~w~n', L, Operator, R),
  Value = 19. % TODO: Do the calculation.

vtable_get(Key, Value) :-
  nb_getval(vtable, Vtable),
  Vtable.get(Key, Value).

vtable_put(Key, Value) :-
  nb_getval(vtable, Vtable),
  nb_setval(vtable, Vtable.put(Key, Value)).

:- initialization
  current_prolog_flag(argv, [InFile|_]),
  run(InFile),
  halt.
