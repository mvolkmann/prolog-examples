:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(strings).

run(InFile) :-
  format("run: InFile = ~w~n", [InFile]),
  open(InFile, read, Stream),
  read(Stream, P),
  format("run: P = ~w~n", [P]),
  close(Stream),
  execute(P).

% P = program([fn(multiply,[a,b],[assign(c,math(*,a,b)),return(c)]),assign(product,call(multiply,[2,3])),print(product)]),
execute(P) :-
  format("execute: P = ~w~n", [P]),
  bb_set(vtable, vtable{}),
  eval(P).

eval(assign(Name, Value)) :-
  format("eval assign: Name = ~w, Value = ~w~n", [Name, Value]),
  lookup(Value, V),
  format("eval assign: V = ~w~n", [V]),
  vtable_put(Name, V).

eval(call(Name, Args)) :-
  format("eval call: Name = ~w, Args = ~w~n", [Name, Args]),
  vtable_get(Name, Stmts),
  maplist(eval, Stmts).

eval(fn(Name, Args, Stmts)) :-
  format("eval fn: Name = ~w, Args = ~w~n", [Name, Args]),
  vtable_put(Name, Stmts).

eval(program(Stmts)) :-
  writeln('eval program'),
  maplist(eval, Stmts).

eval(print(Value)) :-
  format("eval print: Value = ~w~n", [Value]),
  lookup(Value, V),
  format("eval: V = ~w~n", [V]),
  writeln(V).

eval(return(Value)) :-
  format("eval return: Value = ~w~n", [Value]),
  vtable_get(stack_, Stack),
  vtable_put(stack_, [Value | Stack]).

lookup(call(_, _), Value) :-
  writeln('lookup call'),
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
  format("lookup const: Value = ~w~n", [Value]),
  V = Value.

lookup(math(Operator, LHS, RHS), Value) :-
  format("lookup math: Operator = ~w~n", [Operator]),
  format("lookup math: LHS = ~w~n", [LHS]),
  format("lookup math: RHS = ~w~n", [RHS]),
  lookup(LHS, L),
  lookup(RHS, R),
  format("math ~w ~w ~w~n", L, Operator, R),
  Value = 19. % TODO: Do the calculation.

lookup(Name, Value) :-
  format("lookup by name: Name = ~w~n", [Name]),
  vtable_get(Name, Value),
  format("lookup by name: Value = ~w~n", [value]).

vtable_get(Key, Value) :-
  format("vtable_get: Key = ~w~n", [Key]),
  bb_get(vtable, Vtable),
  format("vtable_get: Vtable = ~w~n", [Vtable]),
  Value = Vtable.get(Key),
  format("vtable_get: Value = ~w~n", [Value]).

vtable_put(Key, Value) :-
  bb_get(vtable, Vtable),
  NewVtable = Vtable.put(Key, Value),
  format("vtable_put: NewVtable = ~w~n", [NewVtable]),
  bb_set(vtable, NewVtable).

writeln(X) :- write(X), nl.

:- initialization((
  argv([InFile|_]),
  run(InFile),
  halt
)).
