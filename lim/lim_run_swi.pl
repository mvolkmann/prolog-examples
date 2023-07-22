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
  nb_setval(vtables, [vtable{}]),
  eval(P).

eval(assign(Name, Value)) :-
  format('eval assign: Name = ~w, Value = ~w~n', [Name, Value]),
  lookup(Value, V),
  format('eval assign: V = ~w~n', [V]),
  vtables_put(Name, V).

eval(call(Name, Args)) :-
  format('eval call: Name = ~w, Args = ~w~n', [Name, Args]),
  vtables_get(Name, Stmts),
  maplist(eval, Stmts).

eval(fn(Name, Args, Stmts)) :-
  format('eval fn: Name = ~w, Args = ~w~n', [Name, Args]),
  vtables_put(Name, Stmts).

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
  % Store the return value in the previous vtable.
  vtables_get(vtables, [_|T]),
  [Vtable|_] = T,
  Vtable.put(return_, Value).

lookup(call(_, _)) :-
  format('lookup call'),

  % Add a new vtable to the front of the list
  % to hold local variables in this function call.
  vtables_get(vtables, Vtables),
  NewVtable = vtable{},
  vtables_put(vtables, [NewVtable | Vtables]),

  % TODO: Call the function.

  % Remove the vtable for this call.
  vtables_get(vtables, [_|T]),
  vtables_put(vtables, T).

lookup(const(Value), V) :-
  format('lookup const: Value = ~w~n', [Value]),
  V = Value.

lookup(math(Operator, LHS, RHS), Value) :-
  format('lookup math: Operator = ~w~n', [Operator]),
  format('lookup math: LHS = ~w~n', [LHS]),
  format('lookup math: RHS = ~w~n', [RHS]),
  lookup(LHS, L),
  lookup(RHS, R),
  format('math ~w ~w ~w~n', [L, Operator, R]),
  Operator == '+' -> Value is L + R;
  Operator == '-' -> Value is L - R;
  Operator == '*' -> Value is L * R;
  Operator == '/' -> Value is L / R;
  Value = 0. % TODO: Throw an error.

lookup(Name, Value) :-
  format('lookup by name: Name = ~w~n', [Name]),
  vtables_get(Name, Value),
  format('lookup by name: Value = ~w~n', [value]).

% This gets the value of a given key in the
% first vtable found in the vtables list that defines it.
vtables_get(Key, Value) :-
  format('vtables_get: Key = ~w~n', [Key]),
  nb_getval(vtables, Vtables),
  format('vtables_get: Vtables = ~w~n', [Vtables]),
  vtables_get_(Vtables, Key, Value),
  format('vtables_get: Value = ~w~n', [Value]).

% Theses are auxiliary rules used by vtables_get.
vtables_get_([], _, _) :- fail.
vtables_get_([H|T], Key, Value) :-
  V = H.get(Key) -> Value = V; vtables_get(T, Key, Value).

% This adds or updates a given key in the
% first vtable found in the vtables list.
vtables_put(Key, Value) :-
  nb_getval(vtables, [H|T]),
  NewH = H.put(Key, Value),
  format('vtable_put: NewH = ~w~n', [NewH]),
  nb_setval(vtables, [NewH|T]).

:- initialization
  current_prolog_flag(argv, [InFile|_]),
  run(InFile),
  halt.
