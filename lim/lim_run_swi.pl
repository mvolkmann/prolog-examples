:- use_module(library(apply)).
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

% This kind of call does not assign its return value to anything.
eval(call(Name, Args)) :-
  format('eval call: Name = ~w, Args = ~w~n', [Name, Args]),
  vtables_get(Name, [Params, Stmts]),
  maplist(eval, Stmts).

eval(fn(Name, Params, Stmts)) :-
  format('eval fn: Name = ~w, Params = ~w~n', [Name, Params]),
  vtables_put(Name, [Params, Stmts]).

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

param_assign(VT0, Name, Value, VT1) :-
  format('param_assign: Name = ~w, Value = ~w~n', [Name, Value]),
  VT1 = VT0.put(Name, Value).

% This kind of call uses the return value,
% perhaps in an assignment or as a function argument.
lookup(call(Name, Args)) :-
  format('lookup call: Name = ~w~n', [Name]),
  format('lookup call: Args = ~w~n', [Args]),

  % Add a new vtable to the front of the list
  % to hold local variables in this function call.
  vtables_get(vtables, Vtables),
  VT0 = vtable{},

  % Get the argument values.
  maplist(lookup, Args, Values),
  format('lookup call: Values = ~w~n', [Values]),

  % Get the parameters and statements in the function.
  vtables_get(Name, [Params|Stmts]),

  % Assign all the argument values to the parameters.
  foldl(param_assign(VT0), Params, Values, VT),
  format('lookup call: VT = ~w~n', [VT]),
  vtables_put(vtables, [VT | Vtables]),

  % Execute the statements.
  maplist(eval, Stmts),

  % Remove the vtable for this call.
  vtables_get(vtables, [_|T]),
  vtables_put(vtables, T).

lookup(const(Value), V) :-
  format('lookup const: Value = ~w~n', [Value]),
  V = Value.

lookup(math(Operator, LHS, RHS), Result) :-
  lookup(LHS, L),
  lookup(RHS, R),
  format('lookup math: ~w ~w ~w~n', [L, Operator, R]),
  (
    Operator == '+' -> Result is L + R;
    Operator == '-' -> Result is L - R;
    Operator == '*' -> Result is L * R;
    Operator == '/' -> Result is L / R;
    Result = 0 % TODO: Throw an error.
  ),
  format('lookup math: result = ~w~n', [Result]).

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
