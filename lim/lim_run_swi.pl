:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

run(InFile) :-
  Options = [type(binary)],
  open(InFile, read, Stream, Options),
  fast_read(Stream, P),
  close(Stream),
  execute(P).

% P = program([fn(multiply,[a,b],[assign(c,math(*,a,b)),return(c)]),assign(product,call(multiply,[2,3])),print(product)]),
execute(P) :-
  % format('execute: P = ~w~n', [P]),
  nb_setval(vtables, [vtable{}]),
  eval(P).

eval(assign(Name, Value)) :-
  lookup(Value, V),
  vtables_put(Name, V).

% This kind of call does not assign its return value to anything.
eval(call(Name, Args)) :-
  process_call(Name, Args).

eval(fn(Name, Params, Stmts)) :-
  vtables_put(Name, [Params, Stmts]).

eval(program(Stmts)) :-
  maplist(eval, Stmts).

eval(print(Value)) :-
  lookup(Value, V),
  writeln(V).

eval(return(Value)) :-
  lookup(Value, V),
  % Store the return value.
  nb_setval(return_, V).

% This kind of call uses the return value,
% perhaps in an assignment or as a function argument.
lookup(call(Name, Args), V) :-
  process_call(Name, Args),
  nb_getval(return_, V).

lookup(const(Value), Value).

lookup(math(Operator, LHS, RHS), Result) :-
  lookup(LHS, L),
  lookup(RHS, R),
  (
    Operator == '+' -> Result is L + R;
    Operator == '-' -> Result is L - R;
    Operator == '*' -> Result is L * R;
    Operator == '/' -> Result is L / R;
    Result = 0 % TODO: Throw an error.
  ).

lookup(Name, Value) :-
  vtables_get(Name, Value).

param_assign(Name, Value, VT0, VT1) :-
  VT1 = VT0.put(Name, Value).

process_call(Name, Args) :-
  % Get the argument values.
  maplist(lookup, Args, Values),

  % Get the parameters and statements in the function.
  vtables_get(Name, [Params, Stmts]),

  % Assign all the argument values to the parameters.
  VT0 = vtable{},
  foldl(param_assign, Params, Values, VT0, VT),

  % Add a new vtable to the front of the list
  % to hold local variables in this function call.
  nb_getval(vtables, Vtables),
  nb_setval(vtables, [VT | Vtables]),

  % Execute the statements.
  maplist(eval, Stmts),

  % Remove the vtable for this call.
  nb_getval(vtables, [_|T]),
  nb_setval(vtables, T).

% This gets the value of a given key in the
% first vtable found in the vtables list that defines it.
vtables_get(Key, Value) :-
  nb_getval(vtables, Vtables),
  vtables_get_(Vtables, Key, Value).

% Theses are auxiliary rules used by vtables_get.
vtables_get_([], _, _) :- fail.
vtables_get_([H|T], Key, Value) :-
  (V = H.get(Key) ->
    Value = V;
    vtables_get_(T, Key, Value)
  ).

% This adds or updates a given key in the
% first vtable found in the vtables list.
vtables_put(Key, Value) :-
  nb_getval(vtables, [H|T]),
  NewH = H.put(Key, Value),
  nb_setval(vtables, [NewH|T]).

:- initialization
  current_prolog_flag(argv, [InFile|_]),
  run(InFile),
  halt.
