:- use_module(library(apply)). % for foldl
:- use_module(library(dcg/basics)).

% This assigns a value to a variable.
% The value can be a constant, math expression, or a function call.
eval(assign(Name, Value)) :-
  lookup(Value, V),
  vtables_put(Name, V).

% This kind of call does not assign its return value to anything.
eval(call(Name, Args)) :- process_call(Name, Args).

% This stores a function definition in the current vtable.
eval(fn(Name, Params, Stmts)) :- vtables_put(Name, [Params, Stmts]).

% This evaluates all the statements in a program.
eval(program(Stmts)) :- maplist(eval, Stmts).

% This gets a value (which can be a constant, math expression,
% or a function call) and prints it to stdout.
eval(print(Value)) :- lookup(Value, V), writeln(V).

% This stores a value being returned from a function
% so the caller can find it.  See "lookup(call...) below."
eval(return(Value)) :-
  lookup(Value, V),
  % Store the return value so caller can retrieve it.
  nb_setval(return_, V).

% This kind of call uses the return value,
% perhaps in an assignment or as a function argument.
lookup(call(Name, Args), V) :-
  process_call(Name, Args),
  nb_getval(return_, V).

% This gets the value of a constant.
lookup(const(Value), Value).

% This evaluates a math expression.
lookup(math(Operator, LHS, RHS), Result) :-
  lookup(LHS, L),
  lookup(RHS, R),
  ( Operator == '+' ->
    Result is L + R
  ; Operator == '-' ->
    Result is L - R
  ; Operator == '*' ->
    Result is L * R
  ; Operator == '/' ->
    Result is L / R
  ;
    fail
  ).

% This gets a value from the vtables.
lookup(Name, Value) :-
  vtables_get(Name, Value).

% This adds a name/value pair to a vtable, creating a new vtable.
param_assign(Name, Value, VT0, VT1) :-
  VT1 = VT0.put(Name, Value).

% This is used by both "eval(call...)" and "lookup(call...)"
% to process calling a function.
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

% This reads a binary file produced by limc.
read_file(InFile, Program) :-
  Options = [type(binary)],
  open(InFile, read, Stream, Options),
  fast_read(Stream, Program),
  close(Stream).

% This runs the code in a binary file produced by limc.
% The file should contain a binary representation of a Prolog term
% that describes a program found in a .lim file.
run_file(InFile) :-
  read_file(InFile, Program),
  run_program(Program).

% This runs a lim program.
run_program(Program) :-
  % Create the top-level vtable.
  nb_setval(vtables, [vtable{}]),
  eval(Program).

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

run :-
  % Get the first command-line argument which should be
  % a file path to the .limb file to run.
  current_prolog_flag(argv, [InFile|_]),
  run_file(InFile).
