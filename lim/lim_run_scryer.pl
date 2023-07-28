:- use_module(library(assoc)).
:- use_module(library(iso_ext)). % for bb_get and bb_put
:- use_module(strings).

% Skip empty statements from blank lines and comments.
eval([]).

% This assigns a value to a variable.
eval(a(Name, Value)) :- lookup(Value, V), vtables_put(Name, V).

% This calls a function, but does not use its return value.
eval(c(Name, Args)) :- process_call(Name, Args).

% This stores a function definition in the current vtable.
eval(f(Name, Params, Stmts)) :- vtables_put(Name, [Params, Stmts]).

% This evaluates all the statements in a program.
eval(pr(Stmts)) :- maplist(eval, Stmts).

% This prints a value to stdout.
eval(p(Value)) :- lookup(Value, V), writeln(V).

% This stores a value being returned from a function
% so the caller can find it.  See "lookup(c...) below."
eval(r(Value)) :-
  lookup(Value, V),
  % Store the return value so caller can retrieve it.
  bb_put(return_, V).

% This calls a function and uses its return value.
lookup(c(Name, Args), V) :-
  process_call(Name, Args),
  bb_get(return_, V).

% This gets the value of a constant.
lookup(k(Value), Value).

% This evaluates a math expression.
lookup(m(Operator, LHS, RHS), Result) :-
  lookup(LHS, L),
  lookup(RHS, R),
  (
    Operator == (+) -> Result is L + R;
    Operator == (-) -> Result is L - R;
    Operator == (*) -> Result is L * R;
    Operator == (/) -> Result is L / R;
    writeln('lookup math: Operator not matched'),
    fail
  ).

% This gets a value from the vtables.
lookup(Name, Value) :- vtables_get(Name, Value).

% This adds a name/value pair to a vtable, creating a new vtable.
param_assign(Name, Value, VT0, VT1) :- put_assoc(Name, VT0, Value, VT1).

% This is used by both "eval(c...)" and "lookup(c...)"
% to process calling a function.
process_call(Name, Args) :-
  % Get the argument values.
  maplist(lookup, Args, Values),

  % Get the parameters and statements in the function.
  vtables_get(Name, [Params, Stmts]),

  % Assign all the argument values to the parameters.
  empty_assoc(VT0),
  foldl(param_assign, Params, Values, VT0, VT),

  % Add a new vtable to the front of the list
  % to hold local variables in this function call.
  bb_get(vtables, Vtables),
  bb_put(vtables, [VT | Vtables]),

  % Execute the statements.
  maplist(eval, Stmts),

  % Remove the vtable for this call.
  bb_get(vtables, [_|T]),
  bb_put(vtables, T).

% This reads a file produced by limc.
read_file(InFile, Program) :-
  open(InFile, read, Stream),
  read(Stream, Program),
  close(Stream).

% This runs the code in a file produced by limc.
% The file should contain a Prolog term
% that describes a program found in a .lim file.
run_file(InFile) :-
  read_file(InFile, Program),
  run_program(Program).

% This runs a lim program.
run_program(Program) :-
  % Create the top-level vtable.
  empty_assoc(Vtable),
  bb_put(vtables, [Vtable]),
  eval(Program).

% This gets the value of a given key in the
% first vtable found in the vtables list that defines it.
vtables_get(Key, Value) :-
  bb_get(vtables, Vtables),
  vtables_get_(Vtables, Key, Value).

% Theses are auxiliary rules used by vtables_get.
vtables_get_([], _, _) :- fail.
vtables_get_([H|T], Key, Value) :-
  (get_assoc(Key, H, V) ->
    Value = V;
    vtables_get_(T, Key, Value)
  ).

% This adds or updates a given key in the
% first vtable found in the vtables list.
vtables_put(Key, Value) :-
  bb_get(vtables, [H|T]),
  put_assoc(Key, H, Value, NewH),
  bb_put(vtables, [NewH|T]).

writeln(X) :- write(X), nl.

run :-
  % Get the first command-line argument which should be
  % a file path to the .limb file to run.
  argv([InFile|_]),
  run_file(InFile).
