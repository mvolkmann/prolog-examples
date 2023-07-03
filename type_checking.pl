demo(V, T) :- integer(V), T = 'integer', !.
demo(V, T) :- float(V), T = 'float', !.
demo(V, T) :- rational(V), T = 'rational', !.
% In SWI-Prolog, only double-quoted strings are considered strings.
demo(V, T) :- string(V), T = 'string', !.
demo(V, T) :- atom(V), T = 'atom', !.

demo2(V, T) :- number(V), T = 'number', !.

demo3(V, T) :- atomic(V), T = 'atomic', !.
demo3(V, T) :- compound(V), T = 'compound', !.

demo4(V, T) :- ground(V) -> T = 'ground'; T = 'nonground', !.
