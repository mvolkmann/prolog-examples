:- use_module(library(format)).

demo :-
  Format = "X=~w and Y=~w~n",
  Args = [1, 2],
  call(format, Format, Args). % outputs "X=1 and Y=2
