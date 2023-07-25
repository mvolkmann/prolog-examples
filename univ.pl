:- initialization((
  Format = 'X=~w and Y=~w~n',
  Args = [1, 2],
  Goal =.. [format, Format, Args],
  call(Goal). % outputs "X=1 and Y=2
)).
