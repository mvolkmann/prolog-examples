greet(Name) :- format('Hello, ~w!', [Name]).

greet :-
  write("Enter your name: "),
  read(Name),
  format("Hello, ~w!", [Name]).

