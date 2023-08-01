greet(Name) :- format('Hello, ~w!', [Name]).

greet :-
  write("Enter your name: "),
  read(Name), % enter an atom with period at end
  format("Hello, ~w!", [Name]).

