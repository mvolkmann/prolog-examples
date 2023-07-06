report(P) :-
  format('Hello, ~w ~w!~n', [P.first, P.last]),
  format('I see you are ~w years old.~n', P.age),
  format('Your zip is ~w.~n', P.address.zip).

:- initialization
  P = person{
    first: 'Mark',
    last: 'Volkmann',
    age: 62,
    address: _{
      street: '123 Some Street',
      city: 'Somewhere',
      state: 'MO',
      zip: 12345
    }
  },
  report(P),
  halt.
