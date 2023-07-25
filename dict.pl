report(P) :-
  format('Hello, ~w ~w!~n', [P.first, P.last]),
  format('I see you are ~w years old.~n', P.age),
  format('Your zip is ~w.~n', P.address.zip),
  Key = age,
  Value = P.get(Key, 0),
  format('key ~w = ~w~n', [Key, Value]).

:- initialization((
  P = person{
    first: 'Mark',
    last: 'Volkmann',
    age: 62,
    % value: 19,
    address: _{
      street: '123 Some Street',
      city: 'Somewhere',
      state: 'MO',
      zip: 12345
    }
  },
  dict_keys(P, Keys),
  format('Keys = ~w~n', [Keys]),
  report(P),
  Value = P.get(value, 0),
  (Value > 0 ->
    format('Value = ~w~n', [Value]);
    writeln('no value found')
  ),
  halt
)).
