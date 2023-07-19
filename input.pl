greet :-
  write('Enter your name: '),
  read_line_to_string(user_input, Name), % user must press return key
  format('Hello, ~w!~n', [Name]).


echo_keys :-
    with_tty_raw(echo_loop).

% ASCII code for carriage return generated by return key in macOS.
quit(13).

read_string(S0) :-
  get_single_char(Code),
  (quit(Code) ->
    !;   
    char_code(Atom, Code),
    write(Atom),
    append(S0, Atom, S1),
    read_string(S1)
  ).

/*
:- initialization
  greet,
  halt.
*/
