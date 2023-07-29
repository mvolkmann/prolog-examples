% :- use_module(library(charsio)).
:- use_module(library(format)).

get_line_to_chars(Stream, Cs0) :-
  get_line_to_chars_(Stream, Cs0, []).

get_line_to_chars_(Stream, Cs0, Cs) :-
  '$get_n_chars'(Stream, 1, Char),
  ( Char == [] ->
    Cs0 = Cs
  ; Char = [C],
    ( C == '\n' ->
      Rest = Cs
    ; Cs0 = [C|Rest],
      get_line_to_chars_(Stream, Rest, Cs)
    )
  ).

demo :-
  repeat,
    write('Enter name: '),
    % read_line_to_string(user_input, Name), % user must press return key
    get_line_to_chars(user_input, Name),

    format("Hello, ~s!~n", [Name]),
  !.

