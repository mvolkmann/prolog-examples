% This relates a string to the same string,
% but with the first letter capitalized.
capitalize(S0, S1) :-
  string_chars(S0, [H|T]),
  string_upper(H, U),
  atomics_to_string([U|T], S1).
  
% This relates a list of atoms to a sentence.
atoms_sentence(Atoms, Sentence) :-
  % Convert the list atoms into a list of strings.
  maplist(atom_string, Atoms, Strings),
  % Get the first word and a list of the remaining words.
  [W | Ws] = Strings,
  % Capitalize the first word.
  capitalize(W, C),
  % Join the words back into a single string
  % with a space between each word.
  atomics_to_string([C | Ws], ' ', S),
  % Add period at end.
  string_concat(S, ".", Sentence).

% This converts a list of atoms to a sentence
% and writes it to the current output stream.
write_sentence(Atoms) :-
  atoms_sentence(Atoms, Sentence),
  writeln(Sentence).

% Enter demo. to test this.
demo :-
  L = [
    [i, like, apples],
    [bananas, are, good, too],
    [cherries, can, be, tart]
  ],
  maplist(write_sentence, L).
