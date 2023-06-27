processStream(end_of_file, _) :- !. % a "cut" that stops execution

processStream(Char, Stream) :-
  write(Char),
  get_char(Stream, NextChar),
  processStream(NextChar, Stream).

readFile(File) :- 
  open(File, read, Stream),
  get_char(Stream, Char),
  processStream(Char, Stream),
  close(Stream).

writeFile(File, Text) :- 
  open(File, write, Stream),
  write(Stream, Text), nl,
  close(Stream).

