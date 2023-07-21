process_stream(end_of_file, _) :- !. % a "cut" that stops execution

process_stream(Char, Stream) :-
  write(Char),
  get_char(Stream, NextChar),
  process_stream(NextChar, Stream).

read_file(File) :- 
  open(File, read, Stream),
  get_char(Stream, Char),
  process_stream(Char, Stream),
  close(Stream).

write_file(File, Text) :- 
  open(File, write, Stream),
  write(Stream, Text),
  close(Stream).

my_goal :-
  writeln('line #1'),
  writeln('line #2').

:- initialization
  % open('demo.txt', write, Stream),
  % Stream = string(S),

  /*
  % With this approach, everything my_goal writes to stdout
  % is captured in the string S.
  with_output_to(string(S), my_goal),
  write(S),
  */

  new_memory_file(Handle), % creates the handle
  open_memory_file(Handle, write, Stream, [free_on_close(true)]), % opens a stream to the memory file
  writeln(Stream, 'line #1'),
  writeln(Stream, 'line #2'),
  close(Stream),
  memory_file_to_string(Handle, S),
  format('S = ~w~n', [S]).

  % halt.
