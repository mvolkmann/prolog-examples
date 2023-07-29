% The fast_write and fast_read predicates are specific to SWI-Prolog.
%
:- initialization((
  File = "demo.pb",
  Data = foo(bar, baz),
  Options = [type(binary)],
  open(File, write, WStream, Options),
  fast_write(WStream, Data),
  close(WStream),
  open(File, read, RStream, Options),
  fast_read(RStream, NewData),
  close(RStream),
  format('NewData = ~w~n', NewData),
  halt
)).
