:- use_module(library(dcgs)).

demo1 -->
  "start ",
  "middle",
  " end".

demo2 -->
  "start ",
  {
    Var = "middle"
  },
  seq(Var),
  " end".
