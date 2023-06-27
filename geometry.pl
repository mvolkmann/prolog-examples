:- use_module(library(clpr)).

% area(circle, Radius, X) :- X is pi * Radius^2.
area(circle, Radius, X) :- Pi is pi, {X = Pi * Radius^2}.

% area(square, Side, X) :- X is Side^2.
area(square, Side, X) :- {X = Side^2}.

% area(rectangle, Width, Height, X) :- X is Width * Height.
area(rectangle, Width, Height, X) :- {X = Width * Height}.

double(N, R) :- R is N * 2.

