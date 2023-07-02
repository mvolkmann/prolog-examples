:- use_module(library(clpr)).

area(circle, Radius, X) :- Pi is pi, {X = Pi * Radius^2}.

% area(square, Side, X) :- X is Side^2.
area(square, Side, X) :- {X = Side^2}.

% area(rectangle, Width, Height, X) :- X is Width * Height.
area(rectangle, Width, Height, X) :- {X = Width * Height}.

% This is an alternate implementation from @sifb
% in the Prolog Discord channel on 6/27/23.
radius_area(R, A) :-
    ground(R), % tests whether R is not a free variable
    A is pi * R^2.

radius_area(R, A) :-
    ground(A), % tests whether A is not a free variable
    R is sqrt(A / pi).


