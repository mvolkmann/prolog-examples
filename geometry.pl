area(circle, Radius, X) :- X is pi * Radius^2.
area(square, Side, X) :- X is Side^2.
area(rectangle, Width, Height, X) :- X is Width * Height.

