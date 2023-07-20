write_all([]).
write_all([H|T]) :- writeln(H), write_all(T).
