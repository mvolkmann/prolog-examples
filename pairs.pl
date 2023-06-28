
queen_can_attack((R, _), (R, _)).
queen_can_attack((_, C), (_, C)).
queen_can_attack((R1, C1), (R2, C2)) :-
  abs(R1 - R2) =:= abs(C1 - C2).
