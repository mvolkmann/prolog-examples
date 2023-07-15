:- initialization
  Things = things{
    t1: thing{
      id: obj{ key: 1 }
    }
  },
  Thing = Things.get(t1),
  Obj = Thing.get(id),
  copy_term(Obj, NewObj),
  nb_set_dict(key, NewObj, 2),
  format('Obj.key = ~w~n', [Obj.key]),
  format('NewObj.key = ~w~n', [NewObj.key]),
  halt.
