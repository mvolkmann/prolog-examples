:- initialization
  Thing = thing{
    id: obj{ key: 1}
  },
  Obj = Thing.get(id),
  % Make a copy of Obj.
  copy_term(Obj, NewObj),
  % Modify the copy destructively.
  nb_set_dict(key, NewObj, 2),
  % Verify that it was updated.
  writeln(NewObj.key), % 2
  % WHY DID THIS CHANGE ALSO?
  writeln(Obj.key), % 2
  halt.
