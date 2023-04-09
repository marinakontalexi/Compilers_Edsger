let foo =
  symbol_push (Symbol("a", Type(Int, 0), None, !scope));
  symbol_push (Symbol("aa", Type(Int, 0), None, !scope));
  scope_add ();
  symbol_push (Symbol("b", Type(Int, 0), None, !scope))
  