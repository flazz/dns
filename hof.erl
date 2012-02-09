-module(hof).
-compile(export_all).

unfold(Fun, Seed) ->
  case Fun(Seed) of
    {} -> [];
    {X, NextSeed} -> [X | unfold(Fun, NextSeed)]
  end.
