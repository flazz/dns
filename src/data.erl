-module(data).
-compile(export_all).

initial_data() ->
  Data = [
    {{<<"foo.bar.com">>, address, internet}, {3600, {1,1,2,2}}}
  ],
  dict:from_list(Data).

loop(Records) ->
  io:format("db: starting up~n"),

  receive
    {Pid, load} ->
      NewRecords = initial_data(),
      Pid ! ok,
      loop(NewRecords);

    %{Pid, insert, Key, Value} ->
      %NewRecords = dict:store(Key, Value, Records),
      %Pid ! ok,
      %loop(NewRecords);

    {Pid, get, Key} ->
      Msg = case dict:find(Key, Records) of
              {ok, Value} -> Value;
              error -> not_found
            end,
      Pid ! Msg,
      loop(Records);

    stop -> io:format("db: shutting down~n")
  end.
