-module(silentd).
-compile(export_all).

-define(QR_QUERY, 0).
-define(QR_RESPONSE, 1).

-define(OPCODE_QUERY, 0).
-define(OPCODE_IQUERY, 1).
-define(OPCODE_STATUS, 2).

-define(AUTHORITATIVE_ANSWER, 1).
-define(NON_AUTHORITATIVE_ANSWER, 0).

-define(NO_TRUNCATION, 0).
-define(TRUNCATION, 1).

-define(NO_RECURSION_AVAILABLE, 0).
-define(RECURSION_AVAILABLE, 1).

-define(RCODE_NO_ERROR, 0).
-define(RCODE_FORMAT_ERROR, 1).
-define(RCODE_SERVER_FAILURE, 2).
-define(RCODE_NAME_ERROR, 3).
-define(RCODE_NOT_IMPLEMENTED, 4).
-define(RCODE_NOT_REFUSED, 5).

server(Port) ->
  DBase = spawn(fun dbloop/0),
  {ok,Socket} = gen_udp:open(Port,[binary, {active, true}]),
  io:format("~p~n", [Socket]),
  listen(Socket, DBase).

listen(Socket, DBase) ->
  receive
    {udp,Socket,Host,Port,Bin} ->
      Response = response(Bin, DBase),
      gen_udp:send(Socket, Host, Port, Response),
      io:format("shutting down~n"),
      gen_udp:close(Socket),
      DBase ! stop
  end.

dbloop() ->
  receive
    stop -> done;

    {Pid, "foo.bar.com", address, internet} ->
      io:format("lookup~n"),
      Pid ! {1,1,2,2},
      dbloop();

    {Pid, _Address, _Type, _Class} ->
      Pid ! not_found
  end.

response(Request, DBase) ->
  <<QHeader:12/bytes, QBody/bytes>> = Request,

  % parse question header
  <<Id:16/unsigned, ?QR_QUERY:1, ?OPCODE_QUERY:4,
  _AA:1, ?NO_TRUNCATION:1, RecursionDesired:1, _RecursionAvailable:1, 0:3, _ResponseCode:4,
  QuestionCount:16/unsigned,
  _AnswerCount:16/unsigned,
  _NSCount:16/unsigned,
  _ARCount:16/unsigned>> = QHeader,

  % parse question body
  QNameSize = size(QBody) - 4,
  <<QName:QNameSize/bytes, QType:16, QClass:16>> = QBody,
  io:format("q name: ~p~n",[QName]),

  DBase ! {self(), qname_to_domain_name(QName), address, internet},

  Address = receive
    {A, B, C, D} -> <<A:8, B:8, C:8, D:8>>
  end,

  % make answer header
  AHeader = make_header(Id, ?QR_QUERY, ?OPCODE_QUERY,
    ?AUTHORITATIVE_ANSWER, ?NO_TRUNCATION, RecursionDesired, ?RECURSION_AVAILABLE,
    ?RCODE_NO_ERROR, 0, 1, 0, 0),

  Name = <<"foo.bar.com">>,
  Address = <<1:8, 1:8, 2:8, 2:8>>,
  Type = QType,
  Class = QClass,
  TTL = 3600,

  % make answer body
  ABody = make_resource_record(Name, Type, Class, TTL, Address),
  <<AHeader/bytes, ABody/bytes>>.

make_header(Id, Qr, OpCode, AuthoritativeAnswer, Truncation, RecursionDesired, AuthoritativeAnswer, RCode, QCount, ACount, NSCount, ARCount) ->
  <<Id:16/unsigned,
  Qr:1, OpCode:4, AuthoritativeAnswer:1, Truncation:1, RecursionDesired:1, ?RECURSION_AVAILABLE:1, 0:3, RCode:4,
  QCount:16/unsigned, ACount:16/unsigned, NSCount:16/unsigned, ARCount:16/unsigned>>.

make_resource_record(Name, Type, Class, TTL, Data) ->
  NameLength = size(Name),
  DataLength = size(Data),
  <<NameLength:8, Name/bytes, 0:8,
  Type:16, Class:16, TTL:32/unsigned,
  DataLength:16/unsigned, Data/bytes>>.

qname_to_domain_name(QName) ->
  Labels = unfold(fun silentd:take_label/1, QName),
  string:join(Labels, ".").

unfold(Fun, Seed) ->
  case Fun(Seed) of
    {} -> [];
    {X, NextSeed} -> [X | unfold(Fun, NextSeed)]
  end.

take_label(<<0:8>>) -> {};
take_label(<<Size:8, RestWithLabel/bytes>>) ->
  <<Label:Size/bytes, Rest/bytes>> = RestWithLabel,
  { binary_to_list(Label), Rest }.
