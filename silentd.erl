-module(silentd).
-compile(export_all).

server(Port) ->
  {ok,Socket} = gen_udp:open(Port,[binary, {active, true}]),
  io:format("~p~n", [Socket]),
  listen(Socket).

listen(Socket) ->
  receive
    {udp,Socket,Host,Port,Bin} = M->
      io:format("req: ~p~n", [M]),
      Response = response(Bin),
      gen_udp:send(Socket, Host, Port, Response),
      io:format("shutting down~n"),
      gen_udp:close(Socket)
  end.

-define(QR_QUERY, 0).
-define(qrResponse, 1).

-define(opcodeQuery, 0).
-define(opcodeIQuery, 1).
-define(opcodeStatus, 2).

-define(authoritativeAnswer, 1).
-define(nonAuthoritativeAnswer, 0).

-define(noTruncation, 0).
-define(truncation, 1).

-define(noRecursionAvailable, 0).
-define(recursionAvailable, 1).

-define(rcodeNoError, 0).
-define(rcodeFormatError, 1).
-define(rcodeServerFailure, 2).
-define(rcodeNameError, 3).
-define(rcodeNotImplemented, 4).
-define(rcodeNotRefused, 5).

-define(testReq, <<195,103,1,0,0,1,0,0,0,0,0,0,3,102,111,111,3,98,97,114,3, 99,111,109,0,0,1,0,1>>).

response(Request) ->
  <<QHeader:12/bytes, QBody/bytes>> = Request,

  <<Id:16/unsigned, ?QR_QUERY:1, ?opcodeQuery:4,
  _AA:1, ?noTruncation:1, RecursionDesired:1, _RecursionAvailable:1, 0:3, _ResponseCode:4,
  QuestionCount:16/unsigned,
  AnswerCount:16/unsigned,
  NSCount:16/unsigned,
  ARCount:16/unsigned>> = QHeader,

 AHeader = make_header(Id, ?QR_QUERY, ?opcodeQuery,
   ?authoritativeAnswer, ?noTruncation, RecursionDesired, ?recursionAvailable,
   ?rcodeNoError, 0, 1, 0, 0),

 Name = <<"foo.bar.com">>,
 Address = <<1:8, 1:8, 2:8, 2:8>>,
 Type = 1,
 Class = 1,
 TTL = 3600,
 ABody = make_resource_record(Name, Type, Class, TTL, Address),
 <<AHeader/bytes, ABody/bytes>>.

make_header(Id, Qr, OpCode, AuthoritativeAnswer, Truncation, RecursionDesired, AuthoritativeAnswer, RCode, QCount, ACount, NSCount, ARCount) ->
  <<Id:16/unsigned,
  Qr:1, OpCode:4, AuthoritativeAnswer:1, Truncation:1, RecursionDesired:1, ?recursionAvailable:1, 0:3, RCode:4,
  QCount:16/unsigned, ACount:16/unsigned, NSCount:16/unsigned, ARCount:16/unsigned>>.

make_resource_record(Name, Type, Class, TTL, Data) ->
  NameLength = size(Name),
  DataLength = size(Data),
  <<NameLength:8, Name/bytes, 0:8,
  Type:16, Class:16, TTL:32/unsigned,
  DataLength:16/unsigned, Data/bytes>>.

d_qr(0) -> 'query';
d_qr(1) -> response.

d_opcode(0) -> standard;
d_opcode(1) -> inverse;
d_opcode(2) -> 'status';
d_opcode(X) when X =< 15 -> reserved.

decode(Data) ->
  {Header, MessageData} = header(Data),
  io:format("header: ~p~n",[Header]),
  { _, _, _, _, _, Qs, _As, _NSs, _ARs} = Header,
  Message = message(MessageData, Qs),
  io:format("message: ~p~n",[Message]),
  Header.

header(Data) ->
  <<ID:16, QR:1, OPCODE:4, _AA:1, TC:1, RD:1, _RA:1, 0:3, _RCODE:4,
  QDCOUNT:16/unsigned, ANCOUNT:16/unsigned, NSCOUNT:16/unsigned, ARCOUNT:16/unsigned,
  Message/bits>> = Data,
  Header = { ID, d_qr(QR), d_opcode(OPCODE), TC == 1, RD == 1, QDCOUNT, ANCOUNT, NSCOUNT, ARCOUNT},
  {Header, Message}.

message(Data, Qs) ->
  io:format("qname: ~p~n",[Data]),
  {QName, Data2} = take_q_name(Data, []),
  <<QTYPE:16, QCLASS:16>> = Data2,
  {QName, d_qtype(QTYPE), d_qclass(QCLASS)}.

take_q_name(Data, Names) ->
  <<Size:8/unsigned, Rest/bytes>> = Data,
  case Size of
    0 -> {Names, Rest};
    _ ->
      <<QName:Size/bytes, Rest2/bytes>> = Rest,
      NewName = Names ++ [ binary_to_list(QName) ],
      take_q_name(Rest2, NewName)
  end.

d_qtype(X) ->
  case X of
    1 -> a;
    2 -> ns;
    3 -> md;
    4 -> mf;
    5 -> cname;
    6 -> soa;
    7 -> mb;
    8 -> mg;
    9 -> mr;
    10 -> null;
    11 -> wks;
    12 -> ptr;
    13 -> hinfo;
    14 -> minfo;
    15 -> mx;
    16 -> txt;
    252 -> axfr;
    253 -> mailb;
    254 -> maila;
    255 -> '*'
  end.

d_qclass(X) ->
  case X of
    1 -> 'in';
    2-> cs;
    3-> ch;
    4-> hs;
    255 -> '*'
  end.
