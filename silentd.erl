-module(silentd).
-export([server/1]).

server(Port) ->
  {ok,Socket} = gen_udp:open(Port,[binary, {active, true}]),
  io:format("~p~n", [Socket]),
  listen(Socket).

listen(Socket) ->
  receive
    {udp,Socket,Host,Port,Bin}=M ->
      Data = decode(Bin),
      io:format("responding to ~p~n",[M]),
      gen_udp:send(Socket, Host, Port, response(Data)),
      io:format("shutting down~p~n",[Data]),
      gen_udp:close(Socket)
  end.

response(Data) ->
 { ID, _QR, _OPCODE, TC, RD, QDCOUNT, ANCOUNT, NSCOUNT, ARCOUNT} = Data,
  <<ID:16, 1:1, 0:4, 0:1, 0:1, 0:1, 0:1, 0:3, 0:4,
  0:16/unsigned, 1:16/unsigned, 0:16/unsigned, 0:16/unsigned,

  11:8, "foo.bar.com", 0:8,
  0:8, 1:8,
  0:8, 1:8,
  3600:32/unsigned,
  4:16/unsigned,
  1:8, 1:8, 2:8, 2:8

  >>.



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
