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
-define(RCODE_REFUSED, 5).

-define(CLASS_IN, 1).
-define(CLASS_CS, 2).
-define(CLASS_CH, 3).
-define(CLASS_HS, 4).
-define(Q_CLASS_ANY, 255).

-define(TYPE_A, 1).
-define(TYPE_NS, 2).
-define(TYPE_MD, 3).
-define(TYPE_MF, 4).
-define(TYPE_CNAME, 5).
-define(TYPE_SOA, 6).
-define(TYPE_MB, 7).
-define(TYPE_MG, 8).
-define(TYPE_MR, 9).
-define(TYPE_NULL, 10).
-define(TYPE_WKS, 11).
-define(TYPE_PTR, 12).
-define(TYPE_HINFO, 13).
-define(TYPE_MINFO, 14).
-define(TYPE_MX, 15).
-define(TYPE_TXT, 16).
-define(Q_TYPE_AXFR, 252).
-define(Q_TYPE_MAILB, 253).
-define(Q_TYPE_MAILA, 254).
-define(Q_TYPE_ANY, 255).

-record(header, {id,
    qr, opcode, authoritative_answer, truncation, recursion_desired, recursion_available,
    rcode, qcount, acount, nscount, arcount}).

server(Port) ->
  DBase = spawn(data, loop, [[]]),
  DBase ! {self(), load},
  {ok,Socket} = gen_udp:open(Port,[binary, {active, true}]),
  io:format("dns: starting up: ~p~n", [Port]),
  listen(Socket, DBase),
  io:format("dns: shutting down~n"),
  gen_udp:close(Socket),
  DBase ! stop.

listen(Socket, DBase) ->
  receive
    {udp,Socket,Host,Port,Bin} ->
      Response = response(Bin, DBase),
      gen_udp:send(Socket, Host, Port, Response)
  end.

lookup(Name, TypeCode, ClassCode, DBase) ->

  Class = case ClassCode of
    ?CLASS_IN -> internet;
    ?CLASS_CS -> csnet;
    ?CLASS_CH -> chaos;
    ?CLASS_HS -> hesiod;
    ?Q_CLASS_ANY -> any
  end,

  Key = {Name, address, Class},

  DBase ! {self(), get, Key},

  receive
    {T, {A, B, C, D}} -> {T, <<A:8, B:8, C:8, D:8>>}
  end.

% TODO handle multiple questions
response(Request, DBase) ->
  <<QHeaderB:12/bytes, QBody/bytes>> = Request,

  % parse question header
  QHeader = parse_header(QHeaderB),
  valid_question_header(QHeader),
  {QName, QType, QClass} = parse_question_body(QBody),

  {TTL, Address} = lookup(QName, QType, QClass, DBase),

  AHeader = make_answer_header(QHeader),
  AHeaderB = serialize_header(AHeader),

  % make answer body
  ABodyB = serialize_resource_record(QName, QType, QClass, TTL, Address),
  <<AHeaderB/bytes, ABodyB/bytes>>.

parse_question_body(QBody) ->
  QNameSize = size(QBody) - 4,
  <<QName:QNameSize/bytes, QType:16, QClass:16>> = QBody,
  Name = qname_to_domain_name(QName),
  {Name, QType, QClass}.

valid_question_header(#header{qr = ?QR_QUERY, opcode = ?OPCODE_QUERY, truncation = ?TRUNCATION}) -> true;
valid_question_header(_) -> false.

make_answer_header(QHeader) ->
  QHeader#header{
    qr=?QR_RESPONSE,
    authoritative_answer=?AUTHORITATIVE_ANSWER,
    truncation=?NO_TRUNCATION,
    recursion_available=?RECURSION_AVAILABLE,
    rcode=?RCODE_NO_ERROR,
    qcount=0,
    acount=1,
    nscount=0,
    arcount=0
  }.

parse_header(Header) ->
  <<ID:16/unsigned, QR:1, OPCODE:4,
  AA:1, TC:1, RD:1, RA:1, 0:3, RCODE:4,
  QDCOUNT:16/unsigned,
  ANCOUNT:16/unsigned,
  NSCOUNT:16/unsigned,
  ARCOUNT:16/unsigned>> = Header,
  #header{
    id = ID,
    qr = QR,
    opcode = OPCODE,
    authoritative_answer = AA,
    truncation = TC,
    recursion_desired = RD,
    recursion_available = RA,
    rcode = RCODE,
    qcount = QDCOUNT,
    acount = ANCOUNT,
    nscount = NSCOUNT,
    arcount = ARCOUNT
  }.

make_header(Id, Qr, OpCode, AuthoritativeAnswer, Truncation, RecursionDesired, RecursionAvailable, RCode, QCount, ACount, NSCount, ARCount) ->
  <<Id:16/unsigned,
  Qr:1, OpCode:4, AuthoritativeAnswer:1, Truncation:1, RecursionDesired:1, RecursionAvailable:1, 0:3, RCode:4,
  QCount:16/unsigned, ACount:16/unsigned, NSCount:16/unsigned, ARCount:16/unsigned>>.

serialize_header(H) ->
  <<
  (H#header.id):16/unsigned,
  (H#header.qr):1,
  (H#header.opcode):4,
  (H#header.authoritative_answer):1,
  (H#header.truncation):1,
  (H#header.recursion_desired):1,
  (H#header.recursion_available):1,
  0:3,
  (H#header.rcode):4,
  (H#header.qcount):16/unsigned,
  (H#header.acount):16/unsigned,
  (H#header.nscount):16/unsigned,
  (H#header.arcount):16/unsigned
  >>.

serialize_resource_record(Name, Type, Class, TTL, Data) ->
  NameLength = size(Name),
  DataLength = size(Data),
  <<NameLength:8, Name/bytes, 0:8,
  Type:16, Class:16, TTL:32/unsigned,
  DataLength:16/unsigned, Data/bytes>>.

qname_to_domain_name(QName) ->
  Labels = hof:unfold(fun silentd:take_label/1, QName),
  NameString = string:join(Labels, "."),
  list_to_binary(NameString).

% TODO put this inside qname_to_domain_name
take_label(<<0:8>>) -> {};
take_label(<<Size:8, RestWithLabel/bytes>>) ->
  <<Label:Size/bytes, Rest/bytes>> = RestWithLabel,
  { binary_to_list(Label), Rest }.
