-module(dns).
-compile(export_all).

-include("include/dns_values.hrl").

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

response(Request, DBase) ->
  <<QHeaderB:12/bytes, QBody/bytes>> = Request,

  QHeader = parse:question_header(QHeaderB),
  Questions = parse:questions(QBody, QHeader#header.qcount),

  % TODO filter questions that are not answered, replace them with auth?
  Answers = [ answer_question(Q, DBase) || Q <- Questions ],

  QCount = length(Questions),
  ACount = length(Answers),
  AHeaderB = make_answer_header(QHeader, QCount, ACount),

  QuestionsB = list_to_binary([ serialize:question(Q) || Q <- Questions ]),
  AnswersB = list_to_binary(Answers),
  <<AHeaderB/bytes, QuestionsB/bytes, AnswersB/bytes>>.

answer_question({Name, Type, Class}, DBase) ->
  {TTL, Address} = lookup(Name, Type, Class, DBase),
  serialize:resource_record(Name, Type, Class, TTL, Address).

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

make_answer_header(QHeader, QCount, ACount) ->
  AnswerHeader = QHeader#header{
    qr=?QR_RESPONSE,
    authoritative_answer=?AUTHORITATIVE_ANSWER,
    truncation=?NO_TRUNCATION,
    recursion_available=?RECURSION_AVAILABLE,
    rcode=?RCODE_NO_ERROR,
    qcount=QCount,
    acount=ACount,
    nscount=0,
    arcount=0
  },
  serialize:header(AnswerHeader).

make_header(Id, Qr, OpCode, AuthoritativeAnswer, Truncation, RecursionDesired, RecursionAvailable, RCode, QCount, ACount, NSCount, ARCount) ->
  <<Id:16/unsigned,
  Qr:1, OpCode:4, AuthoritativeAnswer:1, Truncation:1, RecursionDesired:1, RecursionAvailable:1, 0:3, RCode:4,
  QCount:16/unsigned, ACount:16/unsigned, NSCount:16/unsigned, ARCount:16/unsigned>>.
