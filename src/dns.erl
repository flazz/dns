-module(dns).
-compile(export_all).

-include("include/dns_values.hrl").

server(Port) ->
  DBase = spawn(data, loop, [[]]),
  DBase ! {self(), load},
  receive
    ok -> ok;
    Other -> error(Other)
  end,
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

  Answers = answer_questions(Questions, DBase),

  AHeaderB = make_answer_header(QHeader, QHeader#header.qcount, length(Answers)),
  QuestionsB = [ serialize:question(Q) || Q <- Questions ],
  AnswersB = Answers,

  list_to_binary([AHeaderB] ++ QuestionsB ++ AnswersB).

answer_questions(Questions, DBase) ->
  Loopkups = [ {Q, lookup(Name, Type, Class, DBase)} || {Name, Type, Class} = Q <- Questions ],

  Pred = fun
    ({_, not_found}) -> false;
    (_) -> true
  end,

  XAnswers = lists:filter(Pred, Loopkups),
  Answers = [ serialize:resource_record(Name, Type, Class, Ttl, serialize:address(Address)) ||
    {{Name, Type, Class}, {Ttl , Address}} <- XAnswers],
  Answers.

assemble_answers({{Name, Type, Class}, {Ttl, Address}}) ->
  Data = serialize:address(Address),
  serialize:resource_record(Name, Type, Class, Ttl, Data).

class_from_code(?CLASS_IN) -> internet;
class_from_code(?CLASS_CS) -> csnet;
class_from_code(?CLASS_CH) -> chaos;
class_from_code(?CLASS_HS) -> hesiod;
class_from_code(?Q_CLASS_ANY) -> any.

lookup(Name, TypeCode, ClassCode, DBase) ->
  Class = class_from_code(ClassCode),
  Key = {Name, address, Class},
  DBase ! {self(), get, Key},
  receive Any -> Any end.

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
