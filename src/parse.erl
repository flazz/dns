-module(parse).
-compile(export_all).

-include("include/dns_values.hrl").

request(Request) ->
  <<HeaderB:12/bytes, BodyB/bytes>> = Request,
  Header = question_header(HeaderB),
  Questions = questions(BodyB, Header#header.qcount),
  {Header, Questions}.

question_header(HeaderBin) ->
  Header = header(HeaderBin),
  valid_question_header(Header),
  Header.

valid_question_header(#header{qr = ?QR_QUERY, opcode = ?OPCODE_QUERY, truncation = ?TRUNCATION}) -> true;
valid_question_header(_) -> false.

header(Header) ->
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

questions(QBody, QCount) ->
  hof:unfold(fun parse:take_question/1, {QBody, QCount}).

question_body(QBody) ->
  QNameSize = size(QBody) - 4,
  <<QName:QNameSize/bytes, QType:16, QClass:16>> = QBody,
  Name = qname_to_domain_name(QName),
  {Name, QType, QClass}.

qname_to_domain_name(QName) ->

  TakeLabel = fun
    (<<0:8>>) -> {};
    (<<Size:8, RestWithLabel/bytes>>) ->
      <<Label:Size/bytes, Rest/bytes>> = RestWithLabel,
      { binary_to_list(Label), Rest }
  end,

  Labels = hof:unfold(TakeLabel, QName),
  NameString = string:join(Labels, "."),
  list_to_binary(NameString).

take_question({<<>>, 0}) -> {};
take_question({<<>>, _Count}) -> error(bad_question);
take_question({_QBody, 0}) -> error(bad_question);
take_question({QBody, QCount}) ->
  QName = take_qname(QBody),
  QNameSize = size(QName),
  <<QName:QNameSize/bytes, 0:8, QType:16, QClass:16, Rest/bytes>> = QBody,
  DottedQName = varchars_to_dotted_name(QName),
  Val = {DottedQName, QType, QClass},
  NextSeed = {Rest, QCount - 1},
  { Val, NextSeed }.

take_qname(QBody) ->
  Labels = hof:unfold(fun parse:take_label/1, QBody),
  list_to_binary(Labels).

take_label(<<0:8, _/bytes>>) -> {};
take_label(<<Size:8, Label:Size/bytes, Rest/bytes>>) ->
  { <<Size:8, Label:Size/bytes>>, Rest }.

varchars_to_dotted_name(VarChar) ->
  TakeVarChar = fun
    (<<>>) -> {};
    (<<Size:8, Label:Size/bytes, Rest/bytes>>) ->
      LabelString = binary_to_list(Label),
      {LabelString, Rest}
  end,
  Labels = hof:unfold(TakeVarChar, VarChar),
  NameString = string:join(Labels, "."),
  list_to_binary(NameString).
