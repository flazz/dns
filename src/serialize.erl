-module(serialize).
-compile(export_all).

-include("include/dns_values.hrl").

label_to_varchar(Label) ->
  Size = size(Label),
  list_to_binary([Size, Label]).

domain_name(Name) ->
  Labels = binary:split(Name, <<".">>, [global]),
  VarChars = lists:map(fun label_to_varchar/1, Labels),
  list_to_binary(VarChars ++ [<<0:8>>]).

question({Name, Type, Class}) ->
  Labels = domain_name(Name),
  list_to_binary([Labels] ++ [<<Type:16, Class:16>>]).

header(H) ->
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

resource_record(Name, Type, Class, TTL, Data) ->
  Labels = domain_name(Name),
  DataLength = size(Data),
  Tail = <<Type:16, Class:16, TTL:32/unsigned, DataLength:16/unsigned, Data/bytes>>,
  list_to_binary([Labels, Tail]).

address_record(Name, Type, Class, TTL, Address) ->
  {A, B, C, D} = Address,
  Data = <<A:8, B:8, C:8, D:8>>,
  resource_record(Name, Type, Class, TTL, Data).

response(Header, Questions, Answers) ->
  Hb = header(Header),
  Qb = [ question(Q) || Q <- Questions ],
  Ab = [ address_record(Name, Type, Class, TTL, Address) || {Name, Type, Class, TTL, Address} <- Answers ],
  List = [Hb] ++ Qb ++ Ab,
  list_to_binary(List).
