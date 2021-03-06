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
  {RequestHeader, Questions} = parse:request(Request),

  Lookups = [ lookup(Q, DBase) || Q <- Questions ],
  Answers = [ L || L <- Lookups, L /= not_found ],

  ResponseHeader = RequestHeader#header{
    qr=?QR_RESPONSE,
    authoritative_answer=?AUTHORITATIVE_ANSWER,
    truncation=?NO_TRUNCATION,
    recursion_available=?RECURSION_AVAILABLE,
    rcode=?RCODE_NO_ERROR,
    qcount=length(Questions),
    acount=length(Answers),
    nscount=0,
    arcount=0
  },

  serialize:response(ResponseHeader, Questions, Answers).

lookup({Name, TypeCode, ClassCode}, DBase) ->
    Class = class_from_code(ClassCode),
    Key = {Name, address, Class},
    DBase ! {self(), get, Key},
    receive
      {Ttl, Address} ->
        {Name, TypeCode, ClassCode, Ttl, Address};
      Any -> Any
    end.

class_from_code(?CLASS_IN) -> internet;
class_from_code(?CLASS_CS) -> csnet;
class_from_code(?CLASS_CH) -> chaos;
class_from_code(?CLASS_HS) -> hesiod;
class_from_code(?Q_CLASS_ANY) -> any.
