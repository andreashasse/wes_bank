-module(wes_bank_elli_handler).

-behaviour(elli_handler).

-include_lib("elli/include/elli.hrl").

-export([handle/2,
         handle_event/3]).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('POST', [Session, "transfer"], Req) ->
    [From, To, Amount] = args([from, to, amount], Req),
    ok = wes_bank:transfer(Session, From, To, Amount),
    {204, [], <<"">>}.

handle_event(_Event, _Data, _Args) ->
    ok.

args(Args, Req) ->
    {Props} = jiffy:encode(elli_request:body(Req)),
    lists:map(
      fun(Arg) ->
              {_, Value} = lists:keyfind(atom_to_binary(Arg, utf8), 1, Props),
              Value
      end,
      Args).
