-module(wes_bank_elli_handler).

-behaviour(elli_handler).

-include_lib("elli/include/elli.hrl").

-export([handle/2,
         handle_event/3]).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('POST', [Session], _Req) ->
    wes_bank:start_session(Session),
    {204, [], <<"">>};
handle('POST', [Session, <<"insert">>], Req) ->
    [Account, Amount] = args([account, amount], Req),
    wes_bank:open_account(Session, Account),
    ok = wes_bank:insert(Session, Account, Amount),
    {204, [], <<"">>};
handle('POST', [Session, <<"withdraw">>], Req) ->
    [Account, Amount] = args([account, amount], Req),
    wes_bank:open_account(Session, Account),
    ok = wes_bank:withdraw(Session, Account, Amount),
    {204, [], <<"">>};
handle('POST', [Session, <<"transfer">>], Req) ->
    [From, To, Amount] = args([from, to, amount], Req),
    wes_bank:open_account(Session, From),
    wes_bank:open_account(Session, To),
    ok = wes_bank:transfer(Session, From, To, Amount),
    {204, [], <<"">>};
handle('GET', [_Session, Account, <<"balance">>], _Req) ->
    Value = wes_bank:balance(Account),
    {200, [], jiffy:encode({[{balance, Value}]})};
handle(_, _, _) -> ignore.


handle_event(request_complete, _Data, _Args) ->
    ok;
handle_event(Event, Data, _Args) ->
    error_logger:info_msg("Event ~p: ~p", [Event, Data]),
    ok.

args(Args, Req) ->
    {Props} = jiffy:decode(elli_request:body(Req)),
    lists:map(
      fun(Arg) ->
              case lists:keyfind(atom_to_binary(Arg, utf8), 1, Props) of
                  {_, Value} -> Value;
                  false -> throw({400, [], <<"">>})
              end
      end,
      Args).
