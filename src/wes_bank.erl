-module(wes_bank).

-export([start_session/1,
         open_account/2,
         transfer/4,
         insert/3,
         withdraw/3,
         balance/1,
         test/0]).

start_session(Session) ->
    {ok, _} = wes_channel:start(bank_session, Session, []).

stop_session(Session) ->
    wes_channel:stop(bank_session, Session).

insert(Session, To, Amount) when Amount > 0 ->
    Payload = {To, Amount},
    ok = wes_channel:command(bank_session, Session, insert, Payload).

withdraw(Session, To, Amount) when Amount >  0 ->
    Payload = {To, Amount},
    ok = wes_channel:command(bank_session, Session, withdraw, Payload).

transfer(Session, From, To, Amount) when Amount > 0 ->
    Payload = {From, To, Amount},
    ok = wes_channel:command(bank_session, Session, transfer, Payload).

balance(Name) ->
    wes_channel:read(bank_session, account, Name, balance).

open_account(Session, Account) ->
    ok = wes_channel:register_actor(bank_session, Session, account,
                                    Account, [Account]).

test() ->
    Session = andreas,
    start_session(Session),
    open_account(Session, a1),
    open_account(Session, a2),
    error_logger:info_msg("Balance 1 a1 ~p", [balance(a1)]),
    error_logger:info_msg("Balance 1 a2 ~p", [balance(a2)]),
    insert(Session, a1, 10),
    insert(Session, a2, 5),
    error_logger:info_msg("Balance 2 a1 ~p", [balance(a1)]),
    error_logger:info_msg("Balance 2 a2 ~p", [balance(a2)]),
    transfer(Session, a1, a2, 1),
    error_logger:info_msg("Balance 3 a1 ~p", [balance(a1)]),
    error_logger:info_msg("Balance 3 a2 ~p", [balance(a2)]),
    stop_session(Session),
    ok.
