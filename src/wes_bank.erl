-module(wes_bank).

-export([start_session/1,
         open_account/2,
         transfer/4,
         insert/3,
         withdraw/3,
         balance/1,
         test/0]).

-define(CHANNEL, bank_session).

start_session(Session) ->
    wes_channel:start(?CHANNEL, Session, []).

stop_session(Session) ->
    wes_channel:stop(?CHANNEL, Session).

insert(Session, To, Amount) when Amount > 0 ->
    Payload = {To, Amount},
    ok = wes_channel:command(?CHANNEL, Session, insert, Payload).

withdraw(Session, To, Amount) when Amount >  0 ->
    Payload = {To, Amount},
    ok = wes_channel:command(?CHANNEL, Session, withdraw, Payload).

transfer(Session, From, To, Amount) when Amount > 0 ->
    Payload = {From, To, Amount},
    ok = wes_channel:command(?CHANNEL, Session, transfer, Payload).

balance(Name) ->
    wes_channel:read(account, Name, balance).

open_account(Session, Account) ->
    ok = wes_channel:ensure_actor(?CHANNEL, Session, account,
                                  Account, [Account]).

test() ->
    Session = andreas,
    Account1 = <<"ac1">>,
    Account2 = <<"ac2">>,
    start_session(Session),
    open_account(Session, Account1),
    open_account(Session, Account2),
    error_logger:info_msg("Balance 1 Account1 ~p", [balance(Account1)]),
    error_logger:info_msg("Balance 1 Account2 ~p", [balance(Account2)]),
    insert(Session, Account1, 10),
    insert(Session, Account2, 5),
    error_logger:info_msg("Balance 2 Account1 ~p", [balance(Account1)]),
    error_logger:info_msg("Balance 2 Account2 ~p", [balance(Account2)]),
    transfer(Session, Account1, Account2, 1),
    error_logger:info_msg("Balance 3 Account1 ~p", [balance(Account1)]),
    error_logger:info_msg("Balance 3 Account2 ~p", [balance(Account2)]),
    stop_session(Session),
    ok.
