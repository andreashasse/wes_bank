-module(wes_bank).

-export([start_session/1,
         open_account/2,
         create_account/2,
         transfer/4,
         insert/3,
         withdraw/3,
         balance/1,
         test_create/0,
         test_load/0]).

-define(chan(Session), {bank_session, Session}).
-define(actor(Account), {account, Account}).
-define(spec(Account, Type), {Type, ?actor(Account), [Account]}).

start_session(Session) ->
    {ok, _Ref} = wes:ensure_channel(?chan(Session)).

stop_session(Session) ->
    ok = wes:stop_channel(?chan(Session)).

insert(Session, Account, Amount) when Amount > 0 ->
    Payload = {Account, Amount},
    ok = wes:command(?chan(Session), insert, Payload).

withdraw(Session, Account, Amount) when Amount >  0 ->
    Payload = {Account, Amount},
    ok = wes:command(?chan(Session), withdraw, Payload).

transfer(Session, FromAccount, ToAccount, Amount) when Amount > 0 ->
    Payload = {FromAccount, ToAccount, Amount},
    ok = wes:command(?chan(Session), transfer, Payload).

balance(Account) ->
    wes:read(?actor(Account), balance).

create_account(Session, Account) ->
    ok = wes:create_actor(?chan(Session), ?spec(Account, create)).

open_account(Session, Account) ->
    case wes:ensure_actor(?chan(Session), ?spec(Account, load)) of
        ok -> ok;
        {error, {error_registering_actor, already_locked}} -> {error, busy}
    end.

test_create() ->
    Session = andreas,
    Account1 = <<"ac1">>,
    Account2 = <<"ac2">>,
    start_session(Session),
    create_account(Session, Account1),
    create_account(Session, Account2),
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

test_load() ->
    Session = adam,
    Account1 = <<"ac1">>,
    Account2 = <<"ac2">>,
    start_session(Session),
    open_account(Session, Account1),
    open_account(Session, Account2),
    error_logger:info_msg("Balance 2 Account1 ~p", [balance(Account1)]),
    error_logger:info_msg("Balance 2 Account2 ~p", [balance(Account2)]),
    stop_session(Session),
    ok.
