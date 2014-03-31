-module(wes_bank).

-export([start_session/1,
         open_account/2,
         create_account/2,
         transfer/4,
         insert/3,
         withdraw/3,
         balance/2,
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
    true = lists:all(fun({_, ok}) -> true end,
                     wes:command(?chan(Session), insert, Payload)),
    ok.

withdraw(Session, Account, Amount) when Amount >  0 ->
    Payload = {Account, Amount},
    true = lists:all(fun({_, ok}) -> true end,
                     wes:command(?chan(Session), withdraw, Payload)),
    ok.

transfer(Session, FromAccount, ToAccount, Amount) when Amount > 0 ->
    Payload = {FromAccount, ToAccount, Amount},
    true = lists:all(fun({_, ok}) -> true end,
                     wes:command(?chan(Session), transfer, Payload)),
    ok.

balance(Session, Account) ->
    Result = wes:command(?chan(Session), balance),
    proplists:get_value(?actor(Account), Result).

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
    error_logger:info_msg("Balance 1 Account1 ~p", [balance(Session, Account1)]),
    error_logger:info_msg("Balance 1 Account2 ~p", [balance(Session, Account2)]),
    insert(Session, Account1, 10),
    insert(Session, Account2, 5),
    error_logger:info_msg("Balance 2 Account1 ~p", [balance(Session, Account1)]),
    error_logger:info_msg("Balance 2 Account2 ~p", [balance(Session, Account2)]),
    transfer(Session, Account1, Account2, 1),
    error_logger:info_msg("Balance 3 Account1 ~p", [balance(Session, Account1)]),
    error_logger:info_msg("Balance 3 Account2 ~p", [balance(Session, Account2)]),
    stop_session(Session),
    ok.

test_load() ->
    Session = adam,
    Account1 = <<"ac1">>,
    Account2 = <<"ac2">>,
    start_session(Session),
    open_account(Session, Account1),
    open_account(Session, Account2),
    error_logger:info_msg("Balance 2 Account1 ~p", [balance(Session, Account1)]),
    error_logger:info_msg("Balance 2 Account2 ~p", [balance(Session, Account2)]),
    stop_session(Session),
    ok.
