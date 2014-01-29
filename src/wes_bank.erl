-module(wes_bank).

-export([start_wes/1,
         transfer/4,
         insert/3,
         withdraw/3,
         balance/1]).

start_wes(Session) ->
    wes_channel:start(bank_session, Session, []).

insert(Session, To, Amount) when Amount > 0 ->
    Payload = {To, Amount},
    start_wes(Session),
    ok = wes_channel:register_actor(bank_session, Session, account, To, [To]),
    ok = wes_channel:command(bank_session, Session, insert, Payload).

withdraw(Session, To, Amount) when Amount >  0 ->
    Payload = {To, Amount},
    start_wes(Session),
    ok = wes_channel:register_actor(bank_session, Session, account, To, [To]),
    ok = wes_channel:command(bank_session, Session, withdraw, Payload).

transfer(Session, From, To, Amount) when Amount > 0 ->
    Payload = {From, To, Amount},
    start_wes(Session),
    ok = wes_channel:register_actor(bank_session, Session, account, From, [From]),
    ok = wes_channel:register_actor(bank_session, Session, account, To, [To]),
    ok = wes_channel:command(bank_session, Session, transfer, Payload).

balance(Name) ->
    wes_channel:read(bank_session, account, Name, balance).
