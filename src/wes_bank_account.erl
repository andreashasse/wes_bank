-module(wes_bank_account).

-behaviour(wes_actor).

-export([init/1,
         read/2,
         command/4,
         key/1,
         to_struct/2,
         from_struct/1]).

-record(wb_acc,
        {balance = 0,
         name}).

init([Name]) ->
    {ok, #wb_acc{name = Name}}.

read(balance, #wb_acc{balance = B}) ->
    B.

command(_, transfer, [{Name, _To, Amount}],
        #wb_acc{name = Name, balance = AccAmount})
  when Amount > AccAmount->
    throw({negative_balance, Name});
command(_, transfer, {Name, _To, Amount}, #wb_acc{name = Name} = State) ->
    {ok, State#wb_acc{balance = State#wb_acc.balance - Amount}};
command(_, transfer, {_From, Name, Amount}, #wb_acc{name = Name} = State) ->
    {ok, State#wb_acc{balance = State#wb_acc.balance + Amount}};
command(_, transfer, {_From, Name, Amount}, #wb_acc{name = Name} = State) ->
    {ok, State#wb_acc{balance = State#wb_acc.balance + Amount}};
command(_, insert, {Name, Amount}, #wb_acc{name = Name} = State) ->
    {ok, State#wb_acc{balance = State#wb_acc.balance + Amount}};
command(_, withdraw, {Name, Amount}, #wb_acc{balance = Balance})
  when Amount > Balance ->
    throw({negative_balance, Name});
command(_, withdraw, {Name, Amount}, #wb_acc{name = Name} = State) ->
    {ok, State#wb_acc{balance = State#wb_acc.balance - Amount}};
command(_, _, _, State) ->
    {ok, State}.

key(Actorname) ->
    <<"wb_account", (atom_to_binary(Actorname, utf8))/binary>>.

to_struct(_Actorname, #wb_acc{name = Name, balance = Balance}) ->
    jiffy:encode({[{balance, Balance}, {name, Name}]}).

from_struct({_Key, Value}) ->
    {Props} = jiffy:decode(Value),
    {_, Balance} = lists:keyfind(<<"balance">>, 1, Props),
    {_, Name} = lists:keyfind(<<"name">>, 1, Props),
    #wb_acc{balance = Balance,
            name = Name}.
