-module(wes_bank_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    case wb_sup:start_link() of
        {ok, Pid} ->
            %% CRAP
            wes_db_ets:start([{sup_name,wes_db_ets}]),
            wes_lock_ets:start(1000),
            wes_stats_ets:start_link(),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
