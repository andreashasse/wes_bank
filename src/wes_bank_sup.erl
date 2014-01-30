-module(wes_bank_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    ElliSpec = elli_spec(),
    ChildSpecs = [ElliSpec],
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

elli_spec() ->
    Mods =
        [{wes_bank_elli_handler, []}],
    Args =
        [{port, 8080},
         {callback, elli_middleware},
         {callback_args, [{mods, Mods}]}],
    ?CHILD(elli, worker, [Args]).
