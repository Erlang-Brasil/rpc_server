-module(rpc_server_io_manager_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).


%%% @doc Inicia o supervisor do io manager.
%%%
%%% @returns {ok, pid()} | {error, term()}
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%% @doc Callback de inicialização do supervisor.
%%%
%%% @returns {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [
    
    ],

    {ok, {SupFlags, ChildSpecs}}. 