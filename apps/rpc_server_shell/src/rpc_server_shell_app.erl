%%%-------------------------------------------------------------------
%% @doc rpc_server_shell public API
%% @end
%%%-------------------------------------------------------------------

-module(rpc_server_shell_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rpc_server_shell_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
