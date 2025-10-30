%%%-------------------------------------------------------------------
%% @doc rpc_server_http public API
%% @end
%%%-------------------------------------------------------------------

-module(rpc_server_http_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rpc_server_http_listen_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
