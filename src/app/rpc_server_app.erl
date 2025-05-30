-module(rpc_server_app).

-author('Fernando Areias <nando.calheirosx@gmail.com>').
-behaviour(application).

-include_lib("kernel/include/logger.hrl").

-export([start/2, stop/1]).

%%% @doc Inicia a aplicação.
%%%
%%% @param StartType application:start_type() - Tipo de início
%%% @param StartArgs term() - Argumentos de início
%%% @returns {ok, pid()} | {error, term()}
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    case rpc_server_sup:start_link() of
        {ok, Pid} ->
            ?LOG_INFO("RPC Server iniciado"),
            {ok, Pid};
        Error ->
            ?LOG_ERROR("Falha ao iniciar RPC Server: ~p", [Error]),
            Error
    end.

%%% @doc Para a aplicação.
%%%
%%% @param State term() - Estado da aplicação
%%% @returns ok
-spec stop(term()) -> ok.
stop(_State) ->
    ?LOG_INFO("RPC Server parado"),
    ok.
 