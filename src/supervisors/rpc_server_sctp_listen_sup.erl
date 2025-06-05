-module(rpc_server_sctp_listen_sup).

-author('Fernando Areias <nando.calheirosx@gmail.com>').
-include("rpc_server.hrl").

-define(MODULO_VERSAO, 1).
-vsn(?MODULO_VERSAO).

-ignore_xref([{start_link, 0}]).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


%%% @doc Inicia o supervisor principal.
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
    ?LOG_INFO("Iniciando supervisor do listening, versão ~p", [?MODULO_VERSAO]),
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 10,
        period => 60
    },
    
    ChildSpecs = [
        #{
            id => rpc_server_sctp_acceptor_sup,
            start => {rpc_server_sctp_acceptor_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [rpc_server_sctp_acceptor_sup]
        },
        #{
            id => rpc_server_sctp_listen,
            start => {rpc_server_sctp_listen, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [rpc_server_sctp_listen]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}. 