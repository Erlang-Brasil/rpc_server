-module(rpc_server_sctp_acceptor_sup).

-author('Fernando Areias <nando.calheirosx@gmail.com>').
-include("rpc_server.hrl").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_acceptor/2]).


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
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    
    ChildSpecs = [
        #{
            id => rpc_server_sctp_connection,
            start => {rpc_server_sctp_connection, start_link, []},
            restart => temporary,  % Não inicia automaticamente
            shutdown => 90,
            type => worker,
            modules => [rpc_server_sctp_connection]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}. 

-spec start_acceptor(socket() | ssl:sslsocket(), socket() | ssl:sslsocket()) -> ok | {error, Reason :: term()}.
start_acceptor(ClientSocket, ListenSocket) ->
    Args = [ClientSocket, ListenSocket],
    case supervisor:start_child(?MODULE, [Args]) of
        {ok, AcceptorPid} ->
            ?LOG_INFO("Acceptor criado com PID ~p", [AcceptorPid]), 
            {ok};
        {error, Reason} ->
            ?LOG_ERROR("Falha ao iniciar acceptor: ~p", [Reason]),
            {error, Reason}
    end.