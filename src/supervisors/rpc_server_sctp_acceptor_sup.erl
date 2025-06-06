-module(rpc_server_sctp_acceptor_sup).

-author('Fernando Areias <nando.calheirosx@gmail.com>').
-include("rpc_server.hrl").

-define(MODULO_VERSAO, 1).
-vsn(?MODULO_VERSAO).

-ignore_xref([{start_link, 1}]).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).
-export([start_acceptor/2]).


%%% @doc Inicia o supervisor principal.
%%%
%%% @returns {ok, pid()} | {error, term()}
-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).


%%% @doc Callback de inicialização do supervisor.
%%%
%%% @returns {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}
-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_Args) ->
    ?LOG_INFO("Iniciando supervisor do acceptor, versão ~p", [?MODULO_VERSAO]),
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


%%% @doc Inicia um novo processo acceptor para gerenciar uma conexão recebida.
%%%
%%% Essa função é chamada sempre que uma nova conexão é aceita no socket de escuta.
%%% Ela inicia um novo processo filho sob o supervisor do módulo atual, responsável por
%%% gerenciar a comunicação com o cliente conectado.
%%%
%%% @param ClientSocket {socket() | ssl:sslsocket()} - O socket conectado ao cliente.
%%% @param ListenSocket {socket() | ssl:sslsocket()} - O socket que aceitou a conexão.
%%%
%%% @returns ok | {error, Reason :: term()}
%%%          Retorna `ok` se o processo acceptor foi iniciado com sucesso,
%%%          ou `{error, Reason}` caso contrário (por exemplo, falha ao iniciar o child no supervisor).
%%%
% !WARNING da onde vem a chamada ? ssl:sslsocket() dialzyer apontou erro 
-spec start_acceptor(socket(), socket()) -> ok | {error, Reason :: term()}.
start_acceptor(ClientSocket, ListenSocket) ->    
    Args = [ClientSocket, ListenSocket],
    case supervisor:start_child(?MODULE, [Args]) of
        {ok, AcceptorPid} ->
            ?LOG_INFO("Acceptor criado com PID ~p", [AcceptorPid]),            
            case gen_tcp:controlling_process(ClientSocket, AcceptorPid) of
                ok ->
                    ?LOG_INFO("Controle do socket transferido para o acceptor ~p", [AcceptorPid]),
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("Falha ao transferir controle do socket para o acceptor: ~p", [Reason]),
                    supervisor:terminate_child(?MODULE, AcceptorPid),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG_ERROR("Falha ao iniciar acceptor: ~p", [Reason]),
            {error, Reason}
    end.