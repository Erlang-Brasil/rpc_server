-module(rpc_server_shell_manager_sup).

-author('Fernando Areias <nando.calheirosx@gmail.com>').

-include("rpc_server.hrl").

-define(MODULO_VERSAO, 1).
-vsn(?MODULO_VERSAO).

-ignore_xref([{start_link, 0}]).

-behaviour(supervisor).
-include("rpc_server.hrl").
-export([start_link/1]).

-export([init/1, start_shell/2]).


%%% @doc Inicia o supervisor do io manager.
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
    ?LOG_INFO("Iniciando supervisor do shell manager, versão ~p", [?MODULO_VERSAO]),
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [
    #{
        id => rpc_server_shell_instance,
        start => {rpc_server_shell_instance, start_link, []},
        restart => temporary,  % Não inicia automaticamente
        shutdown => 9000,
        type => worker,
        modules => [rpc_server_shell_instance]
    }
    ],

    {ok, {SupFlags, ChildSpecs}}. 

% !WARNING Line 51 Column 30: Unknown type ssl:sslsocket/0
-spec start_shell(socket(), pid()) -> pid() | {error, Reason :: term()}.
start_shell(ClientSocket, ConnectionPid) ->
    Args = [ClientSocket, ConnectionPid],
    case supervisor:start_child(?MODULE, [Args]) of 
        {ok, ShellPid} -> 
            ?LOG_INFO("Shell Iniciado PID ~p", [ShellPid]),
            ShellPid;
        {error, Reason} ->
            ?LOG_ERROR("Falha ao iniciar o shell ~p", [Reason]),
            {error, Reason}
    end.

 