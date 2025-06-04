%%% @doc Supervisor principal da aplicação (Raiz da árvore)
%%%
%%% Este supervisor gerencia todos os processos da aplicação,
%%% incluindo o listener TCP e seus componentes.
%%%
%%% @end
-module(rpc_server_sup).
-author('Fernando Areias <nando.calheirosx@gmail.com>').


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
    process_flag(trap_exit, true),
    
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    ChildSpecs = [ 
        #{
            id => rpc_server_sctp_listen_sup,
            start => {rpc_server_sctp_listen_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [rpc_server_sctp_listen_sup]
        },
        #{
            id => rpc_server_shell_manager_sup,
            start => {rpc_server_shell_manager_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [rpc_server_shell_manager_sup]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
