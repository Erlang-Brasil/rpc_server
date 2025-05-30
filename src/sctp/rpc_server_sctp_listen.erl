-module(rpc_server_sctp_listen).

-author('Fernando Areias <nando.calheirosx@gmail.com>').

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("rpc_server.hrl").

 
-record(state, {
    socket :: socket() | undefined,
    acceptor_sup :: pid() | undefined,
    ref :: reference() | undefined
}).


%%% @doc Inicia o listener TCP.
%%%
%%% @returns {ok, pid()} | {error, term()}
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, #state{}} | {stop, term()}.
init([]) ->
    process_flag(trap_exit, true),

    case wait_for_acceptor_sup(50) of
        {ok, AcceptorSup} ->    
            start_listening(AcceptorSup);
        {error, Reason} ->
            ?LOG_ERROR("Falha ao encontrar supervisor de acceptors: ~p", [Reason]),
            {stop, Reason}
    end.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({inet_async, ListenSocket, Ref, {ok, ClientSocket}}, #state{socket = ListenSocket, ref = Ref} = State) ->
    ?LOG_INFO("Nova conexão recebida de ~p", [inet:peername(ClientSocket)]),
    case set_sockopt(ListenSocket, ClientSocket) of
        ok ->
            {ok} = rpc_server_sctp_acceptor_sup:start_acceptor(ClientSocket, ListenSocket),
            {ok, NewRef} = prim_inet:async_accept(ListenSocket, -1),
            {noreply, State#state{ref = NewRef}};
        {error, Reason} ->
            ?LOG_ERROR("Erro ao configurar opções do socket: ~p", [Reason]),
            gen_tcp:close(ClientSocket),
            {stop, Reason, State}
    end;

handle_info({inet_async, ListenSocket, Ref, Error}, #state{socket = ListenSocket, ref = Ref} = State) ->
    ?LOG_ERROR("Erro no acceptor do socket: ~p", [Error]),
    {stop, exceeded_accept_retry_count, State};

handle_info(_Info, State) ->
    {noreply, State}.


%% PRIVATE FUNCTIONS

wait_for_acceptor_sup(0) ->
    {error, acceptor_supervisor_not_found};
wait_for_acceptor_sup(Attempts) ->
    case whereis(rpc_server_sctp_acceptor_sup) of
        undefined ->
            timer:sleep(200),
            wait_for_acceptor_sup(Attempts - 1);
        Pid ->
            {ok, Pid}
    end.


start_listening(AcceptorSup) ->
    Port = application:get_env(rpc_server, tcp_port, 8080),
    Options = [
        binary,
        {packet, 2},
        {reuseaddr, true},
        {keepalive, true},
        {backlog, 128},
        {active, false}
    ],
    ?LOG_INFO("Tentando escutar na porta ~p com opções ~p", [Port, Options]),
    handle_listening(gen_tcp:listen(Port, Options), AcceptorSup).

handle_listening({ok, Socket}, AcceptorSup) ->
    case inet:sockname(Socket) of 
        {ok, {_IP, Port}} ->
            ?LOG_INFO("Socket escutando em ~p:~p", [_IP, Port]),
            {ok, Ref} = prim_inet:async_accept(Socket, -1),
            {ok, #state{socket = Socket, acceptor_sup = AcceptorSup, ref = Ref}};
        {error, Reason} ->
            ?LOG_ERROR("Socket não está escutando: ~p", [Reason]),
            gen_tcp:close(Socket),
            {stop, Reason}
    end;

handle_listening({error, Reason}, _) ->
    ?LOG_ERROR("Falha ao iniciar listener: ~p", [Reason]),
    {stop, Reason}.

set_sockopt(ListenSocket, ClientSocket) ->
    true = inet_db:register_socket(ClientSocket, inet_tcp),
    case prim_inet:getopts(ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(ClientSocket, Opts) of
                ok -> ok;
                Error -> 
                    gen_tcp:close(ClientSocket), 
                    Error
            end;
        Error ->
            gen_tcp:close(ClientSocket), 
            Error
    end.

 