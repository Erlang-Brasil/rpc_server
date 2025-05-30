-module(rpc_server_sctp_connection).
-author('Fernando Areias <nando.calheirosx@gmail.com>').
-include("rpc_server.hrl").

-behaviour(gen_server).

-record(state, {
    clientSocket :: socket() | undefined,
    listenSocket :: socket() | undefined
}).

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link([ClientSocket, ListenSocket]) ->
    gen_server:start_link(?MODULE, [ClientSocket, ListenSocket], []).

init([ClientSocket, ListenSocket]) ->
    ?LOG_INFO("Iniciando conexao Listen Socket: ~p | Client Socket: ~p ", [ListenSocket, ClientSocket]),

    case inet:peername(ClientSocket) of
        {ok, {_, _}} ->
            inet:setopts(ClientSocket, [{active, true}]),
            gen_server:cast(self(), {process_request, ListenSocket, ClientSocket});
        {error, Reason} ->
            ?LOG_ERROR("Acceptor ~p: Falha ao obter endereço do cliente: ~p", [self(), Reason]),
            gen_tcp:close(ClientSocket),
            {stop, Reason}
    end,
    {ok, #state{listenSocket = ListenSocket, clientSocket = ClientSocket}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({process_request, ListenSocket, ClientSocket}, State) ->
    ?LOG_INFO("Iniciando processamento da request: Listen ~p | Client: ~p", [ListenSocket, ClientSocket]),
    {noreply, State}.

handle_info({tcp, Socket, Data}, #state{clientSocket = Socket} = State) ->
    ?LOG_INFO("Dados recebidos: ~p", [Data]),
    % TODO: Processar os dados recebidos
    {noreply, State};

handle_info({tcp_closed, Socket}, #state{clientSocket = Socket} = State) ->
    ?LOG_INFO("Conexão fechada pelo cliente"),
    {stop, normal, State};

handle_info({tcp_error, Socket, Reason}, #state{clientSocket = Socket} = State) ->
    ?LOG_ERROR("Erro na conexão: ~p", [Reason]),
    {stop, Reason, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{clientSocket = Socket}) ->
    ?LOG_INFO("Terminando conexão: ~p", [Reason]),
    gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
