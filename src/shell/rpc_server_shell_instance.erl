-module(rpc_server_shell_instance).

-author('Fernando Areias <nando.calheirosx@gmail.com>').

-define(MODULO_VERSAO, 1).
-vsn(?MODULO_VERSAO).

-behaviour(gen_server).
-include_lib("kernel/include/inet.hrl").
-include("rpc_server.hrl").

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    socket :: socket() | undefined,
    connection :: pid() | undefined,
    hashId :: term() | undefined,
    status :: connected | disconnected
}).

start_link([ClientSocket, ConnectionPid]) ->
    gen_server:start_link(?MODULE, [ClientSocket, ConnectionPid], []).

init([ClientSocket, ConnectionPid]) ->
    HashId = gen_hash_identification(ClientSocket),
    ets:insert(connection_table, {HashId, self()}),
    ?LOG_INFO("Iniciando shell ClientSocket ~p | ConnectionPid ~p | Hash ~p | Versão ~p", [ClientSocket, ConnectionPid, HashId, ?MODULO_VERSAO]),
    gen_server:cast(ConnectionPid, {command_response, io_lib:format("Client identification ~p~n", [HashId])}),
    
    {ok, #state{connection = ConnectionPid, socket = ClientSocket, hashId = HashId, status = connected}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({hibernate}, State = #state{socket = _}) ->
    ?LOG_INFO("[SHELL INSTANCE] - Conexão com o cliente interrompida, marcando como desconectado..."),
    ?LOG_INFO("[SHELL INSTANCE] - Estado antes da desconexão: ~p", [State]),
    NewState = State#state{status = disconnected},
    ?LOG_INFO("[SHELL INSTANCE] - Estado após hibernação: ~p", [NewState]),
    {noreply, NewState};

handle_cast({reconnected, ConnectionPid, ClientSocket}, State) ->
    ?LOG_INFO("[SHELL INSTANCE] - Recebida mensagem de reconexão: ConnectionPid=~p, ClientSocket=~p", [ConnectionPid, ClientSocket]),
    ?LOG_INFO("[SHELL INSTANCE] - Conexão restabelecida com o cliente, shell pronto..."),
    ?LOG_INFO("[SHELL INSTANCE] - Nova ConnectionPid: ~p, Novo ClientSocket: ~p", [ConnectionPid, ClientSocket]),
    ?LOG_INFO("[SHELL INSTANCE] - Estado atual: ~p", [State]),
    gen_server:cast(ConnectionPid, {command_response, "Conexao restabelecida\n"}),
    NewState = State#state{connection = ConnectionPid, socket = ClientSocket, hashId = State#state.hashId, status = connected},
    ?LOG_INFO("[SHELL INSTANCE] - Estado após reconexão: ~p", [NewState]),
    {noreply, NewState};

handle_cast({execute_command, Command}, State = #state{socket = _}) ->
    case State#state.status of
        disconnected ->
            ?LOG_INFO("[SHELL INSTANCE] - Tentativa de executar comando enquanto desconectado: ~p", [Command]),
            {noreply, State};
        connected ->
            ?LOG_INFO("[SHELL INSTANCE] - Executando comando ~p", [Command]),
             
            CommandStr = re:replace(binary_to_list(Command), "[\r\n]+$", "", [{return, list}]),
            ?LOG_INFO("[SHELL INSTANCE] - Resultado comando limpo ~p", [CommandStr]), 
            
            %% TODO: COLOCAR UM TIMEOUT NA EXECUCAO DO COMANDO
            %% TODO: VALIDAR O COMANDO
            ShellCmd = io_lib:format("/bin/sh -c ~p", [CommandStr]),
            ?LOG_INFO("[SHELL INSTANCE] - Comando shell final: ~p", [ShellCmd]),
            Port = open_port({spawn, ShellCmd}, [stream, in, eof, hide, exit_status]),
            {ExitCode, Output} = get_data(Port, []),
            ?LOG_INFO("[SHELL INSTANCE] - Resultado shell ~p", [{ExitCode, Output}]),
            
            gen_server:cast(State#state.connection, {command_response, io_lib:format("~s~n", [Output])}),
            
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    ?LOG_INFO("[SHELL INSTANCE] - Mensagem cast genérica recebida: ~p", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    ?LOG_INFO("[SHELL INSTANCE] - Mensagem info recebida: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% PRIVATE FUNCITONS




%% @doc
%% Gera um hash identificador único para uma conexão (baseado em djb_hash).
%%
%% Este hash pode ser usado como identificador persistente para reconexões,
%% baseado nas informações do socket do cliente (por exemplo, IP e porta).
%%
%% @param Socket :: pid() | {inet:ip_address(), inet:port_number()}
%% @return non_neg_integer()
-spec gen_hash_identification(gen_tcp:socket() | {inet:ip_address(), inet:port_number()}) -> non_neg_integer().
gen_hash_identification(Socket) when is_port(Socket) ->
    {ok, {IP, Port}} = inet:peername(Socket),
    gen_hash_from_ip_port(IP, Port);

gen_hash_identification({IP, Port}) when is_tuple(IP), (size(IP) == 4 orelse size(IP) == 8), is_integer(Port), Port > 0 ->
    gen_hash_from_ip_port(IP, Port).

%% @private
%% Converte o IP para string (ex: "192.168.0.1") e concatena com porta
%% Depois aplica o hash DJB
gen_hash_from_ip_port(IP, Port) ->
    IPStr = inet:ntoa(IP),
    Str = lists:flatten(io_lib:format("~s:~p", [IPStr, Port])),
    crypto:hash(sha256, Str).
    


%% @doc
%% Lê todos os dados recebidos de uma porta Erlang até o fim (`eof`),
%% acumulando-os e retornando junto com o código de saída.
%%
%% Essa função é útil para capturar saída de processos externos abertos
%% com `open_port/2`, como scripts ou comandos do sistema operacional.
%%
%% @param Port :: port() - A porta conectada ao processo externo.
%% @param Sofar :: iodata() - Dados já recebidos (acumulador).
%%
%% @returns {ExitCode, Data}
%%   <ul>
%%     <li>{@type ExitCode = integer()} - Código de saída do processo externo.</li>
%%     <li>{@type Data = binary() | string()} - Dados completos recebidos da porta.</li>
%%   </ul>
%%
%% @private open_port/2
get_data(Port, Sofar) ->
    receive
    {Port, {data, Bytes}} ->
        get_data(Port, [Sofar|Bytes]);
    {Port, eof} ->
        Port ! {self(), close},
        receive
        {Port, closed} ->
            true
        end,
        receive
            {'EXIT',  Port,  _} ->
                ok
        after 1 ->             
            ok
        end,
        ExitCode =
            receive
            {Port, {exit_status, Code}} ->
                Code
        end,
        {ExitCode, lists:flatten(Sofar)}
    end.