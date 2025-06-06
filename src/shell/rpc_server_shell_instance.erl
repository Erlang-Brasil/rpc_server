-module(rpc_server_shell_instance).

-author('Fernando Areias <nando.calheirosx@gmail.com>').

-define(MODULO_VERSAO, 1).
-vsn(?MODULO_VERSAO).

-behaviour(gen_server).
-include_lib("kernel/include/inet.hrl").
-include("rpc_server.hrl").

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, handle_hibernate/1]).

-record(state, {
    socket :: socket() | undefined,
    connection :: pid() | undefined,
    hashId :: term() | undefined
}).

start_link([ClientSocket, ConnectionPid]) ->
    gen_server:start_link(?MODULE, [ClientSocket, ConnectionPid], []).

init([ClientSocket, ConnectionPid]) ->
    HashId = gen_hash_identification(ClientSocket),
    ets:insert(connection_table, {HashId, self()}),
    ?LOG_INFO("Iniciando shell ClientSocket ~p | ConnectionPid ~p | Hash ~p | Versão ~p", [ClientSocket, ConnectionPid, HashId, ?MODULO_VERSAO]),
    gen_server:cast(ConnectionPid, {command_response, io_lib:format("Client identification ~p~n", [HashId])}),
    
    {ok, #state{connection = ConnectionPid, socket = ClientSocket, hashId = HashId}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({hibernate}, State = #state{socket = _}) ->
    ?LOG_INFO("[SHELL INSTANCE] - Conexão com o cliente interrompida, hibernando o shell..."),
    proc_lib:hibernate(?MODULE, handle_hibernate, [State]);

handle_cast({execute_command, Command}, State = #state{socket = _}) ->
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
    
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_hibernate(State) ->
    {noreply, State, hibernate}.
 

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
    djb_hash(list_to_binary(Str)).

%% @private
%% Implementação do DJB hash
djb_hash(Bin) when is_binary(Bin) ->
    djb_hash(Bin, 5381).

djb_hash(<<C, Rest/binary>>, Hash) ->
    djb_hash(Rest, Hash * 33 + C);
djb_hash(<<>>, Hash) ->
    Hash.
    


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