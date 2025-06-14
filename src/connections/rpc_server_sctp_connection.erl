-module(rpc_server_sctp_connection).
-author('Fernando Areias <nando.calheirosx@gmail.com>').
-include("rpc_server.hrl").

-define(MODULO_VERSAO, 1).

-vsn(?MODULO_VERSAO).
-behaviour(gen_server).

-record(state, {
    clientSocket :: socket() | undefined,
    listenSocket :: socket() | undefined,
    shellInstancePid :: pid() | undefined
}).

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% @doc Inicia um servidor gen_server responsável por gerenciar a conexão de um cliente.
%%%
%%% Esta função é utilizada para iniciar um processo vinculado (`link`) do tipo `gen_server`
%%% que será responsável por gerenciar a comunicação com um cliente conectado via socket.
%%%
%%% @param StartType application:start_type() - Tipo de início da aplicação,
%%%        geralmente passado automaticamente quando iniciado como parte de uma aplicação OTP.
%%% @param StartArgs list() - Lista contendo os argumentos necessários para a inicialização,
%%%        espera-se que contenha dois elementos:
%%%        <ul>
%%%          <li>{@type ClientSocket} - O socket conectado ao cliente.</li>
%%%          <li>{@type ListenSocket} - O socket que está escutando por novas conexões.</li>
%%%        </ul>
%%%
%%% @returns {ok, pid()} | {error, term()}
%%%          Retorna `{ok, Pid}` se o servidor foi iniciado com sucesso,
%%%          ou `{error, Reason}` caso contrário.
%%%
-spec start_link([]) -> {ok, pid()} | {error, term()}.
start_link([ClientSocket, ListenSocket]) ->
    gen_server:start_link(?MODULE, [ClientSocket, ListenSocket], []).


%%% @doc Inicializa o estado do servidor gen_server para gerenciar uma conexão SCTP.
%%%
%%% Esta função é chamada automaticamente quando o processo gen_server é iniciado.
%%% Ela configura o socket do cliente, obtém informações do peer (cliente remoto),
%%% ativa o modo de recepção assíncrona no socket e agenda o processamento da requisição.
%%%
%%% @param Args {list()} - Lista contendo:
%%%        <ul>
%%%          <li>{@type ClientSocket} - Socket conectado ao cliente.</li>
%%%          <li>{@type ListenSocket} - Socket que aceitou a conexão.</li>
%%%        </ul>
%%%
%%% @returns {ok, State} | {stop, Reason}
%%%          Retorna `{ok, State}` se a inicialização foi bem-sucedida,
%%%          ou `{stop, Reason}` caso ocorra um erro (por exemplo, falha em obter o endereço do cliente).
%%%
-spec init([]) -> {ok, #state{}} | {stop, term()}.
init([ClientSocket, ListenSocket]) ->
    ?LOG_INFO("Iniciando conexao Listen Socket: ~p | Client Socket: ~p | Versão ~p", [ListenSocket, ClientSocket, ?MODULO_VERSAO]),

    case inet:peername(ClientSocket) of
        {ok, {_, _}} ->
            case inet:setopts(ClientSocket, [{active, true}]) of
                ok ->
                    ?LOG_INFO("Socket configurado para modo ativo"),
                    {ok, #state{listenSocket = ListenSocket, clientSocket = ClientSocket}};
                {error, Reason} ->
                    ?LOG_ERROR("Falha ao configurar socket para modo ativo: ~p", [Reason]),
                    gen_tcp:close(ClientSocket),
                    {stop, Reason}
            end;
        {error, Reason} ->
            ?LOG_ERROR("Acceptor ~p: Falha ao obter endereço do cliente: ~p", [self(), Reason]),
            gen_tcp:close(ClientSocket),
            {stop, Reason}
    end.


%%% @doc Manipula chamadas síncronas ao servidor gen_server.
%%%
%%% Esta função é chamada automaticamente quando um cliente envia uma mensagem usando `gen_server:call/2`.
%%% Ela permite tratar requisições síncronas e pode também solicitar o encerramento controlado do servidor.
%%%
%%% @param Request term() - A requisição recebida do cliente.
%%% @param From {pid(), Tag} - Identificador do processo que fez a chamada.
%%% @param State #state{} - Estado atual do servidor gen_server.
%%%
%%% @returns {reply, Reply, NewState} |
%%%          {stop, Reason, Reply, NewState}
%%%
%%%          Retorna:
%%%          <ul>
%%%            <li>{@type {reply, Reply, NewState}} - Resposta à chamada e novo estado.</li>
%%%            <li>{@type {stop, Reason, Reply, NewState}} - Encerra o servidor após responder.</li>
%%%          </ul>
%%%
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} |
    {stop, term(), term(), #state{}}.
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.



%% @doc
%% Manipula mensagens assíncronas enviadas ao servidor gen_server.
%%
%% Esta função é chamada automaticamente quando um cliente ou outro processo envia uma mensagem usando `gen_server:cast/2`.
%% Ela processa as mensagens sem exigir uma resposta imediata.
%%
%% Casos tratados:
%% <ul>
%%   <li>`{command_response, Response}`: envia a resposta ao cliente através do socket TCP.</li>
%%   <li>`_Msg`: qualquer outra mensagem desconhecida é ignorada silenciosamente.</li>
%% </ul>
%%
%% @param Msg term() - A mensagem recebida pelo servidor.
%% @param State #state{} - Estado atual do servidor gen_server.
%%
%% @returns {noreply, NewState}
%% <ul>
%%   <li>{@type {noreply, NewState}} - Não há resposta necessária; o servidor continua com o novo estado.</li>
%% </ul>
%%
%% @see gen_server:cast/2
-spec handle_cast({command_response, term()}, #state{}) -> {noreply, #state{}}.
handle_cast({command_response, Response}, State) -> 
    ?LOG_INFO("Conexão recebeu a resposta, enviando para o cliente..."),
    ok = gen_tcp:send(State#state.clientSocket, list_to_binary(Response)),
    ?LOG_INFO("Resposta enviada para o cliete com sucesso."),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%% @doc Manipula mensagens de dados recebidos via TCP.
%%%
%%% Esta função é chamada automaticamente quando uma mensagem do tipo `{tcp, Socket, Data}`
%%% é recebida pelo processo gen_server. Ela verifica se os dados foram recebidos no socket
%%% do cliente atual e registra as informações no log para depuração.
%%%
%%% @param Message {tuple()} - A mensagem TCP recebida, contendo:
%%%        <ul>
%%%          <li>{@type Socket} - O socket onde os dados foram recebidos.</li>
%%%          <li>{@type Data} - Os dados binários ou bytes recebidos.</li>
%%%        </ul>
%%% @param State #state{} - Estado atual do servidor gen_server.
%%%
%%% @returns {noreply, #state{}} - Retorna sem resposta e mantém o estado inalterado.
%%%
-spec handle_info({tcp | tcp_closed | tcp_error, inet:socket(), term()}, #state{}) -> {noreply, #state{}}.   
handle_info({tcp, Socket, Data}, #state{clientSocket = Socket} = State) ->
    ?LOG_INFO("TCP handler chamado - Socket: ~p, Data: ~p", [Socket, Data]),
    Command = parser_command:parse(Data),
    NewState = handle_command(Command, State),
    {noreply, NewState};

%%% @doc Manipula o fechamento de uma conexão TCP pelo cliente.
%%%
%%% Esta função é chamada automaticamente quando o cliente fecha a conexão TCP,
%%% gerando uma mensagem do tipo `{tcp_closed, Socket}`. O processo gen_server
%%% responde com o término normal da execução, liberando recursos associados à conexão.
%%%
%%% @param Message {tuple()} - A mensagem recebida, contendo:
%%%        <ul>
%%%          <li>{@type Socket} - O socket que foi fechado.</li>
%%%        </ul>
%%% @param State #state{} - Estado atual do servidor gen_server.
%%%
%%% @returns {stop, normal, #state{}} - Solicita a parada normal do gen_server.
%%%
handle_info({tcp_closed, Socket}, #state{clientSocket = Socket} = State) ->
    ?LOG_INFO("Conexão fechada pelo cliente"),
    gen_server:cast(State#state.shellInstancePid, {hibernate}), 
    {stop, normal, State};

%%% @doc Manipula mensagens de erro em uma conexão TCP.
%%%
%%% Esta função é chamada automaticamente quando ocorre um erro em uma conexão TCP,
%%% gerando uma mensagem do tipo `{tcp_error, Socket, Reason}`. O processo gen_server
%%% responde com o término imediato da execução, repassando a razão do erro.
%%%
%%% @param Message {tuple()} - A mensagem de erro recebida, contendo:
%%%        <ul>
%%%          <li>{@type Socket} - O socket onde ocorreu o erro.</li>
%%%          <li>{@type Reason} - Motivo do erro (geralmente um átomo ou string).</li>
%%%        </ul>
%%% @param State #state{} - Estado atual do servidor gen_server.
%%%
%%% @returns {stop, Reason, #state{}} - Solicita a parada do gen_server com a razão do erro.
%%%
handle_info({tcp_error, Socket, Reason}, #state{clientSocket = Socket} = State) ->
    ?LOG_ERROR("Erro na conexão: ~p", [Reason]),
    {stop, Reason, State};

%%% @doc Manipula mensagens inesperadas ou desconhecidas recebidas pelo servidor.
%%%
%%% Esta função é chamada quando o gen_server recebe uma mensagem que não foi tratada
%%% por nenhuma das outras cláusulas de `handle_info/2`. Ela ignora a mensagem e continua
%%% a execução normal do processo.
%%%
%%% @param Message term() - A mensagem recebida (não reconhecida ou não tratada).
%%% @param State #state{} - Estado atual do servidor gen_server.
%%%
%%% @returns {noreply, #state{}} - Retorna sem resposta e mantém o estado inalterado.
%%%
handle_info(_Info, State) ->
    ?LOG_INFO("Connection recebeu mensagem nao tratada: ~p", [_Info]),
    ?LOG_INFO("Connection state: ~p", [State]),
    {noreply, State}.


%%% @doc Função chamada quando o servidor gen_server está sendo encerrado.
%%%
%%% Esta função é invocada automaticamente antes do término do processo gen_server,
%%% seja por término normal ou devido a um erro. É utilizada para liberar recursos,
%%% como fechar o socket TCP associado à conexão com o cliente.
%%%
%%% @param Reason term() - Motivo do término. Pode ser:
%%%        <ul>
%%%          <li>{@type normal} - Término esperado e limpo.</li>
%%%          <li>{@type shutdown} - Desligamento solicitado pelo supervisor.</li>
%%%          <li>{@type {shutdown, term()}} - Desligamento com motivo específico.</li>
%%%          <li>{@type term()} - Qualquer outro valor indica uma falha.</li>
%%%        </ul>
%%% @param State #state{} - Estado atual do servidor gen_server antes do término.
%%%
%%% @returns ok | any() - Deve sempre retornar `ok` a menos que precise registrar erros extras.
%%%
-spec terminate(term(), #state{}) -> {ok}.
terminate(Reason, #state{clientSocket = Socket}) ->
    ?LOG_INFO("Terminando conexão: ~p", [Reason]),
    gen_tcp:close(Socket),
    ok.


%%% @doc Atualiza o estado do servidor durante uma mudança de código em tempo de execução.
%%%
%%% Esta função é chamada automaticamente pelo gen_server quando uma atualização de código
%%% ("hot code loading") ocorre, permitindo que o estado seja convertido ou adaptado à nova versão.
%%%
%%% @param OldVsn term() - Versão anterior do módulo ou `{down, term()}` se estiver revertendo.
%%% @param State #state{} - Estado atual do servidor antes da mudança.
%%% @param Extra term() - Dados extras opcionais passados durante a mudança de código.
%%%
%%% @returns {ok, NewState} - Retorna o estado atualizado (ou inalterado) para uso na nova versão.
%%%
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



% TODO: REFATORAR ISSO, SEM TEMPO AGORA
handle_command({<<"reconnect">>, HashBin}, State = #state{shellInstancePid = undefined}) when is_binary(HashBin) ->
    HashStr = binary_to_list(HashBin),
    case catch list_to_integer(HashStr) of
        HashInt when is_integer(HashInt) ->
            case ets:lookup(connection_table, HashInt) of
                [{HashInt, Pid}] when is_pid(Pid) ->
                    ?LOG_INFO("Encontrou a instância do shell anterior"),
                    ?LOG_INFO("Verificando se processo ~p está vivo: ~p", [Pid, is_process_alive(Pid)]),
                    ?LOG_INFO("Enviando mensagem de reconexão para ~p", [Pid]),
                    ?LOG_INFO("Mensagem sendo enviada: {reconnected, ~p, ~p}", [self(), State#state.clientSocket]),
                    gen_server:cast(Pid, {reconnected, self(), State#state.clientSocket}),
                    State#state{shellInstancePid = Pid};
                [] ->
                    ?LOG_INFO("Não havia instância do shell existente, criando uma nova instância do shell...~n"),
                    ShellPid = rpc_server_shell_manager_sup:start_shell(State#state.clientSocket, self()),
                    State#state{shellInstancePid = ShellPid};
                _ ->
                    ?LOG_INFO("Registro encontrado na ETS, mas PID inválido para hash ~p~n", [HashInt]),
                    State
            end;
        _ ->
            ?LOG_ERROR("Hash inválido recebido para reconexão: ~p", [HashBin]),
            State
    end;

handle_command({<<"reconnect">>, HashBin}, State = #state{shellInstancePid = Pid}) 
    when is_binary(HashBin), is_pid(Pid) ->
      ?LOG_INFO("O cliente já possui uma conexão ativa. Ignorando tentativa de reconexão duplicada."),
      gen_server:cast(self(), {command_response, "Você ja esta conectado.\n"}),
      State;

handle_command(Cmd, State) ->
    NewState = validar_conexao(State),
    gen_server:cast(NewState#state.shellInstancePid, {execute_command, Cmd}),
    io:format("Enviando comando: ~p~n", [Cmd]),
    NewState.

validar_conexao(State = #state{shellInstancePid = undefined}) ->
    ?LOG_INFO("Não havia instância do shell existente, criando uma nova instância do shell...~n"),
    ShellPid = rpc_server_shell_manager_sup:start_shell(State#state.clientSocket, self()),
    State#state{shellInstancePid = ShellPid};

validar_conexao(State) ->
    State.