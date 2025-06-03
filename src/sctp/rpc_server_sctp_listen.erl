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


%%% @doc Inicializa o estado do servidor gen_server responsável por escutar novas conexões.
%%%
%%% Esta função é chamada durante a inicialização do processo gen_server. Ela prepara o ambiente
%%% para aceitar novas conexões, garantindo que o supervisor de acceptors esteja disponível
%%% e iniciando o processo de escuta em um socket.
%%%
%%% @param Args term() - Argumentos de inicialização (espera-se uma lista vazia `[]` neste caso).
%%%
%%% @returns {ok, State} | {stop, Reason}
%%%          Retorna `{ok, State}` se a inicialização foi bem-sucedida,
%%%          ou `{stop, Reason}` caso contrário (por exemplo, falha ao localizar o supervisor).
%%%
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


%%% @doc Manipula chamadas síncronas ao servidor gen_server.
%%%
%%% Esta função é chamada quando um cliente faz uma chamada usando `gen_server:call/2`.
%%% Ela permite tratar requisições síncronas e pode também solicitar o encerramento controlado do servidor.
%%%
%%% @param Request term() - A requisição recebida do cliente.
%%% @param From {pid(), Tag} - Tupla identificando o processo que fez a chamada.
%%% @param State #state{} - Estado atual do servidor gen_server.
%%%
%%% @returns {reply, Reply, NewState} |
%%%          {stop, Reason, Reply, NewState}
%%%
%%%          <ul>
%%%            <li>{@type {reply, Reply, NewState}} - Responde à chamada e atualiza o estado.</li>
%%%            <li>{@type {stop, Reason, Reply, NewState}} - Encerra o servidor após responder.</li>
%%%          </ul>
%%%
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} |
    {stop, term(), term(), #state{}}.

handle_call(stop, _From, State) ->
    ?LOG_INFO("Recebido pedido de parada. Encerrando servidor..."),
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    ?LOG_DEBUG("Requisição desconhecida ignorada: ~p", [_Request]),
    {reply, ok, State}.

%%% @doc Manipula mensagens assíncronas enviadas ao servidor gen_server.
%%%
%%% Esta função é chamada quando uma mensagem é enviada ao servidor usando `gen_server:cast/2`.
%%% Diferente de `handle_call/3`, não há resposta imediata ao remetente.
%%%
%%% @param Msg term() - A mensagem recebida pelo servidor.
%%% @param State #state{} - Estado atual do servidor gen_server.
%%%
%%% @returns {noreply, NewState} - Continua a execução sem resposta.
%%%
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.

handle_cast(_Msg, State) ->
    ?LOG_DEBUG("Mensagem não tratada ignorada: ~p", [_Msg]),
    {noreply, State}.

%%% @doc Função chamada quando o servidor gen_server está sendo encerrado.
%%%
%%% Esta função é invocada automaticamente antes do término do processo gen_server,
%%% seja por término normal ou devido a um erro. Neste caso, ela não realiza nenhuma ação adicional,
%%% mas pode ser estendida para liberar recursos (como fechar sockets ou cancelar timers).
%%%
%%% @param Reason term() - Motivo do término. Pode ser:
%%%        <ul>
%%%          <li>{@type normal} - Término esperado.</li>
%%%          <li>{@type shutdown} - Desligamento solicitado pelo supervisor.</li>
%%%          <li>{@type {shutdown, term()}} - Desligamento com motivo específico.</li>
%%%          <li>{@type term()} - Qualquer outro valor indica uma falha.</li>
%%%        </ul>
%%% @param State #state{} - Estado atual do servidor gen_server antes do término.
%%%
%%% @returns ok - Sempre retorna `ok`.
%%%
-spec terminate(term(), #state{}) -> ok.

terminate(_Reason, _State) ->
    ok.

%%% @doc Atualiza o estado do servidor durante uma mudança de código em tempo de execução.
%%%
%%% Esta função é chamada automaticamente pelo gen_server durante uma atualização de código
%%% ("hot code loading"), permitindo que o estado seja convertido ou adaptado à nova versão.
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


%%% @doc Manipula eventos assíncronos de aceitação de nova conexão TCP.
%%%
%%% Esta função é chamada quando um novo cliente se conecta ao servidor e o evento é recebido
%%% através de uma mensagem do tipo `{inet_async, ListenSocket, Ref, {ok, ClientSocket}}`.
%%% O processo inicia um acceptor dedicado à nova conexão e prepara o socket para futuras conexões.
%%%
%%% @param Message {inet_async, ListenSocket, Ref, {ok, ClientSocket}} - Evento de nova conexão.
%%% @param State #state{} - Estado atual do servidor gen_server.
%%%
%%% @returns {noreply, NewState} | {stop, Reason, NewState}
%%%          Retorna `{noreply, NewState}` se a conexão foi aceita com sucesso,
%%%          ou `{stop, Reason, NewState}` caso ocorra falha na configuração do socket.
%%%

-spec handle_info({inet_async, inet:socket(), reference(), {ok, inet:socket()}}, #state{}) ->
    {noreply, #state{}} |
    {stop, term(), #state{}}.
handle_info({inet_async, ListenSocket, Ref, {ok, ClientSocket}}, #state{socket = ListenSocket, ref = Ref, acceptor_sup = _} = State) ->
    ?LOG_INFO("Nova conexão recebida de ~p", [inet:peername(ClientSocket)]),
    case set_sockopt(ListenSocket, ClientSocket) of
        ok ->
            case rpc_server_sctp_acceptor_sup:start_acceptor(ClientSocket, ListenSocket) of
                {ok} ->
                    {ok, NewRef} = prim_inet:async_accept(ListenSocket, -1),
                    {noreply, State#state{ref = NewRef}};
                {error, Reason} ->
                    ?LOG_ERROR("Falha ao iniciar acceptor: ~p", [Reason]),
                    gen_tcp:close(ClientSocket),
                    {stop, Reason, State}
            end;
        {error, Reason} ->
            ?LOG_ERROR("Erro ao configurar opções do socket: ~p", [Reason]),
            gen_tcp:close(ClientSocket),
            {stop, Reason, State}
    end;


%%% @doc Manipula erros durante o processo assíncrono de aceitação de conexão.
%%%
%%% Chamada quando ocorre falha em obter uma nova conexão (ex: erro no socket de escuta).
%%% Gera um log de erro e solicita o encerramento do servidor gen_server.
%%%
%%% @param Message {inet_async, ListenSocket, Ref, Error} - Mensagem de erro assíncrono.
%%% @param State #state{} - Estado atual do servidor gen_server.
%%%
%%% @returns {stop, exceeded_accept_retry_count, State} - Solicita o término do servidor.
%%%
handle_info({inet_async, ListenSocket, Ref, Error}, #state{socket = ListenSocket, ref = Ref} = State) ->
    ?LOG_ERROR("Erro no acceptor do socket: ~p", [Error]),
    {stop, exceeded_accept_retry_count, State};

%%% @doc Manipula mensagens desconhecidas ou não esperadas recebidas pelo servidor.
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
    ?LOG_INFO("Listen recebeu mensagem nao tratada ~p", [_Info]),
    {noreply, State}.

%%% @doc Aguarda até que o supervisor de acceptors esteja disponível.
%%%
%%% Esta função realiza tentativas periódicas para localizar o processo registrado como
%%% `rpc_server_sctp_acceptor_sup`. Se não encontrar após o número máximo de tentativas,
%%% retorna um erro. É útil durante a inicialização do servidor para garantir que os recursos
%%% necessários estejam disponíveis antes de prosseguir.
%%%
%%% @param Attempts integer() - Número máximo de tentativas restantes (começa com um valor positivo).
%%%
%%% @returns {ok, Pid} | {error, acceptor_supervisor_not_found}
%%%          Retorna `{ok, Pid}` se o supervisor for encontrado,
%%%          ou `{error, acceptor_supervisor_not_found}` caso contrário.
%%%

-spec wait_for_acceptor_sup(non_neg_integer()) ->
    {ok, pid()} | {error, acceptor_supervisor_not_found}.
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


%%% @doc Inicia o processo de escuta em um socket TCP.
%%%
%%% Esta função configura e abre um socket TCP para escutar conexões na porta definida
%%% nas variáveis de ambiente ou usa uma porta padrão. Após abrir o socket com sucesso,
%%% ela passa o controle ao handler responsável por aceitar conexões assíncronas.
%%%
%%% @param AcceptorSup pid() - PID do supervisor responsável por gerenciar os acceptors.
%%%
%%% @returns {ok, State} | {stop, Reason}
%%%          Retorna `{ok, State}` se o socket foi aberto com sucesso,
%%%          ou `{stop, Reason}` caso contrário (por exemplo, falha ao escutar na porta).
%%%

-spec start_listening(pid()) -> {ok, #state{}} | {stop, term()}.
start_listening(AcceptorSup) ->
    Port = application:get_env(rpc_server, tcp_port, 8080),
    Options = [
        binary,
        {reuseaddr, true},
        {keepalive, true},
        {backlog, 128},
        {active, false}
    ],
    ?LOG_INFO("Tentando escutar na porta ~p com opções ~p", [Port, Options]),
    handle_listening(gen_tcp:listen(Port, Options), AcceptorSup).


%%% @doc Trata o resultado da tentativa de iniciar um socket TCP para escuta.
%%%
%%% Esta função é chamada após a execução de `gen_tcp:listen/2`. Ela pode receber:
%%% - `{ok, Socket}`: caso o socket tenha sido aberto com sucesso;
%%% - `{error, Reason}`: caso ocorra falha ao abrir o socket (ex: porta em uso).
%%%
%%% No caso de sucesso, obtém informações do socket e prepara para aceitação assíncrona.
%%% No caso de erro, registra uma mensagem e retorna `{stop, Reason}`.
%%%
%%% @param Result {ok, inet:socket()} | term() - Resultado da chamada a `gen_tcp:listen/2`.
%%% @param AcceptorSup pid() - PID do supervisor responsável por gerenciar acceptors.
%%%
%%% @returns {ok, #state{}} | {stop, Reason}
%%%          Retorna `{ok, State}` se o socket foi configurado corretamente,
%%%          ou `{stop, Reason}` caso ocorra falha (ex: não conseguir obter sockname ou erro no listen).
%%%

-spec handle_listening({ok, inet:socket()} | term(), pid()) ->
    {ok, #state{}} | {stop, term()}.
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


%%% @doc Configura opções de socket para uma nova conexão TCP.
%%%
%%% Esta função copia opções de configuração (como `active`, `nodelay`, etc)
%%% do socket de escuta (`ListenSocket`) para o socket recém-aceito (`ClientSocket`),
%%% garantindo consistência nas propriedades de rede entre os dois sockets.
%%%
%%% @param ListenSocket inet:socket() - Socket que está escutando por conexões.
%%% @param ClientSocket inet:socket() - Socket conectado ao cliente recém-aceito.
%%%
%%% @returns ok | {error, Reason}
%%%          Retorna `ok` se todas as opções foram configuradas com sucesso,
%%%          ou `{error, Reason}` caso contrário (por exemplo, erro ao ler ou definir opções).
%%%

-spec set_sockopt(inet:socket(), inet:socket()) ->
    ok | {error, term()}.
set_sockopt(ListenSocket, ClientSocket) ->
    true = inet_db:register_socket(ClientSocket, inet_tcp),
    case prim_inet:getopts(ListenSocket, [nodelay, keepalive, delay_send, priority, tos]) of
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

 