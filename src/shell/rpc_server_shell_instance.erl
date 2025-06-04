-module(rpc_server_shell_instance).

-behaviour(gen_server).

-include("rpc_server.hrl").

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    socket :: socket() | undefined,
    connection :: pid() | undefined
}).

start_link([ClientSocket, ConnectionPid]) ->
    gen_server:start_link(?MODULE, [ClientSocket, ConnectionPid], []).

init([ClientSocket, ConnectionPid]) ->
    ?LOG_INFO("Init shell ClientSocket ~p | ConnectionPid ~p", [ClientSocket, ConnectionPid]),
    {ok, #state{connection = ConnectionPid, socket = ClientSocket}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({execute_command, Command}, State = #state{socket = Socket}) ->
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
    
    %% TODO: REFATORAR PARA O CONNECTION PODER RECEBER O RESULTADO DO COMANDO
    Response = io_lib:format("~s~n", [Output]),
    gen_tcp:send(Socket, list_to_binary(Response)),    
    
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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