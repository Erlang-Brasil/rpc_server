%%%-------------------------------------------------------------------
%%% @author Fernando Areias <nando.calheirosx@gmail.com>
%%% @copyright (C) 2024, Erlang Brasil
%%% @doc Common definitions for RPC Server
%%% @end
%%%-------------------------------------------------------------------

-ifndef(RPC_SERVER_HRL).
-define(RPC_SERVER_HRL, true).

-include_lib("kernel/include/logger.hrl").

%% Types
-type socket() :: port().

%% Application environment keys
-define(TCP_PORT, tcp_port).
-define(TCP_LISTEN_OPTIONS, tcp_listen_options).
-define(TCP_CONNECTION_OPTIONS, tcp_connection_options).

%% Default values
-define(DEFAULT_TCP_PORT, 8080).
-define(DEFAULT_TCP_LISTEN_OPTIONS, [
    binary,     
    {packet, raw},        % 0 = raw - Sem cabeçalho, dados brutos 
    {reuseaddr, true},    % Permite reutilizar o endereço mesmo se estiver em estado TIME_WAIT
    {active, true},       % Modo ativo total (passagem automática de mensagens)
    {backlog, 128}        % Número máximo de conexões pendentes na fila
]).
-define(DEFAULT_TCP_CONNECTION_OPTIONS, [
    binary,
    {packet, raw},        
    {active, false}, 
    {keepalive, true},      % Mantém a conexão ativa usando TCP keepalive
    {send_timeout, 5000},   % Define um timeout de 5 segundos para operações de envio
    {send_timeout_close, true},
    {exit_on_close, true}   % Encerra o processo quando o socket é fechado 
]).

%% Timeouts
-define(ACCEPT_TIMEOUT, 5000).
-define(CONNECTION_TIMEOUT, 5000).
-define(HANDLER_TIMEOUT, 5000).

-endif. 