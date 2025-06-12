-module(parser_command).

-author('Fernando Areias <nando.calheirosx@gmail.com>').
-export([parse/1]).

-define(MODULO_VERSAO, 1).
-vsn(?MODULO_VERSAO).



%% ====================================================================
%% @doc
%% Função responsável por fazer o parsing de um comando recebido via rede.
%%
%% A função remove caracteres de controle como \\r e \\n, e divide o binário
%% com base no caractere ":" para identificar comandos com ou sem valor associado.
%%
%% <ul>
%%   <li>Se o formato for `<<"comando:valor">>`, retorna `{Comando, Valor}`.</li>
%%   <li>Se o formato for apenas `<<"comando">>`, retorna o binário `Comando`.</li>
%% </ul>
%%
%% @end
%%
%% @spec parse(binary()) -> {Command :: binary(), Value :: binary()} | Command :: binary()
%% @param Command Comando recebido como binário, possivelmente contendo `:` e caracteres de nova linha.
%% @return Retorna uma tupla `{Command, Value}` se houver valor, ou apenas `Command` caso contrário.
%% @see parse/1
%% ====================================================================
-spec parse(binary()) -> {binary(), binary()} | binary().
parse(Command) ->
    Cleaned = binary:replace(Command, <<"\r">>, <<>>, [global]),
    Cleaned2 = binary:replace(Cleaned, <<"\n">>, <<>>, [global]),
    case binary:split(Cleaned2, <<":">>, [global]) of
        [Cmd, Value] -> {Cmd, Value};
        [Cmd] -> Cmd
    end.