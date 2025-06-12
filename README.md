# RPC Server

Um servidor RPC STCP altamente concorrente implementado em Erlang/OTP.

## Arquitetura

O servidor é construído usando uma arquitetura supervisionada em camadas, projetada para alta disponibilidade e escalabilidade. Cada conexão STCP é gerenciada por um processo dedicado, permitindo processamento não-bloqueante e isolamento de falhas.

### Árvore de Supervisores

```
rpc_server_sup (Root)
├── rpc_server_shell_manager_sup
│   └── rpc_server_shell_instance (dinâmico, temporary)
│       └── [Gerencia instâncias do shell interativo]
│
└── rpc_server_sctp_listen_sup
    ├── rpc_server_sctp_acceptor_sup
    │   └── rpc_server_sctp_connection (dinâmico, temporary)
    │       └── [Gerencia conexões STCP individuais]
    │
    └── rpc_server_sctp_listen
        └── [Gerencia socket de escuta e async_accept]

Legenda:
- (dinâmico) - Processos criados sob demanda
- (temporary) - Processos que não são reiniciados automaticamente
```

### Estratégias de Supervisão

- `rpc_server_sup`: one_for_one
- `rpc_server_sctp_listen_sup`: rest_for_one
- `rpc_server_sctp_acceptor_sup`: simple_one_for_one
- `rpc_server_shell_manager_sup`: simple_one_for_one

## Módulos

### Supervisores

#### `rpc_server_sup.erl`
- Supervisor raiz da aplicação
- Responsável por iniciar e supervisionar todos os subsistemas
- Gerencia o ciclo de vida da aplicação
- Estratégia: one_for_one

#### `rpc_server_sctp_listen_sup.erl`
- Supervisor do subsistema STCP
- Gerencia o listener e o supervisor de acceptors
- Garante que o servidor STCP esteja sempre disponível
- Estratégia: rest_for_one (garante que o acceptor_sup seja iniciado antes do listener)

#### `rpc_server_sctp_acceptor_sup.erl`
- Supervisor dos processos de conexão
- Gerencia o pool de conexões usando simple_one_for_one
- Cria processos temporários para cada conexão
- Responsável por transferir o controle do socket para o processo de conexão

#### `rpc_server_shell_manager_sup.erl`
- Supervisor das instâncias do shell interativo
- Gerencia o pool de shells usando simple_one_for_one
- Cria processos temporários para cada instância de shell
- Responsável por iniciar novas instâncias de shell conforme necessário

### Workers

#### `rpc_server_sctp_listen.erl`
- Implementa o servidor STCP principal
- Gerencia o socket de escuta
- Implementa async_accept para melhor escalabilidade
- Coordena a criação de novos processos de conexão
- Mantém o estado do servidor e referências de async_accept
- Aguarda a disponibilidade do supervisor de acceptors antes de iniciar

#### `rpc_server_sctp_connection.erl`
- Gerencia uma conexão STCP individual
- Processa mensagens recebidas
- Implementa o protocolo RPC
- Gerencia o ciclo de vida da conexão
- Configura o socket para modo ativo
- Trata eventos STCP (dados, fechamento, erros)

#### `rpc_server_shell_instance.erl`
- Gerencia uma instância do shell interativo
- Fornece interface de linha de comando para administração
- Permite monitoramento e controle do servidor em tempo real
- Implementa comandos administrativos via shell
- Gerencia o sistema de reconexão baseado em hash

#### `parser_command.erl`
- Responsável por fazer o parsing de comandos recebidos via rede
- Remove caracteres de controle como \r e \n
- Divide comandos com base no caractere ":" para identificar comandos com ou sem valor
- Suporta formatos: `<<"comando:valor">>` e `<<"comando">>`

## Fluxo de Conexão

1. O listener (`rpc_server_sctp_listen`) inicia escutando na porta configurada
2. Quando uma nova conexão chega:
   - O listener inicia um novo processo de conexão via acceptor_sup
   - O controle do socket é transferido para o processo de conexão
   - O processo de conexão configura o socket para modo ativo
   - O processo de conexão processa as mensagens recebidas
3. O listener inicia um novo async_accept para a próxima conexão

## Sistema de Reconexão

O servidor implementa um sistema inteligente de reconexão baseado em hash que permite que clientes se reconectem e retomem suas sessões anteriores.

### Como Funciona

1. **Identificação Única**: Quando um cliente se conecta pela primeira vez, o servidor gera um hash único baseado no IP e porta do cliente usando o algoritmo DJB hash.

2. **Armazenamento**: O hash e o PID da instância do shell são armazenados em uma tabela ETS (`connection_table`) para referência futura.

3. **Estado de Desconexão**: Quando a conexão é perdida, a instância do shell permanece ativa mas é marcada como "desconectada".

4. **Reconexão**: Quando o cliente se reconecta e envia `reconnect:hash` como primeira mensagem, o servidor:
   - Busca o hash na tabela ETS
   - Verifica se a instância do shell anterior ainda está ativa
   - Reconecta o cliente à instância existente
   - Restaura o estado da sessão

### Exemplo de Uso

```bash
# Primeira conexão
$ telnet localhost 8080
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Client identification 123456789

# Execute alguns comandos
ls
pwd

# Desconecte (Ctrl+C ou feche a conexão)
^]
telnet> quit
Connection closed.

# Reconecte usando o hash recebido
$ telnet localhost 8080
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.

# Envie o comando de reconexão como primeira mensagem
reconnect:123456789
Conexao restabelecida

# Agora você está reconectado à mesma sessão
```

### Formato do Comando de Reconexão

```
reconnect:HASH
```

Onde `HASH` é o identificador numérico recebido na primeira conexão.

### Benefícios

- **Persistência de Sessão**: Mantém o estado e histórico de comandos
- **Recuperação Automática**: Reconecta automaticamente à sessão anterior
- **Eficiência**: Evita recriar instâncias desnecessariamente
- **Robustez**: Funciona mesmo com interrupções de rede temporárias

## Características

- **Alta Concorrência**: Cada conexão é gerenciada por um processo dedicado
- **Não-Bloqueante**: Uso de async_accept para melhor escalabilidade
- **Fault Tolerance**: Isolamento de falhas através da árvore de supervisores
- **Resource Management**: Processos são criados/destruídos conforme necessário
- **High Availability**: Supervisores reiniciam componentes críticos
- **Reconexão Inteligente**: Sistema de reconexão baseado em hash para persistência de sessão

## Configuração

O servidor pode ser configurado através do arquivo `config/sys.config`:

```erlang
[
    {kernel, [
        {logger_level, info}
    ]},
    {rpc_server, [
        {tcp_port, 8080},
        {num_acceptors, 100},     % Número de processos aceitando conexões
        {max_connections, 10000}, % Limite máximo de conexões simultâneas
        {tcp_listen_options, [    % Esse é utilizado no listener global
            binary,     
            {packet, raw},        % 0 = raw - Sem cabeçalho, dados brutos 
            {reuseaddr, true},    % Permite reutilizar o endereço mesmo se estiver em estado TIME_WAIT
            {active, true},       % Modo ativo total (passagem automática de mensagens)
            {backlog, 128}        % Número máximo de conexões pendentes na fila
        ]},
        {tcp_connection_options, [ % Esse é utilizado para cada conexão individual
            binary,
            {packet, raw},        
            {active, false}, 
            {keepalive, true},      % Mantém a conexão ativa usando STCP keepalive
            {send_timeout, 5000},   % Define um timeout de 5 segundos para operações de envio
            {send_timeout_close, true},
            {exit_on_close, true}   % Encerra o processo quando o socket é fechado 
        ]}
    ]}
].
```

## Uso

1. Compile o projeto:
```bash
rebar3 compile
```

2. Inicie o servidor:
```bash
rebar3 shell
```

3. O servidor estará escutando na porta configurada (padrão: 8080)

4. Use o shell interativo para administração:
```erlang
% No shell do Erlang
rpc_server_shell:start().
```

### Comandos do Shell

O shell interativo fornece os seguintes comandos:
- `help` - Lista todos os comandos disponíveis
- `status` - Mostra o status atual do servidor
- `connections` - Lista todas as conexões ativas
- `stats` - Exibe estatísticas do servidor

## Logs

O servidor utiliza o sistema de logging do Erlang/OTP. Logs importantes incluem:
- Inicialização do servidor
- Novas conexões
- Erros de conexão
- Processamento de mensagens
- Fechamento de conexões

## Desenvolvimento

### Estrutura de Diretórios

```
rpc_server/
├── app/                   # Módulos da aplicação principal
│   ├── rpc_server_app.erl # Callback do comportamento application
│   └── rpc_server_sup.erl # Supervisor raiz
│
├── supervisors/           # Hierarquia de supervisores
│   ├── rpc_server_sctp_listen_sup.erl
│   ├── rpc_server_sctp_acceptor_sup.erl
│   └── rpc_server_shell_manager_sup.erl
│
├── sctp/                  # Implementação STCP (mantido nome por compatibilidade)
│   └── rpc_server_sctp_listen.erl
│
├── shell/                # Interface de administração
│   └── rpc_server_shell_instance.erl
│
├── connections/          # Gerenciamento de conexões
│   └── rpc_server_sctp_connection.erl
│
└── commands/            # Parser de comandos
    └── parser_command.erl
```

### Dependências

- Erlang/OTP 27 ou superior
- rebar3 para build

### Testes

```bash
rebar3 eunit
rebar3 ct
```

## Contribuição

1. Fork o projeto
2. Crie uma branch para sua feature (`git checkout -b feature/xpto`)
3. Commit suas mudanças (`git commit -m 'Add some xpto'`)
4. Push para a branch (`git push origin feature/xpto`)
5. Abra um Pull Request

## Licença

Este projeto está licenciado sob a MIT License - veja o arquivo LICENSE para detalhes.
