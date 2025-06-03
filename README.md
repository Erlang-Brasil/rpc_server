# RPC Server

Um servidor RPC SCTP altamente concorrente implementado em Erlang/OTP.

## Arquitetura

O servidor é construído usando uma arquitetura supervisionada em camadas, projetada para alta disponibilidade e escalabilidade. Cada conexão SCTP é gerenciada por um processo dedicado, permitindo processamento não-bloqueante e isolamento de falhas.

### Árvore de Supervisores

```
rpc_server_sup (Root)
└── rpc_server_sctp_listen_sup
    ├── rpc_server_sctp_acceptor_sup
    │   └── rpc_server_sctp_connection (dinâmico, temporary)
    │       └── [Gerencia conexões SCTP individuais]
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

## Módulos

### Supervisores

#### `rpc_server_sup.erl`
- Supervisor raiz da aplicação
- Responsável por iniciar e supervisionar todos os subsistemas
- Gerencia o ciclo de vida da aplicação
- Estratégia: one_for_one

#### `rpc_server_sctp_listen_sup.erl`
- Supervisor do subsistema SCTP
- Gerencia o listener e o supervisor de acceptors
- Garante que o servidor SCTP esteja sempre disponível
- Estratégia: rest_for_one (garante que o acceptor_sup seja iniciado antes do listener)

#### `rpc_server_sctp_acceptor_sup.erl`
- Supervisor dos processos de conexão
- Gerencia o pool de conexões usando simple_one_for_one
- Cria processos temporários para cada conexão
- Responsável por transferir o controle do socket para o processo de conexão

### Workers

#### `rpc_server_sctp_listen.erl`
- Implementa o servidor SCTP principal
- Gerencia o socket de escuta
- Implementa async_accept para melhor escalabilidade
- Coordena a criação de novos processos de conexão
- Mantém o estado do servidor e referências de async_accept
- Aguarda a disponibilidade do supervisor de acceptors antes de iniciar

#### `rpc_server_sctp_connection.erl`
- Gerencia uma conexão SCTP individual
- Processa mensagens recebidas
- Implementa o protocolo RPC
- Gerencia o ciclo de vida da conexão
- Configura o socket para modo ativo
- Trata eventos TCP (dados, fechamento, erros)

## Fluxo de Conexão

1. O listener (`rpc_server_sctp_listen`) inicia escutando na porta configurada
2. Quando uma nova conexão chega:
   - O listener inicia um novo processo de conexão via acceptor_sup
   - O controle do socket é transferido para o processo de conexão
   - O processo de conexão configura o socket para modo ativo
   - O processo de conexão processa as mensagens recebidas
3. O listener inicia um novo async_accept para a próxima conexão

## Características

- **Alta Concorrência**: Cada conexão é gerenciada por um processo dedicado
- **Não-Bloqueante**: Uso de async_accept para melhor escalabilidade
- **Fault Tolerance**: Isolamento de falhas através da árvore de supervisores
- **Resource Management**: Processos são criados/destruídos conforme necessário
- **High Availability**: Supervisores reiniciam componentes críticos

## Configuração

O servidor pode ser configurado através do arquivo `config/sys.config`:

```erlang
{rpc_server, [
    {SCTP_port, 8080},
    {backlog, 1024}
]}
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
│   └── rpc_server_sctp_acceptor_sup.erl
│
├── sctp/                  # Implementação SCTP
│   └── rpc_server_sctp_listen.erl
│
└── connections/          # Gerenciamento de conexões
    └── rpc_server_sctp_connection.erl
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
