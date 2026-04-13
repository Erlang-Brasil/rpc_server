# RPC Server

[![[Build & Test] Status](https://github.com/Erlang-Brasil/rpc_server/actions/workflows/build.yml/badge.svg)](https://github.com/Erlang-Brasil/rpc_server/actions/workflows/build.yml)
[![[Dialyzer] Status](https://github.com/Erlang-Brasil/rpc_server/actions/workflows/dialyzer.yml/badge.svg)](https://github.com/Erlang-Brasil/rpc_server/actions/workflows/dialyzer.yml)

Servidor RPC TCP altamente concorrente em Erlang/OTP, composto por dois apps: `rpc_server_http` (conexões TCP) e `rpc_server_shell` (execução de comandos e sessões persistentes).

## Estrutura do projeto

```
rpc_server/
├── apps/
│   ├── rpc_server_http/
│   │   ├── src/
│   │   │   ├── rpc_server_http_app.erl
│   │   │   ├── rpc_server_http_listen_sup.erl
│   │   │   ├── rpc_server_http_acceptor_sup.erl
│   │   │   ├── rpc_server_http_listen.erl
│   │   │   ├── rpc_server_http_connection.erl
│   │   │   └── rpc_server_http_parser_command.erl
│   │   └── include/rpc_server.hrl
│   └── rpc_server_shell/
│       ├── src/
│       │   ├── rpc_server_shell_app.erl
│       │   ├── rpc_server_shell_sup.erl
│       │   ├── rpc_server_shell_manager_sup.erl
│       │   └── rpc_server_shell_instance.erl
│       └── include/rpc_server.hrl
├── config/
│   ├── dev_sys.config
│   ├── dev_sys_quic.config
│   ├── dev_vm.config
│   ├── dev_vm_quic.config
│   ├── dev_http.vm.args
│   ├── dev_http_quic.vm.args
│   ├── dev_shell.vm.args
│   └── dev_shell_quic.vm.args
├── certs/
│   ├── generate.sh
│   ├── cert.pem
│   └── key.pem
├── start.sh
└── rebar.config
```

## Arquitetura (alto nível)

- `rpc_server_http`:
  - `rpc_server_http_listen_sup` (rest_for_one): supervisiona `rpc_server_http_acceptor_sup` e `rpc_server_http_listen`.
  - `rpc_server_http_acceptor_sup` (simple_one_for_one): cria `rpc_server_http_connection` (temporary) por conexão.
  - `rpc_server_http_listen`: abre o socket TCP e faz `async_accept` (padrão porta 8080).
  - `rpc_server_http_connection`: trata I/O, parse de comandos e orquestra o shell.

- `rpc_server_shell`:
  - `rpc_server_shell_sup` (one_for_all): supervisiona `rpc_server_shell_manager_sup`.
  - `rpc_server_shell_manager_sup` (simple_one_for_one): cria `rpc_server_shell_instance` (temporary).
  - `rpc_server_shell_instance`: executa comandos do sistema e gerencia reconexão da sessão.

## Requisitos

- Erlang/OTP 27+
- `rebar3`

## Build

```bash
rebar3 compile
```

## Como rodar

O script `start.sh` permite iniciar cada nó com o protocolo de distribuição desejado: **TCP** (padrão Erlang) ou **QUIC** (RFC 9000, via [erlang_quic](https://github.com/benoitc/erlang_quic)).

### Pré-requisitos para QUIC

Gere os certificados TLS (necessários apenas uma vez):

```bash
cd certs && bash generate.sh
```

### Usando o start.sh

```bash
# TCP (padrão)
./start.sh --node http                    # nó HTTP na porta 8080
./start.sh --node shell                   # nó Shell
./start.sh --node default                 # ambos os apps no mesmo nó

# QUIC
./start.sh --node http --protocol quic    # nó HTTP com distribuição QUIC (porta dist 4434)
./start.sh --node shell --protocol quic   # nó Shell com distribuição QUIC (porta dist 4435)
./start.sh --node default --protocol quic # ambos os apps com QUIC (porta dist 4433)
```

Cada tipo de nó usa uma porta QUIC diferente para evitar conflito no mesmo host:

| Nó | Porta QUIC |
|----|------------|
| default | 4433 |
| http | 4434 |
| shell | 4435 |

> **Importante:** Todos os nós do cluster devem usar o mesmo protocolo. Nós QUIC e TCP não se conectam entre si.

### Desenvolvimento (interativo, sem script)

```bash
rebar3 shell --config config/dev_sys.config
```

Os dois apps sobem no mesmo nó (nome e cookie definidos em `config/dev_vm.config`).

### Release único (ambos os apps)

```bash
rebar3 release
./_build/default/rel/rpc_server/bin/rpc_server console
# para parar
./_build/default/rel/rpc_server/bin/rpc_server stop
```

### Releases separados (um nó por app)

- HTTP somente:
```bash
rebar3 as http_node release -n rpc_server_http
./_build/http_node/rel/rpc_server_http/bin/rpc_server_http console
```

- SHELL somente:
```bash
rebar3 as shell_node release -n rpc_server_shell
./_build/shell_node/rel/rpc_server_shell/bin/rpc_server_shell console
```

- Parar:
```bash
./_build/http_node/rel/rpc_server_http/bin/rpc_server_http stop
./_build/shell_node/rel/rpc_server_shell/bin/rpc_server_shell stop
```

## Configuração

Arquivo principal: `config/dev_sys.config` (TCP) ou `config/dev_sys_quic.config` (QUIC).

Parâmetros relevantes do `rpc_server_http` (padrões):

```erlang
{rpc_server_http, [
  {tcp_port, 8080},
  {num_acceptors, 100},
  {max_connections, 10000}
]}
```

### Configuração QUIC

O `dev_sys_quic.config` inclui a seção de distribuição QUIC:

```erlang
{quic, [
  {dist, [
    {cert_file, "/path/to/cert.pem"},
    {key_file, "/path/to/key.pem"},
    {verify, verify_none},
    {discovery_module, quic_discovery_static},
    {nodes, [
      {'rpc_server@127.0.0.1', {"127.0.0.1", 4433}},
      {'rpc_server_http@127.0.0.1', {"127.0.0.1", 4434}},
      {'rpc_server_shell@127.0.0.1', {"127.0.0.1", 4435}}
    ]}
  ]}
]}
```

### Nomes de nós e cookie

- Nó único: `config/dev_vm.config` (ex.: `-name rpc_server@127.0.0.1`, cookie `service_discovery_cookie`).
- HTTP: `config/dev_http.vm.args` (ex.: `-name rpc_server_http@127.0.0.1`).
- SHELL: `config/dev_shell.vm.args` (ex.: `-name rpc_server_shell@127.0.0.1`).

Logs são configurados via `lager` em `config/dev_sys.config` e gravados em `log/`.

## Uso rápido

1) Conecte via telnet/netcat na porta configurada (padrão 8080):

```bash
telnet localhost 8080
# ou
nc localhost 8080
```

2) Na primeira conexão, o servidor envia uma identificação do cliente (hash baseado em IP:porta).

3) Para reconectar à mesma sessão, envie como primeira mensagem:

```
reconnect:HASH
```

Em seguida, use comandos de sistema simples (ex.: `ls`, `pwd`). A saída é devolvida na mesma conexão.

## Testes

```bash
rebar3 eunit
rebar3 ct
```

## Contribuição

1. Faça fork
2. Crie uma branch (`git checkout -b feature/xpto`)
3. Commit (`git commit -m 'feat: xpto'`)
4. Push (`git push origin feature/xpto`)
5. Abra um PR

## Licença

MIT. Veja `LICENSE.md`.
