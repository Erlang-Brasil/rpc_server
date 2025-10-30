# RPC Server

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
│   ├── dev_vm.config
│   ├── dev_http.vm.args
│   └── dev_shell.vm.args
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

### Desenvolvimento (interativo)

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

Arquivo principal: `config/dev_sys.config`.

Parâmetros relevantes do `rpc_server_http` (padrões):

```erlang
{rpc_server_http, [
  {tcp_port, 8080},
  {num_acceptors, 100},
  {max_connections, 10000}
]}
```

Nomes de nós e cookie (releases e dev):

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