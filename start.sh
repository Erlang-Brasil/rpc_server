#!/bin/bash
set -e

PROTOCOL="tcp"
NODE_TYPE="default"

usage() {
    echo "Usage: $0 --node <http|shell|default> [--protocol <tcp|quic>]"
    echo ""
    echo "Options:"
    echo "  --node       Node type: http, shell, or default (default: default)"
    echo "  --protocol   Distribution protocol: tcp or quic (default: tcp)"
    echo ""
    echo "Examples:"
    echo "  $0 --node http --protocol quic"
    echo "  $0 --node shell --protocol tcp"
    echo "  $0 --node http"
    exit 1
}

while [[ $# -gt 0 ]]; do
    case $1 in
        --protocol)
            PROTOCOL="$2"
            shift 2
            ;;
        --node)
            NODE_TYPE="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
done

if [[ "$PROTOCOL" != "tcp" && "$PROTOCOL" != "quic" ]]; then
    echo "Error: --protocol must be 'tcp' or 'quic'"
    exit 1
fi

if [[ "$PROTOCOL" == "quic" && ! -f certs/cert.pem ]]; then
    echo "Error: QUIC certificates not found. Run: cd certs && bash generate.sh"
    exit 1
fi

case "$NODE_TYPE" in
    http)
        NODE_NAME="rpc_server_http@127.0.0.1"
        PROFILE="http_node"
        APPS="rpc_server_http"
        if [[ "$PROTOCOL" == "quic" ]]; then
            SYS_CONFIG="config/dev_sys_quic.config"
            QUIC_PORT=4434
        else
            SYS_CONFIG="config/dev_sys.config"
        fi
        ;;
    shell)
        NODE_NAME="rpc_server_shell@127.0.0.1"
        PROFILE="shell_node"
        APPS="rpc_server_shell"
        if [[ "$PROTOCOL" == "quic" ]]; then
            SYS_CONFIG="config/dev_sys_quic.config"
            QUIC_PORT=4435
        else
            SYS_CONFIG="config/dev_sys.config"
        fi
        ;;
    default)
        NODE_NAME="rpc_server@127.0.0.1"
        PROFILE=""
        APPS="rpc_server_http,rpc_server_shell"
        if [[ "$PROTOCOL" == "quic" ]]; then
            SYS_CONFIG="config/dev_sys_quic.config"
            QUIC_PORT=4433
        else
            SYS_CONFIG="config/dev_sys.config"
        fi
        ;;
    *)
        echo "Error: --node must be 'http', 'shell', or 'default'"
        exit 1
        ;;
esac

EXTRA_FLAGS=""
if [[ "$PROTOCOL" == "quic" ]]; then
    CERT_PATH="$(pwd)/certs/cert.pem"
    KEY_PATH="$(pwd)/certs/key.pem"
    EXTRA_FLAGS="-proto_dist quic -epmd_module quic_epmd -start_epmd false -quic_dist_port $QUIC_PORT -quic_dist_cert $CERT_PATH -quic_dist_key $KEY_PATH"
fi

echo "=== rpc_server ==="
echo "  Node:     $NODE_TYPE ($NODE_NAME)"
echo "  Protocol: $PROTOCOL"
echo "  Config:   $SYS_CONFIG"
if [[ "$PROTOCOL" == "quic" ]]; then
    echo "  QUIC port: $QUIC_PORT"
fi
echo ""

export ERL_FLAGS="$EXTRA_FLAGS"

if [[ -n "$PROFILE" ]]; then
    exec rebar3 as "$PROFILE" shell \
        --name "$NODE_NAME" \
        --setcookie service_discovery_cookie \
        --config "$SYS_CONFIG" \
        --apps "$APPS"
else
    exec rebar3 shell \
        --name "$NODE_NAME" \
        --setcookie service_discovery_cookie \
        --config "$SYS_CONFIG" \
        --apps "$APPS"
fi
