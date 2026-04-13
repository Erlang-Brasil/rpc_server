#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

if [ -f cert.pem ] && [ -f key.pem ]; then
    echo "Certificates already exist. Remove cert.pem and key.pem to regenerate."
    exit 0
fi

echo "Generating self-signed certificate for QUIC distribution..."
openssl req -x509 -newkey rsa:2048 \
    -keyout key.pem -out cert.pem \
    -days 365 -nodes -subj '/CN=localhost'

echo "Done. Files created:"
echo "  $SCRIPT_DIR/cert.pem"
echo "  $SCRIPT_DIR/key.pem"
