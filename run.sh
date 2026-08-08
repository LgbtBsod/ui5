#!/bin/bash
# Удобный запуск: ./run.sh [port]
# По умолчанию порт 3000 (preview-порт space-z.ai sandbox).
cd "$(dirname "$0")"
PORT="${1:-3000}"
echo "Starting Fiori Elements mock server on port $PORT..."
echo "App URL:    http://localhost:$PORT/index.html"
echo "OData URL:  http://localhost:$PORT/sap/opu/odata/sap/ZCHECK_SRV/"
echo
exec python3 -u serve.py --port "$PORT"
