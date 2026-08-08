#!/bin/bash
# Стартовый скрипт для mock OData сервера
# Автоматически перезапускает сервер при падении
#
# [Preview Fix] Порт по умолчанию изменён с 8080 на 3000:
# в sandbox-окружении space-z.ai Caddy-прокси на :81
# направляет preview-URL (https://preview-<bot-id>.space-z.ai/)
# именно на localhost:3000. На 8080 preview не работает — будет 502.
# Локально можно указать любой порт: ./start_server.sh 8080
cd "$(dirname "$0")"

PORT="${1:-3000}"

while true; do
    echo "[$(date)] Запуск serve.py на порту $PORT..."
    python3 -u serve.py --port "$PORT"
    EXIT_CODE=$?
    echo "[$(date)] Сервер упал с кодом $EXIT_CODE, перезапуск через 2 сек..."
    sleep 2
done
