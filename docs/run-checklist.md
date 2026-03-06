# Run Checklist

Дата: 2026-03-02

## Статус анализа модулей
- Архитектурный QA прогон по всем модулям (31 gate): **PASS**.
- Ошибки, блокирующие запуск, не обнаружены.

## Чек-лист запуска
- [x] Установлены Python-зависимости mock backend (`mock_gate_way/requirements.txt`).
- [x] Запущен backend: `uvicorn main:app --host 0.0.0.0 --port 8000`.
- [x] Запущен UI static server: `python3 scripts/dev_static_server.py 8080`.
- [x] Проверена доступность UI: `GET /index.html` => OK.
- [x] Проверена проксируемая OData ручка: `RuntimeSettingsSet(Key='GLOBAL')` => OK.

## Примечание
- Архитектура не изменялась, REST/fetch fallback не добавлялся.
