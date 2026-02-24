# Mock SAP Gateway Simulator

Поведенческий mock-сервер на FastAPI для локальной отработки сценариев UI:

- OData-подобные endpoint-ы (`/SaveChanges`, `/AutoSave`, `/SaveDraft`, `/ChecklistFlatSet`)
- lock lifecycle (`acquire`, `heartbeat`, `release`)
- optimistic concurrency (`409 CONFLICT`)
- lock conflicts/expiration (`409 LOCKED`, `410 LOCK_EXPIRED`)
- delta C/U/D patching
- key mapping для draft (`temp_* -> real uuid`)
- paging/sort/filter
- принудительная симуляция `500`

## Запуск

```bash
pip install fastapi uvicorn
cd mock-server
uvicorn main:app --reload --port 8080
```

## Базовый URL

```text
http://localhost:8080
```

При миграции на SAP в UI должен меняться только `BASE_URL`.
