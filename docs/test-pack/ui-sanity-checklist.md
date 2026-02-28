# UI Sanity Checklist (Gateway-like OData V2)

- [ ] `$metadata` загружается без ошибок в консоли.
- [ ] SmartTable показывает строки из `ChecklistSearchSet`.
- [ ] SmartFilterBar применяет серверную фильтрацию (`$filter`, `$top`, `$skip`).
- [ ] Сегмент ALL/FAILED/SUCCESS меняет фильтр `HasFailedChecks` и уважает текущий режим AND/OR.
- [ ] В network нет вызовов `/checklist` (legacy REST).
- [ ] В network нет `403` на `POST .../$batch`.
- [ ] Lock flow работает: ACQUIRE -> HEARTBEAT -> RELEASE.
- [ ] Autosave отправляет изменения через OData (`AutoSave`).
- [ ] Cache-check использует `LastChangeSet` + `CacheToleranceMs`.
- [ ] RuntimeSettingsSet('GLOBAL') загружается один раз и применяется менеджерами интервалов.
