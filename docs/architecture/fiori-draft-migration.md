# Миграция блокировок на Fiori Draft — архитектурный план

Статус: **предложение, не реализовано**. Требует бэкенд-эпика (ABAP/RAP), выходит за рамки текущего UI5-репозитория.

## Почему сейчас нельзя "просто переключить"

Текущий сервис `Z_EHS_PRODUCTION_CONTROL_CKLT_SRV` — классический **OData V2** (SEGW), метаданные
подтверждают `odataVersion: "2.0"` в `manifest.json` и ручные `FunctionImport`: `LockAcquire`,
`LockHeartbeat`, `LockRelease`. Draft — не паттерн UI, а серверная модель данных
(активная + черновик-таблица, `IsActiveEntity`/`HasActiveEntity`/`DraftUUID`, действия
`Edit`/`Activate`/`Discard`/`Resume`). Она нативно поддерживается только:

- **RAP (ABAP Cloud)** через `@Consumption.draftEnabled` / `define draft table`, экспонируется как V4;
- либо классический **BOPF Draft** (Smart Business), тоже практически всегда через V4.

Ключевой вывод: миграция начинается на бэкенде и на уровне сервиса (V2 → V4 + Draft),
а не в UI5-контроллерах.

## Целевая архитектура

```
Backend (RAP/ABAP Cloud):
  Behavior Definition ZR_PRODUCTION_CHECKLIST
    persistent table zprodchklist
    draft table zprodchklist_d
    draft action Edit;
    draft action Activate;
    draft action Discard;
    draft action Resume;
    field ( readonly ) LocalLastChangedAt;

Frontend (UI5, sap.ui.model.odata.v4.ODataModel):
  oListBinding.getCurrentContexts()[0].getBinding()
  oContext = oListBinding.getBoundContext();
  oContext.getObject().IsActiveEntity   // false = черновик существует
  oDraftDataModel = new sap.ui.model.odata.v4.ODataModel({ synchronizationMode: "None", ... });
  oContext.getModel().bindContext("EditAction(...)", oContext).execute();   // "взять в редактирование"
  oContext.getModel().bindContext("ActivateAction(...)", oContext).execute(); // "сохранить"
  oContext.getModel().bindContext("DiscardAction(...)", oContext).execute(); // "отменить/закрыть без сохранения"
```

Что это устраняет **из фронтенда целиком**:

| Убрать | Причина |
|---|---|
| `LockAdapter.js` (172 строки, function-import обёртка) | Draft-таблица = блокировка на уровне записи, управляется ядром |
| `LOCK_STATES` state machine (`IDLE/ACQUIRING_LOCK/EDIT_LOCKED/IDLE_TIMEOUT_GRACE`) | `IsActiveEntity`/`HasActiveEntity` на контексте — уже конечный автомат |
| `TakeoverLockUseCase.js`, ручной heartbeat-опрос | `Resume`-действие возвращает `owner`/`lastChangedBy` из коробки; heartbeat не нужен — Draft не истекает по таймеру, а привязан к сессии редактирования |
| `CloseDetailUseCase.js` — ручной release + 15 model-патчей | `DiscardAction` на неактивированном драфте синхронно откатывает изменения; фронту остаётся один `navTo` |
| `AutosaveDetailUseCase.js` (ручной save-без-активации) | Draft OData V4 модель автосохраняет изменения в `PATCH`-запросах к драфт-сущности без отдельного "autosave endpoint" |
| Кастомные "dirty"-флаги (`WORKFLOW_DIRTY`, `/isDirty`) | `oModel.hasPendingChanges()` — встроенный метод V4 ODataModel |

## Что нужно от бэкенд-команды (ABAP/RAP эпик, отдельный трек)

1. Конвертировать `Z_EHS_PRODUCTION_CONTROL_CKLT_SRV` в CDS-based Business Object с
   `@ObjectModel.draftEnabled: true` (Behavior Definition + Behavior Implementation).
2. Перенести бизнес-правила `LockAcquire`/`AutoSave`/`SaveChanges`/`CreateChecklist` в
   стандартные RAP-действия (`create`, `update`, `Activate`) — большая часть текущей
   валидации (`ValidateChecklistUseCase`) переезжает в `validation` determination на BO.
3. Опубликовать как **OData V4** сервис через Service Binding.
4. Провести миграцию данных существующих незавершённых записей (если есть открытые
   "в работе" чек-листы на момент реза).

## Что делает фронтенд после того, как V4 Draft-сервис готов

1. Заменить `sap.ui.model.odata.v2.ODataModel` на `sap.ui.model.odata.v4.ODataModel` в `manifest.json`.
2. Удалить `LockAdapter`, `TakeoverLockUseCase`, `ComponentLockEventsRuntime`,
   `ComponentLockReleaseRuntime` и связанные тесты.
3. Заменить `CloseDetailUseCase`/`OpenDetailUseCase`/`SaveDetailUseCase` на прямые вызовы
   контекстных Draft-действий, как показано выше.
4. Оставить `Effects`/`Result`-фреймворк как есть для остальных доменов (Search/Analytics) —
   это отдельный, более широкий разговор об архитектуре, не входит в Draft-миграцию.

## Почему это ценно для бизнеса

Текущая связка (V2 + самописный лок-протокол на function-import'ах) требует поддерживать
клиент-серверный протокол блокировок вручную: heartbeat, таймауты, "killed"/"can takeover"
семантику — это код, тесты и баги, которые не нужны, если тот же сценарий "один
пользователь редактирует, остальные видят последнюю активную версию" уже решён ядром
Fiori Draft. Экономия — не в строчках кода на фронте (хотя это тоже, ~600 строк:
`LockAdapter` + `TakeoverLockUseCase` + lock-state-machine в `WorkflowContracts` +
связанные ranner'ы), а в том, что перестаёт существовать целый класс багов
"heartbeat не успел/протух", "два таба открыли одну запись", "lock освобождён, но UI не узнал".

Это отдельный эпик с бэкенд-командой — не блокирует уже выполненную зачистку `CloseDetailUseCase`/
`DetailResetRuntime`, которая работает на текущем V2-сервисе без изменений.
