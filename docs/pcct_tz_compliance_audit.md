# PCCT TZ Compliance Audit (Frontend + ABAP backend draft)

## Scope

Проверка текущего репозитория на соответствие целевому ТЗ по:
- Stateless + app-level locking,
- Fast Initial Load + Lazy Loading без `$expand`,
- CUD Delta для AutoSave,
- heartbeat/GCD/idle/beacon,
- backend ABAP draft (lock service + BOPF mapper + DPC/MPC patterns).

## Что исправлено в этом изменении

### Frontend/UI5

1. **AutoSave переведен на delta payload**
   - Добавлен `util/DeltaPayloadBuilder.js` для сравнения `current` vs `baseline` с выделением:
     - `root` delta,
     - `basic` delta,
     - `checks[]/barriers[]` с `edit_mode` = `C/U/D`.
   - `Component.js` теперь строит именно дельту и вызывает `BackendAdapter.autoSaveCheckList`.

2. **Fast Initial Load / Lazy Loading без `$expand` для detail-sections**
   - `RealBackendService.getChecklistChecks/getChecklistBarriers` теперь ходят в отдельные endpoints
     `/checklist/{id}/checks` и `/checklist/{id}/barriers` c `top=50, skip=0`.

3. **Mock Gateway расширен под lazy section endpoints**
   - Добавлены REST операции:
     - `GET /checklist/{root_id}/checks?top=&skip=`
     - `GET /checklist/{root_id}/barriers?top=&skip=`
   - В `ChecklistService` добавлены методы `list_checks`/`list_barriers`.

4. **Backend Adapter API дополнен autosave каналом**
   - `BackendAdapter.autoSaveCheckList(...)` + реализации в fake/real backend.

### ABAP draft

- RTTI mapper усилен 2-уровневым cache метаданных (L1 process hash + L2 shared buffer `INDX`) для дорогих RTTI/association lookup в рекурсии.
- Добавлен owner-aware lock stack (`zcx_lock_error`, `zcl_lock_manager`, `Z_LOCK_REGS_ASYNC`, `Z_UNLOCK_REGS_UPDATE`) и интеграция в `DPC_EXT` message-container ошибки.
- Сохранился helper ошибок BOPF → OData exception.

## Актуальный статус соответствия (кратко)

| Блок | Статус | Комментарий |
|---|---|---|
| Stateless lock/session model | Partial | UI и mock backend поддерживают session-guid lock semantics, но ABAP draft ещё референсный, не интегрирован в реальную DDIC/BO модель `ZPCCT_*`. |
| Lazy load без `$expand` | Improved | Для checks/barriers реализованы отдельные endpoints и вызовы с `top=50`. |
| CUD Delta AutoSave | Improved | Дельта формируется на UI; для row deltas в real adapter пока fallback на full update (из-за текущего replace API mock backend). |
| Heartbeat/GCD/Idle/Beacon | Good | Менеджеры присутствуют, логика wiring выполнена. |
| Cross-tab steal/is_killed | Partial | Базовый путь есть; требуется end-to-end на реальном ABAP lock service с `ZPCCT_LOCK`/`ZPCCT_LOCK_LOG`. |
| ABAP BOPF/Lock архитектура под TZ | Improved | Добавлены owner-aware lock manager и 2-level RTTI cache; остаётся привязка к production DDIC/BO namespace `ZPCCT_*`. |

## Ключевые оставшиеся gap'ы для полного enterprise-уровня

1. Реализовать в ABAP namespace `ZPCCT_*`:
   - lock FM trio (`ACQUIRE/HEARTBEAT/RELEASE`) + cleanup job + audit log.
   - DPC_EXT с явным COMMIT orchestration только после BOPF+Lock success.
2. Добавить row-level delta MERGE API (в mock и real backend), чтобы autosave не падал в full replace при изменениях rows.
3. Завершить optimistic conflict matrix (409/410/412 маршрутизация) до полного соответствия таблице ошибок ТЗ.

