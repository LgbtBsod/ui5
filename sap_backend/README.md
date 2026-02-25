# SAP Back-End (ABAP) implementation draft for OData processing

Этот каталог содержит архитектурный draft ABAP-артефактов под SAP_BASIS 750 SP15 / SAP HANA 2 SP7
для stateless OData сценария: CUD delta, app-lock service, optimistic concurrency.

## Что реализовано

- `src/zif_zodata_lock_manager.intf.abap` / `src/zcl_zodata_lock_manager.clas.abap`
  - manager блокировок/разблокировок/heartbeat-touch через FM слой.
- `src/zcx_lock_error.clas.abap`
  - T100-aware lock exception с атрибутами `user_fullname` / `pernr`.
- `src/zcl_lock_manager.clas.abap`
  - singleton lock-manager уровня приложения (`ZLOCK_REGS`, timeout, owner-info).
- `src/z_lock_regs_async.fugr.abap` / `src/z_unlock_regs_update.fugr.abap`
  - изолированная фиксация lock/unlock в RFC/update-task стиле.
- `src/zfg_zodata_lock.fugr.abap`
  - FM контроля lock/unlock/update (template для перевода в `Z_PCCT_LOCK_*`).
- `src/zif_zodata_bopf_mapper.intf.abap` / `src/zcl_zodata_bopf_mapper.clas.abap`
  - универсальный RTTI mapper:
    - авто-карта полей,
    - binary search по нормализованным именам,
    - mapping по индексам,
    - поддержка edit_mode C/U/D -> /BOBF change_mode.
- `src/zcl_zodata_rtti_cache.clas.abap`
  - 2-уровневый cache RTTI/config/association metadata:
    - L1: process memory (hashed tables),
    - L2: shared buffer (`INDX`) для снижения стоимости RTTI/metadata в рекурсии.
- `src/zcl_zodata_bopf_msg_helper.clas.abap`
  - агрегация ошибок BOPF (`et_failed_key/et_message`) в GW business exception.
- `src/zcl_zodata_dpc_ext.clas.abap`
  - CUD orchestration через function-import contracts:
    - `LockAcquire`, `LockHeartbeat`, `LockRelease`,
    - `AutoSave`, `SaveChanges`,
    - `MplTreeSet` (MPL через FM).
  - COMMIT только после успешного BOPF + lock шага.
- `src/zcl_zodata_mpc_ext.clas.abap`
  - `define` для function imports и deep complex types (`ZSTR_PCCT_*`).
- `src/zcl_zodata_odata_util.clas.abap`
  - утилита извлечения UUID key из `it_key_tab`.
- `src/zcx_zodata_error.clas.abap`
  - прикладное checked exception.

## Важно для реального внедрения

1. Заменить placeholder BO/Node/Association константы на реальные `ZBO_PCCT` (`ZIF_I_BO_C`).
2. Заменить template DDIC структуры на production-структуры (`ZSTR_PCCT_SAVECHANGES_RQ`, `..._RS`, `ZSTR_BO_*`).
3. Перевести lock FM в namespace `Z_PCCT_LOCK_ACQUIRE/HEARTBEAT/RELEASE/CLEANUP` с таблицами `ZPCCT_LOCK`, `ZPCCT_LOCK_LOG`.
4. Для AutoSave реализовать MERGE дельты по дочерним узлам (Checks/Barriers) с `edit_mode`.
