# Аудит готовности проекта по ТЗ (без SAP-специфики бэкенда)

## Область оценки

Этот аудит проверяет **только прикладной контракт и фактические пользовательские flow** в текущем репозитории (UI5 + adapter + mock gateway), и **осознанно исключает SAP-специфику** (BOPF/CDS/ABAP DPC_EXT/LUW детали). Цель: понять, насколько текущая реализация уже соответствует ТЗ по поведению системы.

---

## Итоговая оценка готовности

- **Общая готовность по flow и архитектурным принципам:** **74%**.
- **Сильные зоны:** lock lifecycle, heartbeat, steal own session, lazy load checks/barriers, autosave + delta payload, beacon release, режимы EXACT/LOOSE.
- **Критические разрывы до целевого ТЗ:**
  1. Search UI пока не использует реальный `SmartFilterBar/SmartTable` (кастомная реализация вместо требуемой enterprise-обвязки).
  2. Несогласованность busy-state (`/isLoading` vs `/isBusy`) между экранами и операциями.
  3. Нет отдельной проверки свежести кэша через dedicated `LastChangeSet` endpoint в UI flow (используется heartbeat/server state, но не полноценный gate перед edit).
  4. Copy flow не открывает новый объект через backend `copy` endpoint как отдельную целевую операцию (copy пока в основном UI-уровневый сценарий).

---

## Проверка соответствия по ключевым требованиям ТЗ

## 1) Stateless + разделение ответственности
- **Статус:** ✅ Частично/практически выполнено.
- Что есть:
  - UI не опирается на ABAP session state; работает с собственным `sessionId` в state-model.
  - Есть отдельный lock service API (`acquire/heartbeat/release`) и отдельный adapter слой.
- Что не закрыто полностью:
  - Нет формального contract guard, который жёстко валидирует обязательность backend-методов в `real` режиме.

## 2) Search (Exact/Loose), paging, серверный контур
- **Статус:** ⚠️ Частично.
- Выполнено:
  - EXACT/LOOSE реализованы в `SmartSearchAdapter.filterData` (AND vs OR).
  - Backend list endpoint поддерживает `top/skip`.
- Разрыв:
  - В UI используется кастомная панель фильтров и таблица, а не реальные `SmartFilterBar + SmartTable`.
  - В real-сервисе `queryCheckLists()` фактически не использует серверный query payload и возвращает общий list.

## 3) Object page Fast Initial Load + Lazy Loading
- **Статус:** ✅ Выполнено.
- Выполнено:
  - Root/basic загружаются отдельно, checks/barriers — отдельными запросами.
  - `$expand` в UI flow не используется; есть отдельные вызовы `/checks` и `/barriers`.
  - Используется лимит `top=50` для дочерних наборов.

## 4) Locking + session steal + killed handling
- **Статус:** ✅ Выполнено.
- Выполнено:
  - Acquire/heartbeat/release реализованы end-to-end.
  - Steal own session реализован (`iv_steal_from`), backend помечает старую сессию killed.
  - UI обрабатывает `is_killed`: перевод в READ, остановка таймеров/работы редактирования.

## 5) Concurrency/Conflict handling
- **Статус:** ⚠️ Частично.
- Выполнено:
  - Конфликты (409) отрабатываются через единый coordinator-диалог.
  - В delta payload передаются `client_changed_on` и `client_version`.
- Разрыв:
  - На gateway нет полноценной единой семантики version check для всех save-path (часть веток использует lock validation + update timestamp, часть — упрощённо).

## 6) CUD Delta + edit_mode
- **Статус:** ✅ Выполнено (UI), ⚠️ частично (backend merge semantics).
- Выполнено:
  - UI формирует delta для root/basic/checks/barriers и маркирует строки `edit_mode = C/U/D`.
- Разрыв:
  - В real backend при наличии row changes выполняется fallback в full update (из-за replace-style row API), т.е. не везде true-merge поведение.

## 7) Smart cache (L1/L2), freshness, 90s lag
- **Статус:** ⚠️ Частично.
- Выполнено:
  - L1 memory + IndexedDB L2 есть.
  - Freshness классификация есть: `FRESH`, `STALE_OK`, `STALE` (30/90 секунд).
- Разрыв:
  - В UI flow не внедрён отдельный строгий pre-edit freshness gate через `LastChangeSet`; используется более мягкая схема.

## 8) Сетевые сбои / grace period
- **Статус:** ✅ Выполнено.
- Выполнено:
  - Есть coordinator online/offline.
  - Есть grace window (60s), после истечения принудительно read-only.

## 9) Таймеры (Heartbeat/GCD/Idle) и правила сброса
- **Статус:** ✅ Выполнено.
- Выполнено:
  - Heartbeat interval 4 мин.
  - GCD interval 5 мин и сброс только при full save (`pcct:fullSave` event).
  - Idle timeout есть (10 мин), переводит в READ.

## 10) Create/Copy/Save/Unload flow
- **Статус:** ⚠️ Частично.
- Выполнено:
  - Create/save реализованы.
  - Unload best-effort release через Beacon + fallback sync XHR.
- Разрыв:
  - Copy в текущем UI сценарии не полностью использует backend copy endpoint как основной путь с возвратом new id/lock; используется упрощённый фронтовый path.

---

## Проверка целевых flow (A–H) из ТЗ

| Flow | Статус | Комментарий |
|---|---|---|
| A. Открытие в READ с проверкой кэша | ⚠️ | Кэш используется, но нет явного отдельного LastChangeSet-gate перед применением stale данных. |
| B. Вход в EDIT через LockAcquire | ✅ | Реализовано; при conflict есть ветка steal own session. |
| C. New → SaveDraft с key mapping | ⚠️ | Создание есть, но полноценный temp→real mapping как обязательный flow не доведён до строгого контракта. |
| D. Copy существующего | ⚠️ | Backend copy endpoint есть, но UI copy flow упрощён и не полностью унифицирован под endpoint copy. |
| E. AutoSave (delta, без GCD reset) | ✅ | Delta autosave реализован; GCD не сбрасывается при autosave. |
| F. Full Save (с GCD reset) | ✅ | Full save вызывает `pcct:fullSave`, GCD reset есть. |
| G. Unload/Beacon release | ✅ | Реализовано sendBeacon + sync XHR fallback. |
| H. Session stolen handling | ✅ | `is_killed` переводит UI в safe read-only path. |

---

## Главные блокеры перед “полным соответствием ТЗ”

1. **Привести Search к целевому enterprise-контракту**
   - Либо внедрить `SmartFilterBar/SmartTable`, либо явно зафиксировать эквивалентный контракт и покрыть тестами.

2. **Унифицировать busy/loading state**
   - Сейчас в проекте используется смесь `/isLoading` и `/isBusy`, что создаёт непредсказуемость UX-индикаторов.

3. **Дожать cache freshness flow до strict-gate перед edit**
   - Перед входом в EDIT должна быть детерминированная проверка свежести (LastChangeSet/эквивалент), а не best-effort.

4. **Стандартизировать copy/new key-mapping контракт**
   - Для new/copy нужны однозначные contract responses: `new_uuid`, `key_mapping`, `version/changed_on`, `lock_expires`.

5. **Убрать replace-style деградацию для row updates**
   - Нужен row-level merge на backend, чтобы delta flow был реальным end-to-end.

---

## Рекомендованный план доработки (короткий)

### Спринт 1 (критические UX/flow риски)
- [ ] Устранить `/isBusy` vs `/isLoading` несогласованность.
- [ ] Внедрить strict freshness check при переходе в EDIT.
- [ ] Зафиксировать copy flow через backend endpoint + lock acquire.

### Спринт 2 (контракт и стабильность)
- [ ] Явный contract schema для save/autosave/copy/new.
- [ ] Доработать backend row merge semantics.
- [ ] Добавить smoke tests на A–H flow.

### Спринт 3 (enterprise-hardening)
- [ ] Финализировать search enterprise-обвязку (SmartFilterBar/SmartTable или контрактный эквивалент).
- [ ] Ввести monitoring метрики: lock conflicts, autosave success ratio, stale cache hits.

---

## Финальный вывод

Если игнорировать SAP-специфику backend-реализации, проект **функционально близок к целевому ТЗ по core flow**, особенно по блокировкам/таймерам/устойчивости редактирования. Но до “полной готовности” не хватает унификации search-контракта, строгой freshness-проверки перед edit, полной стандартизации new/copy контрактов и устранения несогласованных busy-state путей.
