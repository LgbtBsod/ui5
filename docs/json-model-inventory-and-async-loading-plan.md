# JSON модели приложения и план асинхронной загрузки

## 1) Полный перечень JSON моделей (runtime)

> Базовые фабрики моделей определяются в `model/ModelFactory.js`.

### `data` model
- `checkLists: []`
- `visibleCheckLists: []`
- `selectedChecklist: null`

Назначение: основной runtime-слой списка чек-листов и выбранного объекта для Search/Detail flow.

### `state` model
- `mode`, `layout`, `isLoading`, `loadError`, `loadErrorMessage`
- search state: `filterId`, `filterLpc`, `filterFailedChecks`, `filterFailedBarriers`, `searchMode`, `searchMaxResults`
- edit/lock state: `isDirty`, `isLocked`, `isKilled`, `hasConflict`, `sessionId`, `activeObjectId`, `lockExpires`, `idleExpires`
- integration state: `mainServiceMetadataOk`, `mainServiceMetadataError`
- network/autosave: `networkOnline`, `networkGraceMode`, `autosaveState`, `autosaveAt`
- validation/security: `requiredFields`, `requiresUserLogin`, `testUser`, `testUserLogin`
- **new async flags**: `masterDataLoading`, `locationsLoading`

Назначение: глобальный orchestration-state для роутинга, блокировок и UX.

### `layout` model
- `smartFilter` (варианты/поля)
- `smartTable` (варианты/колонки/selection)
- `personalization`

Назначение: UI layout + personalization policy.

### `cache` model
- `pristineSnapshot`
- `keyMapping`
- `lastServerState`

Назначение: diff/dirty checks и cache-meta.

### `masterData` model
- `persons`
- `lpc`
- `professions`
- `statuses`, `resultTypes`, `types`, `timezones`

Назначение: справочники/подсказки/селекты для detail/search.

### `mpl` model
- `locations`

Назначение: иерархия местоположений и ValueHelp.

### Дополнительная JSON модель
- `selected` (инициализируется как `new JSONModel({})` в `Component.js`)

Назначение: редактируемый текущий объект в Detail.

---

## 2) Что уже оптимизировано в bootstrap

Ранее запуск был через общий `Promise.all`, где master-data (`persons/lpc/professions/locations`) блокировала снятие глобального `isLoading`.

Сейчас bootstrap разделен:

1. **Критический путь** (блокирует `isLoading`):
   - login
   - backend init
   - frontend config / server state
   - cache + загрузка чек-листов

2. **Фоновый путь** (не блокирует `isLoading`):
   - `persons/lpc/professions` в фоне (флаг `masterDataLoading`)
   - `locations` отдельно в фоне (флаг `locationsLoading`)

Таким образом, экран и сервис стартуют быстрее, а тяжелые справочники догружаются асинхронно.

---

## 3) Как это связано с refactor roadmap

Этот шаг согласован с общим планом декомпозиции и stabilization, описанным в:
- `docs/current-structure-refactor-audit-2026-02.md`

Особенно с целями по снижению orchestration-нагрузки на bootstrap и постепенному выделению независимых flow-path.

---

## 4) Что можно сделать следующим шагом

1. Добавить per-model telemetry (время загрузки `persons/lpc/professions/locations`) в state/cache.
2. Ввести lazy-load для locations tree (по дате/узлу), чтобы не грузить всю иерархию при старте.
3. Поддержать stale-while-revalidate для masterData: показывать кеш сразу, обновлять в фоне.
4. Привязать UI skeleton/empty-hints к `masterDataLoading` и `locationsLoading` для более предсказуемого UX.


## 5) Нужны ли все модели и можно ли объединять

Коротко: **часть можно объединить, но не всё**.

- `data` + `selected` теоретически можно слить, но это повысит риск side-effects в dirty-check и save-flow (сейчас `selected` — рабочая копия, `data.selectedChecklist` — baseline/источник для сравнения).
- `state` и `layout` лучше держать раздельно: первый про runtime state-машину, второй про персонализацию/настройки UI.
- `masterData` и `mpl` специально разделены, потому что `mpl` lazy-load по ValueHelp-сценарию и имеет отдельный lifecycle.

Рекомендация: начать не с merge моделей, а с **контракта полей** (typed schema + lint checks), чтобы вернуть контроль над структурой без регресса.

## 6) Mpl lifecycle (реализованный)

- При старте сервиса `mpl` **не загружается**.
- При первом открытии VH: если модель пуста — выполняется запрос в backend.
- При повторных открытиях VH используются данные уже заполненной `mpl` модели (без повторного запроса).
