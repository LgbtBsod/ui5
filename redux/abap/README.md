# ABAP-бэкенд для `ZCHECK_SRV` (SAP Basis 750 SP15, classic Gateway OData V2)

## Решение: единственный SSOT-контракт для ОБОИХ фронтендов

В репозитории исторически существовало два независимых бэкенд-спека под
одно и то же имя сервиса `ZCHECK_SRV`: 4-сущностная модель здесь (`CheckRoot`/
`CheckBasic`/`CheckItem`/`Barrier`, с push-down/AMDP/ETag) и старая
3-сущностная модель в `../abap/*` (`Z_Checks_Header`/`Z_Check`/`Z_Barrier`),
которую использовало freestyle mobile-приложение (`pc_lite/`, корень
репозитория). Оба спека объявляли ABAP-классы с ОДИНАКОВЫМИ глобальными
именами (`ZCL_CHECK_DPC_EXT`, `ZCL_CHECK_MPC_EXT`) с разным содержимым —
прямая коллизия объектов при транспорте на одну систему.

**Решение: единственный источник истины — 4-сущностная модель, описанная
в этом файле.** Спек в `pc_lite/abap/*` помечен deprecated (см.
`pc_lite/abap/README.md`), freestyle-приложение переводится на этот контракт
(маппинг полей — `pc_lite/MIGRATION_MAPPING.md`).

## Финальное архитектурное решение: один бэк, разное время подключения draft

Мобильное (freestyle, `pc_lite/`) и полное (Fiori Elements, здесь) приложения
работают с ОДНОЙ моделью данных и одной метадатой — не с двумя копиями.
Способ записи у них разный, и подключается в разное время:

- **Сейчас**: лайт пишет напрямую через обычный (non-draft)
  `CHECKROOTS_CREATE_DEEP_ENTITY` — он уже реализован в `ZCL_CHECK_DPC_EXT`,
  никакого draft лайту не нужно. Уже проверено end-to-end против мока
  (`serve.py`).
- **В момент раскатки draft на create** (Фаза 4, полная композиция) — BOPF
  всегда создаёт черновик на `POST /CheckRoots`, для ЛЮБОГО клиента, без
  исключения (в этом суть BOPF-драфта — клиент не может попросить сразу
  активную запись). Отдельную FunctionImport под "быстрое" создание НЕ
  заводим: OData V2 FunctionImport принимает только скалярные параметры и
  физически не может нести deep-payload (Root + массивы CheckItem/Barrier) —
  это ограничение протокола, не реализации. Вместо этого лайт переиспользует
  тот же deep-entity POST (без изменений) и, если ответ содержит
  `IsActiveEntity=false`, сам вызывает уже нужный FE `CheckRootActivationAction`
  (см. `pc_lite/controller/Main.controller.js#_afterCreate`/`_activateDraft`) —
  реализовано и работает уже сейчас, форвард-совместимо: до Фазы 4 эта ветка
  просто не срабатывает (сервер отдаёт активную запись сразу).

Почему один бэк, а не два по числу фронтендов: два независимо
поддерживаемых бэкенд-спека уже привели к реальному расхождению
справочников (шкала уровней КПР, коды типов проверок/барьеров) — обнаружено
и исправлено на живом коде `pc_lite`, не гипотетически. Один бэк устраняет
этот класс дефектов по построению, а не дисциплиной. Коллизия имён классов
(см. выше) — тот же симптом независимой эволюции двух копий одного бэкенда.

---

Эта папка — то, на что ссылается весь фронтенд-код в комментариях
(`abap/README.md`, `ZI_CheckRoot.ddls.asddls`, `abap/dpc_ext` и т.д.), но чего
физически не было в репозитории до этого коммита. `serve.py`/`serve_config.py`
в корне проекта — Python-мок ТОЧНО ЭТОГО контракта для локальной разработки
фронта без доступа к реальной системе; всё, что описано ниже, — целевая
реализация, с которой мок должен вести себя идентично (и уже ведёт — see
раздел "Соответствие мок ↔ ABAP" ниже).

## Архитектура

Классический (не RAP, не OData V4) SEGW-сервис `ZCHECK_SRV`:

```
SAPUI5 (ext/**)
   │  OData V2, /sap/opu/odata/sap/ZCHECK_SRV/
   ▼
ZCL_CHECK_DPC_EXT  (Data Provider Class ext.)  ──┐
ZCL_CHECK_MPC_EXT  (Model Provider Class ext.)   │  abap/classes/
   │                                             │
   │  push-down чтение (SELECT из CDS)           │
   ▼                                             │
abap/cds/*.ddls.asddls  ──────────────────────────┘
   │
   │  SELECT ... GROUP BY / association / left outer join
   ▼
Транспарентные таблицы (abap/ddic/tables.md) — ZCHK_ROOT/ZCHK_BASIC/
ZCHK_ITEM/ZCHK_BARR + справочники (customizing)

Составной ETag (max по 4 таблицам) — ZCL_CHECK_ROOT_ETAG_AMDP (AMDP,
push-down на HANA SQLScript, abap/amdp/) — читается из DPC_EXT.
```

Ключевое архитектурное решение (совпадает с тем, что уже реализовано в
`serve.py` и проверено живым фронтом): **4 независимые транзакционные
сущности** CheckRoot / CheckBasic / CheckItem / Barrier, а не одна плоская
CheckHeader. Root и Basic имеют независимые ETag — PATCH шапки не блокирует
конкурентный PATCH деталей. CheckItem/Barrier — композитный ключ
RootId+ItemId / RootId+BarrierId.

## Что делает CDS (push-down, объявлено, не написано в ABAP)

| Вычисление | Где |
|---|---|
| Резолв Code→Text (тип проверки/барьера, результат, часовой пояс, профессия, уровень КПР, местоположение) | `ZI_CheckBasic`, `ZI_CheckItem`, `ZI_Barrier` — `coalesce()` + `left outer join` на справочники |
| Fallback ФИО на интеграционное имя, если Pernr не сопоставлен | `ZI_CheckBasic` |
| ChecksAmount/ChecksSuccess/BarriersAmount/BarriersSuccess | `ZI_CheckItemAgg`/`ZI_BarrierAgg` (`count()`/`sum()` + `GROUP BY`) |
| SuccessRateChecks/SuccessRateBarriers | `ZI_CheckRoot` (`round()` над агрегатами) |
| ChecksCriticality/BarriersCriticality (пороги 50%/80%) | `ZI_CheckRoot` (`case when`) |
| HasErrorChecks/HasErrorBarriers, *BadgeHidden | `ZI_CheckRoot` (`case when`) |
| ChecksHidden/BarriersHidden (правило по LpcKey) | `ZI_CheckRoot` (`case when ... in (...)`) |
| StatusCriticality | `ZI_CheckRoot` (`case`) |
| HasChildren в дереве локаций | `ZI_LocationHierarchy` (`exists()`-подзапрос) |

Всё это — **чистые проекции без побочных эффектов**, HANA считает их на
чтении, ни строчки ABAP.

## Что НЕ может (или не должно) быть в CDS — и почему там класс

Это прямой ответ на "покажи примеры классов там, где CDS не отработает":

| Задача | Почему не CDS | Где реализовано |
|---|---|---|
| **Deep create** (Root + вложенные to_Basic/to_Checks/to_Barriers одним запросом) | CDS read-only по определению; generic Gateway framework не каскадирует вложенное создание для non-draft classic-сервиса | `ZCL_CHECK_DPC_EXT=>CHECKROOTS_CREATE_DEEP_ENTITY` |
| **Защита от mass assignment** (PATCH не может переписать Status/ChecksAmount/... в обход UI) | `sap:updatable="false"` в `$metadata` — это подсказка клиенту (SmartField не рисует поле редактируемым), НЕ гарантия для сырого HTTP-клиента в обход UI5; классический (не RAP) сервис не проверяет это за вас на входе в `*_UPDATE_ENTITY` | `ZCL_CHECK_DPC_EXT=>FILTER_READONLY_FIELDS` + `GET_READONLY_FIELDS`, вызывается из всех `*_UPDATE_ENTITY` |
| **Server-side валидация обязательных полей** | `Nullable="false"`/`Common.Required` — тоже подсказка клиенту (RequiredFieldsRule.js блокирует Save только визуально; force-enable через UI5 API это обходит) | `ZCL_CHECK_DPC_EXT=>VALIDATE_REQUIRED_FIELDS` |
| **Конфликт интересов** (инспектор ≠ проверяемый, сравнение по Pernr) | Кросс-полевая бизнес-валидация с raise исключения — CDS не может остановить запись, только сообщить о нарушении на чтении (`@ObjectModel.text.element` и подобные тут не помогут) | `ZCL_CHECK_DPC_EXT=>VALIDATE_NO_CONFLICT_OF_INTEREST` — **это новая проверка, которой нет даже в Python-моке** (там правило только в client-side `PernrRule.js`) |
| **Составной ETag** (max по Root+Basic+N×CheckItem+N×Barrier, батчем на всю страницу списка) | Формально выразимо через ассоциации + доп. helper-CDS-вьюху, но для единственного скалярного MAX ради одного поля это вьюха ради вьюхи; и что важнее — нужен явный контроль "один SQL-запрос на N RootId", а не N запросов | `ZCL_CHECK_ROOT_ETAG_AMDP` (AMDP/SQLScript) + `ZCL_CHECK_DPC_EXT=>RESOLVE_COMPOSITE_ETAGS` |
| **HeaderKpiTitle/HeaderKpiSubtitle** (форматированная строка "КПР: X · Профессия: Y") | Технически возможно через вложенные `concat()`, но нечитаемо и хрупко к правкам формата; тот же формат уже независимо живёт в `ext/util/Constants.js`/`i18n.properties` для transient-объекта как строковое форматирование, а не push-down вычисление | `ZCL_CHECK_DPC_EXT=>FORMAT_KPI_TITLE` / `FORMAT_KPI_SUBTITLE` |
| **sap:default-value на Result** (новая строка проверки сразу "Удовлетворительно") | CDS `@ObjectModel.*` не управляет default-значением OData-свойства — это специфика классической Gateway-схемы метаданных | `ZCL_CHECK_MPC_EXT=>SET_RESULT_DEFAULT_VALUE` |
| Генерация RootId/ItemId/BarrierId, DocId | UUID/number-range — процедурный ABAP по построению | `ZCL_CHECK_DPC_EXT` (create-методы) |

## Известные компромиссы (сознательно не решены здесь)

- **PkLevels как CSV-строка** (`"0,1,2,3,4"` на `ZI_CheckType`/`ZI_BarrierType`).
  Из-за этого контекстный F4-фильтр "тип проверки применим к текущему
  LpcKey" не выражается через `Common.ValueListParameterFilterOnly` (та
  умеет только точное равенство, не "CSV содержит X") и остаётся на
  клиенте (`ext/util/SubTableCrud.js`, `Contains`-фильтр). Правильное
  решение — M:N-таблица `ZCHK_CTYPE_PKL(check_type_code, pk_level)` вместо
  CSV-поля; не сделано, чтобы не расходиться с уже реализованной и
  протестированной моделью мока. См. `ZI_CheckType.ddls.asddls`.
- **Округление ChecksCriticality/BarriersCriticality**. CDS-версия сравнивает
  порог с точной дробью, Python-мок — с `round()`-енным процентом; на
  пограничных значениях (напр. ровно 49.5%) бакет может отличаться на 1.
  См. комментарий в `ZI_CheckRoot.ddls.asddls`.
- **Person как своя Z-таблица**, а не HR CDS (`I_Employee`). В реальном
  ландшафте, где инспекторы — HR-сотрудники, лучше переиспользовать
  стандарт (готовая авторизация P_ORGIN и т.д.) — см. `ZI_Person.ddls.asddls`.

## Соответствие мок ↔ ABAP (для проверки при портировании)

Всё, что описано выше, обязано вести себя идентично тому, что уже
реализовано и покрыто тестами (`tests/test_serve.py`) в Python-моке:

| Мок (`serve.py`/`serve_config.py`) | ABAP |
|---|---|
| `compute_check_root_view()` | `ZI_CheckRoot` + `ZCL_CHECK_DPC_EXT=>FORMAT_KPI_*` |
| `resolve_check_basic/item/barrier()` | `ZI_CheckBasic`/`ZI_CheckItem`/`ZI_Barrier` |
| `compute_etag_timestamp_ms()` / `_root_etag_string()` | `ZCL_CHECK_ROOT_ETAG_AMDP` |
| `strip_readonly_fields()` / `READONLY_FIELDS` | `ZCL_CHECK_DPC_EXT=>FILTER_READONLY_FIELDS` / `GET_READONLY_FIELDS` |
| `RESULT_CODE_SATISFACTORY = "X"` | `ZCL_CHECK_DPC_EXT=>C_RESULT_SATISFACTORY` |
| `BUSINESS_RULES.CHECKS_HIDDEN_PK_LEVEL` / `BARRIERS_HIDDEN_PK_LEVELS` | `ZI_CheckRoot` (case-выражения на ChecksHidden/BarriersHidden) |
| POST CheckRoots (deep create, required-fields) | `ZCL_CHECK_DPC_EXT=>CHECKROOTS_CREATE_DEEP_ENTITY` / `VALIDATE_REQUIRED_FIELDS` |
| `_is_allowed_cors_origin()` / CORS-allowlist | **Не переносится** — на реальной системе фронт и OData-сервис на одном origin (Fiori Launchpad reverse-proxy), CORS не нужен вовсе; это была защита исключительно для локального dev-сервера |
| Mock CSRF-токен (`CSRF_TOKEN = "mock-csrf-token"`) | **Не переносится** — Gateway-фреймворк генерирует и проверяет реальный CSRF-токен нативно (`x-csrf-token: Fetch`), кастомного кода не требуется |

Если при портировании поведение разойдётся — считать ABAP-реализацию
неправильной (мок уже прошёл живые браузерные тесты фронта), либо явно
задокументировать расхождение здесь, по аналогии с разделом "Известные
компромиссы".

## Что дальше (см. также обсуждение "что нужно для прод")

Это спецификация и примеры классов — не готовый к транспорту пакет. Не
хватает: реальных `.tabl`-описаний DDIC (см. `abap/ddic/tables.md` — только
состав полей), maintenance-view для справочников, PFCG-роли и активации
`/sap/opu/odata/sap/ZCHECK_SRV/` в SICF, объекта диапазона номеров для DocId,
полных сгенерированных SEGW-стабов (здесь показаны только редефинированные
методы), и модульных тестов ABAP Unit на `ZCL_CHECK_DPC_EXT` (по образцу
`tests/test_serve.py` — та же матрица кейсов: readonly-fields stripping,
required-fields validation, conflict-of-interest, resolve_type_and_result).
