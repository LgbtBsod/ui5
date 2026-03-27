# ТЗ 2.1 — SAP UI5 / SAP Gateway / CDS-SADL / BOPF

## 1. Назначение документа

Этот документ фиксирует итоговую целевую архитектуру и обязательные правила реализации проекта.

Документ является:
- основным baseline для доработки проекта;
- исходным ТЗ для Codex;
- источником истины для всех последующих изменений;
- правилом, что сначала читаются требования, затем выполняется доработка.

Цель проекта:
- перевести frontend на реальный SAP Gateway OData V2;
- довести backend-реализацию до production-grade состояния;
- сохранить прозрачность интеграции с BOPF;
- максимально использовать стандартные механизмы SAP/UI5/CDS/SADL/Gateway;
- не строить хрупкий самописный framework поверх стандартного стека.

---

## 2. Технологический стек и ограничения

### 2.1. Стек
- SAP BASIS 750 SP15
- SAP HANA 2 SP6
- SAP UI 754 SP5
- SAPUI5 1.71
- SAP Gateway OData V2
- CDS / SADL
- BOPF
- ArchiveLink для вложений

### 2.2. Браузеры
- поддерживается современный Microsoft Edge;
- Internet Explorer не поддерживается и не учитывается;
- нельзя внедрять legacy-решения под IE.

### 2.3. Правило по современному синтаксису
Использовать современный синтаксис и конструкции только в пределах:
- SAPUI5 1.71;
- SAP UI 754 SP5;
- текущего build/runtime toolchain проекта.

Запрещено:
- использовать паттерны, требующие более нового поколения UI5;
- закладываться на OData V4;
- требовать новый transpile/pipeline, которого нет в проекте;
- тянуть модные конструкции, несовместимые со стеком.

---

## 3. Бизнес-смысл системы

Проект является **wrapper-слоем** над данными, приходящими из интеграционного потока.

### 3.1. Основная идея
- большинство записей приходит из интеграционного потока;
- ориентировочно до 10% записей создаются локально через UI;
- даже интеграционные записи должны быть пригодны для:
  - просмотра;
  - обработки;
  - редактирования по правилам системы;
  - аналитики;
  - поиска;
  - вложений;
  - последующего жизненного цикла.

### 3.2. Признак интеграционного происхождения
В root-сущности должен существовать флаг интеграционного происхождения:
- `this_is_the_integration_data`,
- либо эквивалентный ему по смыслу флаг.

### 3.3. Отображение интеграционного происхождения
Если запись интеграционная:
- в карточке должен отображаться визуальный chip / badge / marker;
- при попытке входа в режим редактирования обязательно показывается confirm dialog с пояснением, что запись пришла из интеграционного потока.

---

## 4. Общие архитектурные принципы

### 4.1. Standard-first
Максимально использовать стандартные механизмы:
- CDS/SADL — для чтения;
- Gateway — для OData-контракта;
- Function Imports / custom handlers — только для command-сценариев;
- BOPF — для модификации и C/U/D логики;
- ArchiveLink — для вложений.

### 4.2. Не изобретать лишние слои
Запрещено:
- строить новый самописный framework поверх текущего приложения;
- плодить façade/runtime/manager/service-слои без реальной необходимости;
- добавлять промежуточный уровень “ради красоты”, если это не уменьшает сложность.

### 4.3. OData — только транспорт
Поля UI не должны жить на прямом OData binding.

Правило:
- OData/Gateway используется как transport/integration layer;
- экранные данные живут в JSONModel;
- frontend сам управляет state/data/cache/runtime models.

### 4.4. Stateless-подход
ABAP session не является источником прикладного state.

Источники истины:
- БД / CDS / BOPF / lock table / ArchiveLink;
- JSONModel на фронте только для текущего UI runtime.

### 4.5. Truth before change
Перед любой серьёзной доработкой сначала составляется truth matrix:
- что есть сейчас в коде;
- что есть в metadata;
- что есть в backend archive;
- что является target contract.

Codex не имеет права выдумывать поведение вместо документирования фактической логики.

---

## 5. Правило по ключам и BOPF-совместимости

### 5.1. Ключевая политика
Сохранять технические ключи BOPF как есть:
- `DB_KEY`
- `PARENT_KEY`

### 5.2. Запрещено
Запрещено вводить искусственную канонику вида:
- `ObjectUuid`
- `NodeUuid`

если это ломает прозрачность маппинга к BOPF.

### 5.3. Причина
Нужно обеспечить:
- прозрачное соответствие mapper-а BOPF-структурам;
- минимальный маппинг между frontend payload и `lt_modification`;
- минимальный риск поломки командного контура.

### 5.4. Допустимо
Допустимо иметь отдельный business/display ID для UI,
но он не должен заменять технические `DB_KEY` / `PARENT_KEY` в command-flow.

---

## 6. Жизненный цикл объекта

### 6.1. Канонический набор статусов
- `DRAFT`
- `IN_PROCESS`
- `COMPLETED`
- `CANCELLED`

### 6.2. Разрешённые переходы
- `DRAFT -> IN_PROCESS`
- `DRAFT -> CANCELLED`
- `IN_PROCESS -> COMPLETED`
- `IN_PROCESS -> CANCELLED`
- `COMPLETED -> IN_PROCESS`
- `CANCELLED -> IN_PROCESS`

### 6.3. Editable statuses
Редактирование разрешено только в:
- `DRAFT`
- `IN_PROCESS`

### 6.4. Read-only statuses
До reopen режим read-only действует для:
- `COMPLETED`
- `CANCELLED`

### 6.5. Reopen
Переоткрытие `COMPLETED` и `CANCELLED` переводит объект обратно в:
- `IN_PROCESS`

### 6.6. Важное уточнение
Даже без edit mode карточка всегда по умолчанию является read-only.
Переход в edit mode — отдельный управляемый процесс.

---

## 7. Интеграционные поля и приоритет источников данных

### 7.1. Integration-owned text fields
Следующие поля являются интеграционными текстовыми значениями и остаются валидными даже без reference code:
- `obsr_name`
- `obsd_name`
- `location_name`

### 7.2. Helper/reference keys
Следующие поля являются helper/reference keys и при заполнении становятся приоритетным источником:
- `obsr_pernr`
- `obsd_pernr`
- `location_key`

### 7.3. Правило fallback / priority
- если helper key пустой — использовать интеграционный текст;
- если helper key заполнен — reference-backed значение становится приоритетным.

### 7.4. Derived display fields
#### OBSR
- если `obsr_pernr` initial → `obsr_fullname = obsr_name`
- иначе `obsr_fullname = fullname из HR CDS по PERNR`

#### OBSD
- если `obsd_pernr` initial → `obsd_fullname = obsd_name`
- иначе `obsd_fullname = fullname из HR CDS по PERNR`

#### Location
- если `location_key` initial → `location_name = integration text value`
- иначе `location_name = text из reference hierarchy`

### 7.5. Нельзя делать
- нельзя автоматически пытаться “угадать” лицо или location только по интеграционному тексту;
- нельзя затирать интеграционный текст, если пользователь явно не выбрал справочное значение.

### 7.6. Разрешённая эволюция данных
Во время обработки карточки в edit mode пользователь должен иметь возможность:
- выбрать лицо через suggestion/VH и заполнить `*_pernr`;
- выбрать location через hierarchy VH и заполнить `location_key`.

После этого reference-backed значения становятся приоритетными для дальнейшей работы.

---

## 8. Вход в режим редактирования

### 8.1. Для обычной записи
Порядок входа в edit mode:
1. permission check;
2. lock acquire;
3. cache freshness validation;
4. enter edit mode.

### 8.2. Для интеграционной записи
Порядок входа в edit mode:
1. показать confirm dialog, что запись интеграционная;
2. permission check;
3. lock acquire;
4. cache freshness validation;
5. enter edit mode.

### 8.3. При входе в edit mode
После успешного входа должны запускаться:
- heartbeat;
- lock monitoring;
- autosave timer logic;
- tracking dirty state.

---

## 9. Create flow

### 9.1. Новый объект до первого сохранения
Новый объект:
- не создаётся в БД сразу;
- существует только во frontend session текущей вкладки;
- если пользователь не нажал Save — объект не должен попасть в БД.

### 9.2. После первого успешного save
Backend должен вернуть:
- реальный guid;
- root `DB_KEY`;
- business/display ID.

После этого:
- статус root = `DRAFT`;
- на объект ставится блокировка;
- запускаются edit timers/heartbeat/autosave.

---

## 10. Copy flow

### 10.1. Что копируется
CopyChecklist должен:
- копировать все ноды объекта;
- не копировать вложения;
- не копировать integration flag;
- не копировать интеграционные `*_name` как integration-origin semantics.

### 10.2. Что получает новая запись
- новый объект;
- новый `DB_KEY`;
- новый guid;
- новый business/display ID;
- статус `DRAFT`.

### 10.3. Frontend flow
После copy:
- открывается новая карточка;
- карточка сразу в edit mode;
- сразу запускаются lock/timers/edit-flow.

---

## 11. Save / Autosave / Delete / Modify pipeline

### 11.1. Общий принцип
Все изменения должны идти через единый canonical pipeline:
- frontend формирует diff;
- backend mapper парсит структуру frontend payload;
- формируется `lt_modification`;
- далее вызывается `BOPF modify`.

### 11.2. Autosave
- работает только в edit mode;
- работает только если есть dirty changes;
- отправляет delta по dirty nodes;
- включает `root/basic` + изменённые child rows;
- обновляет `ChangedOn`;
- возвращает normalized payload + новый pristine snapshot;
- при успехе сбрасывает `isDirty = false`.

### 11.3. Full Save
- явное пользовательское действие;
- основной commit path;
- при успехе сбрасывает `isDirty = false`;
- нельзя придумывать лишний blocking validation layer, если бизнес позволяет сохранять всегда.

### 11.4. Delete target architecture
Delete должен быть частью pipeline:
- `diff -> mapper -> lt_modification -> BOPF modify`.

Direct OData DELETE не является target architecture.

### 11.5. Delete child-nodes
Правило для child rows:
- если строка создана в вебе и не сохранена (`edit mode = C`) — удаляем локально;
- если строка уже существует на backend (`edit mode = U`) — помечаем `D`;
- backend обязан обработать `D` через mapper и `BOPF modify`.

---

## 12. Locking и конкуренция

### 12.1. Общая политика
- два пользователя могут одновременно просматривать запись;
- два пользователя не могут одновременно редактировать одну и ту же запись;
- две вкладки одного пользователя не могут независимо редактировать одну и ту же запись без self-steal flow.

### 12.2. Канонический набор команд
- `LockAcquire`
- `LockStatus`
- `LockHeartbeat`
- `LockRelease`

### 12.3. Self-steal
Self-steal реализуется:
- не отдельным endpoint;
- флагом `ForceTakeover` в `LockAcquire`.

При self-steal:
- создаётся новая lock record с новым session id;
- используется стандартный lock flow.

### 12.4. Killed session behavior
Если сессия была перехвачена:
- немедленный readonly downgrade;
- показать blocking message/banner;
- очистить dirty state;
- остановить autosave;
- остановить heartbeat;
- отключить save/edit actions.

### 12.5. Safe release
Разрешён best-effort release,
но backend обязан делать release только если текущая сессия действительно ещё владеет lock.

Blind unlock запрещён.

### 12.6. Canonical takeover parameters
Target contract должен использовать единый takeover-flag:
- `ForceTakeover` или эквивалент.

Параметр `StealFrom` не должен оставаться в контракте,
если backend не докажет его реальную необходимость для business/audit logic.

### 12.7. LockRelease.TrySave
`TrySave` допустим только как legacy backward-compatible параметр,
если unload/save-on-leave действительно остаётся поддерживаемым сценарием.
Иначе его нужно считать deprecated в target contract.

---

## 13. Cache и LastChangeSet

### 13.1. Cache scope
Кэшируем:
- только detail cards;
- только в рамках browser session;
- search results не являются canonical cache;
- при закрытии браузера кэш очищается.

### 13.2. Что входит в detail cache
В detail cache входят:
- root;
- basic;
- checks;
- barriers;
- другие ноды карточки,

но **не attachments**.

### 13.3. Attachments и кэш
Attachments:
- участвуют в `LastChangeSet`;
- не входят в detail cache payload;
- всегда lazy-loaded по требованию.

### 13.4. LastChangeSet состав
`LastChangeSet` должен учитывать:
- root;
- basic;
- checks;
- barriers;
- attachments.

### 13.5. Проверка freshness
Проверка freshness должна выполняться:
- при открытии карточки;
- при входе в edit mode.

Если кэш валиден — можно использовать кэш.
Если нет — обязательна перезагрузка с backend.

### 13.6. Runtime-порог freshness
Порог freshness является runtime-настройкой и должен приходить с backend runtime settings.

---

## 14. Search

### 14.1. Архитектура поиска
Поиск должен оставаться CDS/SADL-based.

Запрещено:
- проектировать новый custom search engine без реальной необходимости;
- изобретать сложную fuzzy/fulltext-логику поверх текущей реализации, если она не подтверждена кодом.

### 14.2. Режимы поиска
- Exact search = `AND`
- Inexact search = `OR`
- режим переключается frontend switch.

### 14.3. Trigger
Поиск запускается явно, а не автоматически в live режиме.

### 14.4. Сортировка
Default sort:
- `date_check descending`

### 14.5. Два отдельных лимита
Существуют два разных параметра, и их нельзя смешивать:

#### 1. Backend fetch limit
Сколько максимум строк забирать с backend за один запрос.

#### 2. UI display chunk size
Сколько строк показывать за один шаг в UI.

### 14.6. Значения по умолчанию
Оба default значения должны приходить из backend runtime settings.
Во frontend допустим fallback constant только для bootstrap safety.

### 14.7. Search field semantics
Какие поля exact / contains / special — не выдумывать.

Codex обязан:
- извлечь фактическую логику из текущей CDS/SADL реализации;
- задокументировать её в `TRUTH_MATRIX.txt`.

### 14.8. Total count
`total count` является опциональным.
Нельзя заставлять backend всегда выполнять дорогой count, если это не требуется UX.

---

## 15. Detail loading и lazy loading

### 15.1. Initial load
При открытии карточки грузим минимум:
- root;
- basic.

### 15.2. Async after render
После первоначального рендера асинхронно грузим:
- checks;
- barriers;
- чанками по 20 строк;
- с busy/skeleton placeholders.

### 15.3. Attachments
Attachments не грузятся при открытии карточки.
Они грузятся только по явному действию пользователя:
- `Show attachments` / аналогичная команда.

### 15.4. Cache flow
Даже если карточка открывается из кэша:
- сначала быстро рендерим оболочку;
- затем асинхронно подтягиваем остальные секции по тому же UX-принципу.

---

## 16. Person Suggest / VH

### 16.1. Правила запуска
- только в edit mode;
- только после user interaction;
- preload запрещён;
- минимально 2–3 символа;
- только активные пользователи.

### 16.2. Цель
Suggestion/VH нужны для заполнения helper keys (`PERNR`), а не для “угадывания” значения по интеграционному тексту.

---

## 17. Location hierarchy

### 17.1. Loading
- preload запрещён;
- первая работа пользователя с hierarchy VH инициирует backend load;
- последующие обращения используют session cache.

### 17.2. UI baseline
Baseline implementation:
- `TableTree`.

### 17.3. Уже заданные поля иерархии
Нужно сохранить существующую структуру:
- `HIERARCHY_RANK`
- `HIERARCHY_TREE_SIZE`
- `HIERARCHY_PARENT_RANK`
- `HIERARCHY_ROOT_RANK`
- `HIERARCHY_LEVEL`
- `drill`
- `parent_id`
- `node_id`

---

## 18. Attachments

### 18.1. Storage
Целевой storage:
- `ArchiveLink`

### 18.2. Ограничения
- max file size = `10 MB`
- allowed groups:
  - office docs
  - spreadsheets
  - text files
  - png
  - jpg/jpeg
  - optionally wav

### 18.3. Права и режимы
- upload только в edit mode;
- delete только в edit mode;
- если пользователь имеет `EDIT` на карточку, он может работать и с attachments.

### 18.4. Dual-table UX
- таблица 1 = attachments, добавленные в текущей сессии;
- таблица 2 = все persisted attachments объекта;
- сначала отображаются session attachments;
- полный persisted список грузится только по явному действию.

### 18.5. Контракт содержимого
Attachment entity должна быть **metadata-first**.

Inline binary `Value` — legacy и не должен оставаться source of truth.

Content retrieval должен использовать ArchiveLink-compatible strategy:
- media stream,
- либо download URL,
- либо document handle/reference.

Точный transport shape может быть финализирован в реализации,
но он должен быть совместим с ArchiveLink.

---

## 19. Permissions

### 19.1. Общий принцип
Frontend не должен владеть сложной authorization logic.

Frontend contract:
- отправляет object identifier + operation code;
- получает `OK / NOT_OK`.

### 19.2. Базовые operation codes
- `CREATE = 01`
- `EDIT = 02`
- `VIEW = 03`
- `DELETE = 06`

### 19.3. Mapping derived operations
- `SAVE -> EDIT`
- `AUTOSAVE -> EDIT`
- `REOPEN -> EDIT`
- `ADD_ATTACHMENT -> EDIT`
- `DELETE_ATTACHMENT -> EDIT`
- `COPY -> CREATE`

### 19.4. Аналитика
Отдельная frontend-проверка на просмотр analytics не требуется,
если backend позже не введёт такое требование.

### 19.5. Константы
Operation codes и mapping должны храниться в dedicated constants/config layer.
Raw literals нельзя размазывать по коду.

### 19.6. Raw string и magic values policy
Все raw string / magic values должны выноситься в dedicated constants class / constants module.

Обязательно выносить в константы:
- operation codes;
- route names;
- model names;
- entity set names;
- function import names;
- action keys;
- status literals;
- edit mode literals (`C`, `U`, `D` и т.п.);
- event channel / topic names;
- local storage / session storage keys;
- fixed message codes;
- CSS class names, если они принадлежат приложению;
- иные повторно используемые технические строки.

Запрещено:
- дублировать одинаковые строки по разным controller/service/runtime слоям;
- держать magic values inline в логике;
- смешивать backend runtime variables и frontend technical constants.

---

## 20. Runtime settings и frontend models

### 20.1. RuntimeSettingsSet
Финальная модель runtime settings допускает mixed shape:
- стабильные высокозначимые параметры — typed fields;
- динамичные группы — JSON fields.

### 20.2. Что считается правильной формой
Не нужно переусложнять `RuntimeSettingsSet` десятками мелких полей,
если это не улучшает поддержку.

### 20.3. Канонический источник runtime settings
На текущем этапе проекта каноническим источником runtime settings является:
- `masterData>/runtime/*`

`state` может содержать только operational projections, если это нужно активному UI-flow.

### 20.4. Backend-driven runtime variables
С backend должны приходить:
- search default limits;
- cache thresholds;
- heartbeat / idle / timer values;
- upload policy;
- required fields;
- feature flags;
- analytics refresh settings / metadata, если нужно.

### 20.5. Frontend constants
Во frontend constants остаются:
- route names;
- endpoint aliases;
- model names;
- operation code constants;
- technical enums / status literals / edit mode literals;
- entity set names;
- function import names;
- message keys / message codes;
- storage keys;
- иные повторно используемые технические строки.

Frontend technical constants должны быть сгруппированы в dedicated constants classes/modules по смыслу,
а не храниться inline в controller/service коде.

---

## 21. Analytics architecture

### 21.1. Разделение аналитики
В проекте есть два аналитических контура:

#### Simple analytics
- только для search screen;
- lightweight summary/KPI;
- read-only;
- CDS/SADL-backed where possible.

#### Full analytics
- отдельный dedicated screen/route;
- отдельная heavy analytics entity / breakdown entity;
- не должен грузиться “просто так”.

### 21.2. Summary entity
Если KPI summary для search rail и analytics screen действительно одинаковые и лёгкие,
допустимо использовать одну shared lightweight summary entity.

Не нужно искусственно плодить ещё одну summary entity без пользы.

### 21.3. Full analytics backend model
Full analytics должен быть построен на:
- dedicated precomputed persistence table;
- background job, которая эту таблицу заполняет/обновляет;
- dedicated CDS/entity поверх этой таблицы.

### 21.4. Что запрещено
- тяжёлая аналитика не должна считаться online в UI-request path;
- search screen не должен тянуть heavy breakdown dataset.

### 21.5. AnalyticsRefreshState
Должна существовать отдельная refresh-state entity.

Минимальный обязательный набор полей для UI:
- `Status`
- `IsRunning`
- `LastSuccessAt`
- `LastError` или `LastMessage`

Дополнительные поля допустимы как backend diagnostic fields.

---

## 22. Standard-first implementation rules

### 22.1. Обязательные best practices
- использовать CDS/SADL для чтения везде, где это возможно;
- использовать Gateway function imports/custom handlers только для command-сценариев;
- сохранять BOPF-compatible key structure;
- использовать JSONModel как UI state/data layer;
- минимизировать количество точек маппинга;
- придерживаться SAP best practices на всех слоях решения: UI5, Gateway, CDS/SADL, BOPF, attachment integration;
- по умолчанию выбирать standard-first решение, если стандарт платформы покрывает задачу без избыточных компромиссов.

### 22.2. Запрещено
- DOM hacks;
- patching внутренних CSS-классов SAP controls;
- самописные hidden frameworks поверх UI5;
- лишние façade/runtime/manager слои;
- скрытая двойная правда между local mock и productive contract;
- hardcoded raw string значения в бизнес- и интеграционной логике, если они могут и должны быть вынесены в константы;
- отклонение от SAP best practices без явного технического обоснования в документе.

### 22.3. Legacy overrides
Считать устаревшим и не target:
- старую attachment inline-binary model;
- direct OData DELETE как целевой путь;
- скрытое использование local/mock contract как второго источника истины.

---

## 23. Требования к Codex

### 23.1. Порядок работы
Codex обязан работать в таком порядке:
1. построить `TRUTH_MATRIX.txt`;
2. зафиксировать `REQUIREMENTS_BASELINE.txt`;
3. обновить frontend/backend contract;
4. убрать расхождения между metadata, adapters и backend;
5. сделать реализацию;
6. вынести raw strings / magic values в constants classes/modules;
7. документировать residual risks.

### 23.2. Запрещено для Codex
- придумывать поведение без анализа кода;
- менять search semantics без truth matrix;
- менять attachment/lock/analytics contract без документирования текущего состояния;
- придумывать новый красивый key contract поверх `DB_KEY/PARENT_KEY`;
- оставлять повторяющиеся raw string literals в коде, если они могут быть централизованы в constants classes/modules;
- игнорировать SAP best practices, если платформа уже даёт стандартный поддерживаемый паттерн.

### 23.3. Unknown behavior
Если какая-то логика не до конца ясна, Codex обязан:
- задокументировать её как unknown/legacy/actual-current behavior;
- не invent fake certainty.

---

## 24. Обязательные артефакты на выходе

Codex обязан сформировать:
- `REQUIREMENTS_BASELINE.txt`
- `TRUTH_MATRIX.txt`
- `FRONTEND_ENDPOINT_MATRIX.txt`
- `GATEWAY_ENTITY_MODEL.txt`
- `FUNCTION_IMPORTS.txt`
- `LOCK_STATE_MACHINE.txt`
- `CACHE_POLICY.txt`
- `CDS .txt`
- `ABAP .txt`
- `work packages`
- `acceptance criteria`
- `residual risks`

---

## 25. Acceptance criteria

Работа считается завершённой только если:
1. productive contract больше не конфликтует с local/mock/backend truth;
2. `DB_KEY/PARENT_KEY` последовательно сохранены в command-flow;
3. search exact/inexact сохранён и документирован;
4. create/copy/save/autosave/delete сведены к coherent modify pipeline;
5. lock flow поддерживает acquire/status/heartbeat/release/self-steal;
6. killed session уходит в readonly корректно;
7. attachments lazy-loaded и ArchiveLink-compatible;
8. attachments исключены из detail cache, но участвуют в `LastChangeSet`;
9. runtime settings централизованы и не размазаны хаотично;
10. full analytics опирается на precomputed backend model;
11. Codex сначала документирует truth, потом меняет код;
12. в проекте не появляется новый хрупкий framework поверх стандарта.

---

## 26. Примеры реализации best practice

### 26.1. Frontend constants
Нельзя:
- хранить `01`, `02`, `03`, `06` по всему коду;
- дублировать raw string literals для route names, model names, entity names, function imports, status values и edit mode markers.

Нужно:
- один или несколько осмысленно разделённых constants files / classes / modules;
- отдельные константы для operation codes и mapping;
- отдельные константы для entity/function names, routes, model names, statuses и edit mode literals;
- использовать константы как единственный frontend source of truth для повторно используемых технических строк.

### 26.2. Save / Delete child rows
Нельзя:
- делать ручной SQL-style delete path в обход общего pipeline.

Нужно:
- `C`-строки удалять локально;
- persisted строки помечать `D`;
- backend обрабатывает `D` через mapper + `lt_modification` + `BOPF modify`.

### 26.3. Attachments
Нельзя:
- загружать все attachments при открытии карточки;
- держать binary `Value` как source of truth.

Нужно:
- metadata-first attachment entity;
- lazy load only on demand;
- ArchiveLink-compatible content access.

### 26.4. Search limits
Нельзя:
- смешивать fetch limit и display chunk size.

Нужно:
- считать это двумя разными настройками и двумя разными частями UX/данных.


### 26.5. SAP best practices
Нельзя:
- реализовывать кастомную механику там, где стандарт SAP/UI5/Gateway/CDS/BOPF уже покрывает задачу;
- обходить стандартный lifecycle controls/events/models без необходимости;
- оставлять неочевидные архитектурные решения без документирования.

Нужно:
- сначала пытаться решить задачу стандартом платформы;
- использовать best practices SAP как default architectural rule;
- отклоняться от стандарта только при явном доказанном ограничении стандарта и с документированным обоснованием.

### 26.5. Lock takeover
Нельзя:
- делать отдельный self-steal endpoint без нужды;
- хранить несколько несогласованных takeover-параметров.

Нужно:
- один явный `ForceTakeover` flag.

---

## 27. Финальный принцип

Проект должен развиваться по правилу:
- стандарт покрывает — оставляем на стандарте;
- стандарт не покрывает command-сценарий — используем Gateway/BOPF/CDS-связку осмысленно;
- не плодим хрупкие самописки;
- не оставляем “серые зоны” для интерпретации Codex;
- сначала truth, потом implementation.
