# Матрица разрывов между UI5 фронтом и SAP backend

Дата: 2026-03-12

Назначение: документ для архитектора, ABAP/CDS разработчика и владельца backend, чтобы быстро понять:

- что фронт ожидает
- что подтверждено в mock backend
- что подтверждено в доступном SAP ABAP коде
- чего не хватает
- что обязательно нужно уточнить до реализации

## 1. Итоговый вывод

Фронт уже задает очень конкретный backend-контракт.

Проблема не в том, что контракт неясен. Проблема в том, что по текущему репозиторию нет подтверждения, что productive SAP backend уже реализует:

- полный OData V2 контракт из `metadata.xml`
- DDIC модель данных
- CDS read model
- авторизацию
- lock/concurrency модель
- attachments
- analytics
- export

Иными словами: фронт можно считать спецификацией для backend, но backend пока нельзя считать готовым к фронту без дополнительной реализации и прояснения архитектурных решений.

## 2. Gap Matrix

| Область | Что ожидает фронт | Что есть в mock backend | Что видно в SAP ABAP | Чего не хватает | Приоритет |
| --- | --- | --- | --- | --- | --- |
| OData service contract | Один сервис `Z_EHS_PRODUCTION_CONTROL_CKLT_SRV`, OData V2, batch, CSRF, canonical entity sets/function imports | Да, контракт покрыт тестами и canonical API | Видны `MPC_EXT` и `DPC_EXT`, но не весь контракт совпадает с frontend metadata | Выровнять productive SAP metadata строго под frontend contract | P1 |
| Search | `ChecklistSearchSet` с фильтрацией, paging, sort, totals/rates | Реализовано | В доступном ABAP не подтвержден read provider | Нужен read-side provider + CDS/view/model | P1 |
| Detail root | `ChecklistRootSet` | Реализовано | Нет полного подтверждения | Нужны entity read + persistence + mapping | P1 |
| Basic info | `ChecklistBasicInfoSet` | Реализовано | Нет полного подтверждения | Нужны DDIC/CDS/read provider | P1 |
| Checks | `ChecklistCheckSet` | Реализовано | Виден mapper для save delta, но не read side | Нужны child-table/read provider/save semantics | P1 |
| Barriers | `ChecklistBarrierSet` | Реализовано | Виден mapper для save delta, но не read side | Нужны child-table/read provider/save semantics | P1 |
| Create | `CreateChecklist` deep payload | Реализовано | В доступном ABAP не подтвержден productive contract в нужном виде | Нужен productive create flow, id generation, versioning | P1 |
| Autosave | `AutoSave` deep delta + conflict control | Реализовано | Есть общий save engine, но контракт и concurrency надо выровнять | Нужны optimistic concurrency и единый payload contract | P1 |
| Save | `SaveChanges` | Реализовано | Есть wrapper-level save engine | Нужны реальные BO/DDIC/CDS/business validations | P1 |
| Delete | delete через `ChecklistRootSet('<ROOT_KEY>')` | Реализовано | В `DPC_EXT` есть delete flow | Нужны auth, consistency rules, cleanup of children/attachments/locks | P1 |
| Status change | `SetChecklistStatus` | Реализовано | Виден общий save/status контур только частично | Нужна status machine и validations | P1 |
| Copy | `CopyChecklist` | Реализовано | Не подтвержден productive end-to-end flow | Нужен copy service на root/basic/check/barrier/attachments metadata | P2 |
| Permissions | `ChecklistPermissionSet`, `ChecklistCreatePermissionSet` | Реализовано | `AUTHORITY-CHECK` не найден | Нужен явный authorization model + projection в OData | P1 |
| Current user | `CurrentUserSet('CURRENT')` | Реализовано | Не подтвержден productive источник | Нужен current user profile service | P1 |
| Runtime settings | `RuntimeSettingsSet('GLOBAL')` | Реализовано | Не подтвержден productive источник | Нужна config table/view/service | P1 |
| Last change | `LastChangeSet` | Реализовано | Не подтвержден productive источник | Нужен aggregate freshness marker | P1 |
| Lock acquire/heartbeat/release | Полный edit-lock lifecycle | Реализовано | Видно несколько lock-механизмов | Нужен один lock service и единая модель | P1 |
| Lock status | `LockStatusSet` | Реализовано | Не подтвержден productive read model | Нужен read provider на lock state | P1 |
| Attachments | `AttachmentSet`, binary `Value`, delete/read | Реализовано локально | Productive implementation не видна | Нужны storage, virus scan, MIME policy, OData mapping | P1 |
| Dictionaries | `DictionaryItemSet` | Реализовано | Productive implementation не видна | Нужны DDIC/customizing/CDS/service | P2 |
| Person value help | `PersonVHSet` | Реализовано | Productive implementation не видна | Нужен источник HR/person data + auth masking if required | P2 |
| Hierarchy | `GetHierarchy` | Реализовано | Есть только MPL/tree след | Нужно уточнение: единая location hierarchy или несколько деревьев | P2 |
| Analytics summary | `SimpleAnalyticalSet` | Реализовано | Не подтверждено | Нужны analytics tables/views/jobs | P2 |
| Analytics breakdown | `WorkflowAnalyticsBreakdownSet` | Реализовано | Не подтверждено | Нужна filtered aggregation на backend | P2 |
| Analytics refresh | `AnalyticsRefreshStateSet`, `AnalyticsRefreshTrigger` | Реализовано | Не подтверждено | Нужен refresh job state model | P2 |
| Export | `ReportExport` | Реализовано | Не подтверждено | Нужен export service с логикой selected/all | P2 |

## 3. Что нужно обязательно уточнить у backend команды

Ниже перечень вопросов, которые надо закрыть до реализации. Я написал их по-русски так, чтобы можно было почти без правок отправить архитектору или ABAP-команде.

### 3.1 По модели данных

- Что будет источником истины для чеклиста:
  - классические DDIC таблицы
  - BOPF BO
  - CDS + behavior
  - RAP
  - или смешанная схема

- Есть ли уже существующие таблицы под:
  - header/root чеклиста
  - check items
  - barrier items
  - attachments
  - lock state
  - analytics snapshot
  - runtime settings

- Если таблицы уже существуют:
  - какие их технические имена
  - какие поля уже есть
  - какие поля отсутствуют относительно frontend contract

- Что хранится как физические таблицы, а что как derived/read model:
  - totals
  - success rates
  - failed flags
  - analytics breakdown
  - permission projection

### 3.2 По DDIC

- Нужны ли новые DDIC структуры под deep payload:
  - save request
  - save response
  - lock request/response
  - attachment delta
  - export contract

- Уже существуют ли типы вроде:
  - `ZSTR_PCCT_SAVECHANGES_RQ`
  - `ZSTR_PCCT_SAVECHANGES_RS`
  - `ZTAB_PCCT_CHECK_DELTA`
  - `ZTAB_PCCT_BARRIER_DELTA`
  - `ZTAB_PCCT_ATTACH_DELTA`

- Если существуют:
  - совпадают ли они с frontend payload по именам полей
  - где нужна adapter-normalization

### 3.3 По CDS

- Что именно будет отдаваться через CDS, а что через ручной ABAP query/service layer

- Нужны ли отдельные CDS views под:
  - search
  - root/basic detail
  - checks/barriers
  - dictionaries
  - person value help
  - permissions
  - current user
  - runtime settings
  - analytics summary
  - analytics breakdown

- Будут ли CDS только read-model слоем, а все mutations останутся в ABAP service classes/function imports

### 3.4 По OData сервису

- Будет ли сохраняться текущий custom OData V2 contract как есть
  - или планируется его перепроектирование

- Если контракт меняется:
  - какие поля
  - какие entity sets
  - какие function imports
  - кто адаптирует фронт

- Кто утверждает канонический `$metadata`:
  - frontend
  - backend
  - совместно

- Будет ли service name точно:
  - `Z_EHS_PRODUCTION_CONTROL_CKLT_SRV`
  - или это временное имя

### 3.5 По lock/concurrency

- Какая lock-модель будет окончательной:
  - enqueue only
  - custom lock table
  - комбинированная

- Нужен один ответственный источник lock state. Сейчас по репозиторию видно несколько вариантов. Надо уточнить:
  - какой класс/ФМ остается
  - что выводится из эксплуатации

- Что является server truth для concurrency:
  - `ETag`
  - `version_number`
  - `changed_on`
  - или комбинация

- Какой сценарий должен происходить при:
  - stale autosave
  - stale save
  - lock expired
  - lock stolen by another session
  - browser beacon release race

### 3.6 По attachments

- Где физически будут храниться файлы:
  - SAP content server
  - ArchiveLink
  - GOS-like storage
  - custom table/blob store
  - внешний DMS

- Будет ли `Value` реально передаваться через OData `Edm.Binary` как сейчас ожидает фронт
  - или backend хочет отдельный media/resource path

- Если backend хочет media stream или отдельный upload API:
  - нужно отдельно согласовать изменение frontend contract

- Нужны уточнения по:
  - virus scan
  - max size
  - allowed mime/extensions
  - retention
  - delete semantics
  - audit trail

### 3.7 По authorization

- Какие authorization objects уже существуют

- Нужно ли заводить новые, например:
  - `Z_UI5_CHKL`
  - `Z_UI5_CHKL_EXP`
  - `Z_UI5_CHKL_ANA`
  - `Z_UI5_CHKL_ATT`

- Где будет происходить основная авторизация:
  - в OData layer
  - в domain service
  - в BO/BOPF validations
  - в CDS DCL
  - комбинация

- Как формируется `ChecklistPermissionSet`:
  - прямой projection auth result
  - derived object per user+root
  - result of service class

### 3.8 По analytics

- Откуда берутся analytics:
  - live aggregation
  - nightly/materialized snapshot
  - background job refresh

- Нужна ли отдельная таблица состояния refresh

- Что будет источником для:
  - total
  - monthly
  - failed checks/barriers
  - avg rates
  - breakdown by dimension

- Какие dimensions обязательны для breakdown

### 3.9 По export

- `ReportExport` должен работать по двум режимам:
  - `selected`
  - `all`

- Нужно уточнить:
  - какой формат результата нужен на backend
  - где происходит final file generation
  - есть ли ограничения по лимиту
  - должны ли применяться те же search filters, что и на экране

### 3.10 По hierarchy / value help

- `GetHierarchy` сейчас выглядит как generic hierarchy seam. Нужно уточнить:
  - это именно location hierarchy
  - или MPL hierarchy
  - или оба сценария

- Какой объект является ключом hierarchy node:
  - технический node id
  - business location key

- Нужна ли историчность по дате (`DateCheck`)

## 4. Мои предложения по реализации

Ниже не просто вопросы, а рекомендуемая архитектурная позиция.

### 4.1 Что лучше хранить в DDIC таблицах

Предлагаю хранить как физические таблицы:

- root/header чеклиста
- checks
- barriers
- attachments metadata
- lock state
- last change marker
- runtime settings
- analytics refresh state

А вот это можно делать как CDS/read projection или derived model:

- search projection
- basic info projection
- permission projection
- current user projection
- analytics summary
- analytics breakdown

### 4.2 Что лучше делать через CDS

Предлагаю через CDS сделать:

- `ChecklistSearchSet`
- `ChecklistRootSet`
- `ChecklistBasicInfoSet`
- `ChecklistCheckSet`
- `ChecklistBarrierSet`
- `DictionaryItemSet`
- `PersonVHSet`
- `CurrentUserSet`
- `RuntimeSettingsSet`
- analytics read views

Почему:

- это read-heavy сценарии
- проще сопровождать
- легче трассировать поля фронта
- проще согласовать контракт

### 4.3 Что лучше НЕ делать только через CDS

Через чистый CDS не стоит пытаться закрыть:

- `CreateChecklist`
- `AutoSave`
- `SaveChanges`
- `LockAcquire`
- `LockHeartbeat`
- `LockRelease`
- `SetChecklistStatus`
- `CopyChecklist`
- `ReportExport`
- attachment binary write/delete

Это лучше оставить в ABAP service/domain layer, потому что там:

- сложная бизнес-логика
- блокировки
- версии
- валидации
- статусные переходы
- аудирование

### 4.4 Что я бы рекомендовал по технике

- OData оставить V2, потому что фронт уже под него заточен.
- Metadata зафиксировать как канон из `app/localService/metadata.xml`.
- Read-side делать через CDS projections.
- Write-side делать через ABAP service classes.
- Locking свести к одному сервису и одному источнику истины.
- Attachments отделить на:
  - metadata table
  - external/content storage
  - service class для binary transport/policy
- Permission seam сделать отдельным сервисом, а не размазывать по DPC_EXT.

## 5. Что я рекомендую вам отправить backend команде

Можно отправить в таком виде:

1. У нас frontend уже жестко ожидает OData V2 контракт из `metadata.xml`.
2. Просьба подтвердить, какие из сущностей будут реализованы через DDIC+CDS, а какие через ABAP classes/service layer.
3. Просьба явно указать:
   - физические таблицы
   - CDS views
   - authorization objects
   - lock strategy
   - attachment storage strategy
   - analytics strategy
4. Просьба подтвердить, что contract names не меняются без согласования с frontend.

## 6. Практический вывод

Если вам нужно быстро двинуть проект, то у backend команды надо в первую очередь попросить ответить на 6 вещей:

1. Какие будут DDIC таблицы.
2. Какие будут CDS views.
3. Кто реализует и фиксирует OData metadata.
4. Какая будет lock/concurrency модель.
5. Где будут храниться attachments.
6. Какие authorization objects и checks будут использоваться.

Пока эти 6 пунктов не закрыты, backend нельзя считать готовым к этому фронту.
