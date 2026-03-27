# ПОЛНЫЙ АУДИТ ПРОЕКТА — SAP UI5 / Gateway / BOPF

## SAP Lead Architect Audit · BASIS 750 SP15 · HANA2 SP6 · UI5 1.71 · SAP UI 754

\---

## 0\. EXECUTIVE SUMMARY

|Категория|Критичность|Кол-во|
|-|-|-|
|Несоответствие ТЗ (contract gaps)|🔴 BLOCKER|8|
|Ошибки логики / поведения|🔴 BLOCKER|6|
|Дублирование логики / кода|🟠 HIGH|7|
|Over-engineering|🟠 HIGH|5|
|Нарушение SAP Best Practice|🟡 MEDIUM|9|
|CSS/DOM нарушения|🟡 MEDIUM|4|
|Мелкие недоработки|🔵 LOW|8|

**Вердикт:** проект **НЕ ГОТОВ** к production. До релиза необходимо устранить все BLOCKER и HIGH.

\---

## 1\. НЕСООТВЕТСТВИЕ ТЗ — КРИТИЧЕСКИЕ РАСХОЖДЕНИЯ

### 1.1 🔴 \[BLOCKER] `IntegrationFlag` отсутствует в `ChecklistRoot`

**ТЗ §3.2:** в root-сущности обязан быть флаг интеграционного происхождения.

**Факт:** `ChecklistRootSet` (metadata.xml) не содержит `IntegrationFlag`. Поле есть только в `ChecklistSearch`. Детальная карточка читает данные из `ChecklistRootSet` + `ChecklistBasicInfoSet` — ни одна из них не несёт этот флаг.

**Следствие:** `DetailContracts.ROOT\_INTEGRATION\_FLAG = "/current/root/integrationFlag"` и `EnterEditUseCase.requiresIntegrationConfirm()` читают поле, которое **никогда не будет заполнено из Gateway**, потому что `ChecklistRoot` его не возвращает.

Весь флоу подтверждения редактирования интеграционных записей (ТЗ §8.2) мёртв.

**Исправление:** Добавить `<Property Name="IntegrationFlag" Type="Edm.Boolean"/>` в `ChecklistRoot` entity (metadata.xml + backend SEGW).

\---

### 1.2 🔴 \[BLOCKER] `ChecklistBasicInfo` не содержит integration text fields (`obsr\_name`, `obsd\_name`)

**ТЗ §7.1 / §7.4:** обязательные поля — `obsr\_name`, `obsd\_name`, `location\_name` (integration-owned text). Правило fallback: если `\*\_pernr` initial → использовать интеграционный текст.

**Факт:** В `ChecklistBasicInfo` есть только `ObserverFullname` / `ObservedFullname` / `LocationName` — это **derived/computed** поля. Нет отдельных `ObserverName` (integration origin) и `ObserverFullname` (reference-backed). Фронтенд не может реализовать fallback-приоритет (ТЗ §7.3), потому что нет двух раздельных источников.

**Следствие:** невозможно отличить "обогащённое из HR" от "пришло из интеграции".

**Исправление:** добавить в `ChecklistBasicInfo`: `ObserverName` (integration text), `ObservedName` (integration text). `ObserverFullname` / `ObservedFullname` остаются как derived. Frontend должен показывать `ObserverName` когда `ObserverPernr` = initial.

\---

### 1.3 🔴 \[BLOCKER] `AutoSave` и `SaveChanges` не имеют `SessionGuid` как top-level параметр

**ТЗ §11.2:** autosave работает только в edit mode и отправляет delta. ТЗ §12 требует привязки сохранения к активной lock-сессии.

**Факт:** `FunctionImport AutoSave` принимает `Payload (SaveChangesRequest)` + `ClientVersion`. `SaveChanges` то же самое. `SessionGuid` передаётся только внутри `SaveChangesRequest.session\_guid` (ComplexType string-field). Это приемлемо, но является **неявным контрактом**.

При этом `SaveChangesRequest` — ComplexType в metadata.xml с полями: `root, checks, barriers, participants, attachments, session\_guid, client\_version`. Это корректно по структуре, но `session\_guid` как embedded-поле в JSON-строке снижает прозрачность Gateway-контракта и усложняет backend-валидацию без дополнительного парсинга.

**Рекомендация:** Документировать как accepted architectural decision или вынести `SessionGuid` на верхний уровень параметров FI для явной Gateway-валидации.

\---

### 1.4 🔴 \[BLOCKER] `HierarchyNode` не содержит поля из ТЗ §17.3

**ТЗ §17.3:** обязательные поля иерархии:
`HIERARCHY\_RANK`, `HIERARCHY\_TREE\_SIZE`, `HIERARCHY\_PARENT\_RANK`, `HIERARCHY\_ROOT\_RANK`, `HIERARCHY\_LEVEL`, `drill`, `parent\_id`, `node\_id`.

**Факт (metadata.xml):** `HierarchyNode` содержит только:
`NodeID, HierarchyLevel, Description, ParentNodeID, DrillState`.

Отсутствуют: `HIERARCHY\_RANK`, `HIERARCHY\_TREE\_SIZE`, `HIERARCHY\_PARENT\_RANK`, `HIERARCHY\_ROOT\_RANK`. Фронтенд использует `sap.ui.table.TreeTable`, для корректного дерева с сортировкой нужны ранги.

**Исправление:** добавить недостающие поля в `HierarchyNode` entity.

\---

### 1.5 🔴 \[BLOCKER] Hardcoded `toleranceMs: 5500` в use cases (нарушение ТЗ §13.6)

**ТЗ §13.6:** порог freshness является runtime-настройкой и должен приходить с backend runtime settings.

**Факт:** В двух use cases жёстко прошиты магические числа:

* `EnterEditUseCase.js:131` → `toleranceMs: 5500`
* `OpenDetailUseCase.js:244` → `toleranceMs: 5500`
* `CacheValidationUseCase.js:78` → `Number((mInput \&\& mInput.toleranceMs) || 5500)` — fallback корректный

Проблема: вызывающие UseCase передают `5500` явно вместо значения из `masterData>/runtime/CacheToleranceMs`. `TimerDefaults.js` корректно определяет `cacheToleranceMs.defaultValue = 5500` — нужно использовать этот механизм как источник.

**Исправление:** в `EnterEditUseCase` и `OpenDetailUseCase` читать tolerance из runtime settings через `ApplyRuntimeSettingsUseCase`/`masterData model`, а не хардкодить.

\---

### 1.6 🔴 \[BLOCKER] `ChecklistBasicInfo` missing `LPC\_KEY`/`PROF\_KEY` — мэппинг разорван

**ТЗ §5.1:** ключевая политика — `DB\_KEY`, `PARENT\_KEY`. ТЗ §7 — Integration fields priority.

**Факт:** `DetailContracts.CARD\_REQUIRED\_KEYS` ссылается на `"basic.LPC\_KEY"` и `"basic.PROF\_KEY"`, но в `ChecklistBasicInfoSet` поля называются `Lpc` и `Profession`. Frontend-маппинг/state работает с ненормализованными именами.

`AutosaveDetailUseCase.mapFieldDelta` явно маппит `LPC\_KEY -> "Lpc"`, `PROF\_KEY -> "Profession"` — это ручной дублирующий маппер.

**Риск:** рассинхронизация между именами в константах и фактическими OData именами при любом изменении.

\---

### 1.7 🟠 \[HIGH] `CopyChecklist` не запускает lock+timers сразу после copy

**ТЗ §10.3:** после copy открывается новая карточка, сразу в edit mode, сразу lock+timers.

**Факт:** `CopyChecklist` FI возвращает `FunctionResult` (поле `DB\_KEY` нового объекта). Frontend должен после получения нового `DB\_KEY`: вызвать `LockAcquire`, перейти в edit mode, запустить heartbeat/autosave. Проверить это в исходниках не полностью возможно без анализа `CopyChecklist`-use case (файл в архиве не найден отдельно — встроен в другой UseCase).

**Действие:** верифицировать, что UseCase обработки ответа CopyChecklist немедленно вызывает LockAcquire и переходит в CREATE/EDIT mode с запуском таймеров.

\---

### 1.8 🟠 \[HIGH] `LastChangeSet` участие attachments — не верифицировано

**ТЗ §13.4:** `LastChangeSet` должен учитывать root, basic, checks, barriers, **attachments**.

**Факт:** `LastChange` entity (metadata.xml) содержит только `DB\_KEY` + `AggChangedOn`. Это aggregate timestamp — ОК. Но нет подтверждения, что backend обновляет `AggChangedOn` при изменении вложений. Это backend concern, но должно быть задокументировано в `CACHE\_POLICY.txt`.

\---

## 2\. ОШИБКИ ЛОГИКИ / ПОВЕДЕНИЯ

### 2.1 🔴 \[BLOCKER] `releaseOnPageLeave` передаёт `ForceTakeover: false` в LockRelease

**Факт:** `LockAdapter.releaseOnPageLeave()` формирует payload с `ForceTakeover: false`. Но `LockRelease` FunctionImport (metadata.xml) **не имеет параметра `ForceTakeover`**. Это лишний параметр, который Gateway проигнорирует или вернёт ошибку.

```js
oPayload = {
    DB\_KEY: sDbKey,
    SessionGuid: sToken,
    ForceTakeover: false  // ← не существует в FI LockRelease
};
```

**Исправление:** убрать `ForceTakeover` из payload в `releaseOnPageLeave`.

\---

### 2.2 🔴 \[BLOCKER] `EnterEditUseCase` — cache validation tolerance hardcoded, не из runtime

(см. п. 1.5 выше — дублирующий blocker в логике edit flow)

При входе в edit mode tolerance берётся не из runtime settings, а константой. Если backend поднял `CacheToleranceMs = 10000`, frontend всё равно использует 5500 → ложная инвалидация кэша.

\---

### 2.3 🟠 \[HIGH] `ChecklistCheckSet` / `ChecklistBarrierSet`: `sap:updatable="true"` без `sap:creatable`/`sap:deletable`

**ТЗ §11.4:** Delete должен идти через `diff -> mapper -> lt\_modification -> BOPF modify`, не через прямой OData DELETE.

**Факт:** EntitySets Check и Barrier имеют `sap:updatable="true"`, `sap:deletable="false"` — это корректно для MERGE-flow. Но `sap:creatable="false"` означает, что создание строк тоже должно идти через FI `SaveChanges`, что соответствует ТЗ. ✅

**Проблема:** `ChecklistCheckSet` и `ChecklistBarrierSet` НЕ должны использоваться для прямого `oModel.update()` в обход FI. Необходимо убедиться, что нет ни одного места, где код напрямую делает `GatewayClient.rawRead` или `create` на эти entity sets для записи.

\---

### 2.4 🟠 \[HIGH] Barriers visibility binding использует `detail>/current/basic/LPC\_KEY` напрямую

**Факт (Detail.view.xml):**

```xml
visible="{path:'detail>/current/basic/LPC\_KEY', formatter:'.isBarriersVisibleByLpc'}"
```

Поле `LPC\_KEY` — это frontend-state путь, который маппится из `Lpc` OData поля. Это нарушение SAP best practice: visibility logic должна быть в ViewModel (computed property или отдельный флаг в JSONModel), а не сырая привязка к техническому ключу через formatter.

Риск: если rename поля — сломается visibility.

\---

### 2.5 🟠 \[HIGH] `LockStatus` entity имеет два дублирующих поля времени: `ExpiresOn` и `LockExpires`

**Факт (metadata.xml):** `LockStatus` содержит:

* `ExpiresOn: Edm.DateTime`
* `LockExpires: Edm.DateTime`

`LockAdapter.normalizeResult` читает оба: `oResult.LockExpires || oResult.ExpiresOn || ...`

**ТЗ §12:** один canonical contract. Дублирование поля в metadata — техдолг, путающий backend-разработчика.

**Исправление:** определить canonical expiry field, второе — deprecated/удалить.

\---

### 2.6 🟠 \[HIGH] `LockStatus` и `FunctionResult` дублируют поля

**Факт:** `FunctionResult` (возвращаемый из всех lock FI) содержит: `Ok, Success, IsKilled, ExpiresOn, LockExpires, Owner, OwnerSession`. `LockStatus` EntitySet содержит: `Ok, Success, IsKilled, ExpiresOn, LockExpires, Owner, OwnerSession, CanTakeover, OwnerSessionMatch` и т.д.

Два разных типа несут частично одинаковую семантику. `LockAdapter.normalizeResult` используется для обоих, угадывая поля через цепочку OR.

**Риск:** при изменении одного типа second тип молча остаётся несинхронным.

\---

## 3\. ДУБЛИРОВАНИЕ ЛОГИКИ / КОДА

### 3.1 🟠 TimerSanitizer vs RuntimeTimerSanitizer — дублирование

**Факт:** В проекте существуют **оба** файла:

* `app/service/framework/TimerSanitizer.js`
* `app/service/framework/RuntimeTimerSanitizer.js`

Оба занимаются sanitization таймерных значений с clamp/min/max. Это нарушение DRY и ТЗ §4.2 ("не плодить слои без реальной необходимости").

**Исправление:** оставить один файл, второй удалить.

\---

### 3.2 🟠 `StatePaths.js` vs `ModelPathContracts.js` — дублирование путей

**Факт:** В проекте два файла с model paths:

* `app/model/StatePaths.js` — содержит 50+ путей
* `app/service/domain/shared/ModelPathContracts.js` — содержит часть тех же путей (ACTIVE\_OBJECT\_ID, SELECTED\_ID и др.)

`SaveDetailUseCase.js` импортирует оба. Это нарушение "single source of truth" для model paths (ТЗ §22.1).

**Исправление:** слить в один файл или сделать `ModelPathContracts` re-export подмножества из `StatePaths`.

\---

### 3.3 🟠 `DetailContracts.STATES` vs `WorkflowContracts.PERSISTENCE\_STATES` — полное дублирование

**Факт:** `DetailContracts.js` содержит `STATES: { AUTOSAVING, CONFLICT, DIRTY, ERROR, IDLE, ... }`. `WorkflowContracts.js` содержит идентичный `PERSISTENCE\_STATES: { AUTOSAVING, CONFLICT, DIRTY, ERROR, IDLE, ... }`. Буквально один в один.

**Исправление:** оставить один источник, второй — re-export или удалить.

\---

### 3.4 🟠 Дублирование normalizeResult логики в LockAdapter

В `LockAdapter.normalizeResult` через цепочку `||` угадываются поля из двух разных response-типов (`FunctionResult` из FI и `LockStatus` из EntitySet). Это признак того, что два разных типа ответа обрабатываются одним normalizer'ом с накопленным техдолгом.

**Исправление:** два отдельных normalizer — один для FI-ответов, второй для EntitySet-ответов.

\---

### 3.5 🟠 `ChecklistBasicInfo` содержит `ObserverPosition`/`ObserverOrgUnit` которые не используются

**Факт (metadata.xml):** `ChecklistBasicInfo` содержит 4 extra поля: `ObserverPosition, ObserverOrgUnit, ObservedPosition, ObservedOrgUnit`. В SELECTS константе (`ODataEntityContracts.SELECTS.CHECKLIST\_BASIC\_INFO`) они **не включены**: `"DB\_KEY,LocationKey,LocationName,LocationText,Bukrs,ObserverPernr,ObserverFullname,ObservedPernr,ObservedFullname,Lpc,Profession,DateCheck,TimeCheck,TimeZone,EquipName"`.

Поля объявлены в metadata, но никогда не запрашиваются. Либо их нужно запрашивать и показывать, либо убрать из metadata.

\---

### 3.6 🟠 `FunctionResult.Success` + `FunctionResult.Ok` — дублирование boolean-флагов

**Факт (metadata.xml):** `FunctionResult` содержит оба: `Ok: Edm.Boolean` и `Success: Edm.Boolean`. `LockAdapter.normalizeResult`:

```js
var bOk = !!(oResult.success || oResult.Success || oResult.Ok || oResult.lockOk || oResult.ok);
```

5 вариантов для одного булевого значения — явный признак накопленного API-долга.

\---

### 3.7 🔵 `MessageCodeConstants` дублирует ключи в DETAIL и FLOW namespace

Проверить при code review — часть flow-кодов дублирует detail-коды по значению.

\---

## 4\. OVER-ENGINEERING

### 4.1 🟠 343 JS-файла (не считая тестов) — критическая фрагментация

**ТЗ §4.2:** "не плодить façade/runtime/manager/service-слои без реальной необходимости".

**Факт:**

* `app/service/framework/` — **70 файлов** одного framework-слоя
* `app/service/framework/execution/` — 15 файлов execution sub-layer
* `app/service/framework/behavior/` — 10 файлов behavior sub-layer
* Итого: 343 JS-файла на frontend приложение среднего масштаба

SAP Best Practice для SAPUI5 1.71: controller → model → service. Три слоя. Здесь реализовано минимум 7 слоев (controller → controller/behavior → controller/runtime → service/features/runtime → service/domain/usecases → service/domain/runtime → infra/adapters → service/framework/execution/behavior).

**Следствие:** новый разработчик не может понять, где реально происходит логика без 2+ часов трассировки.

\---

### 4.2 🟠 `scripts/` — 150+ governance-скриптов для CI не относятся к app

Папка `scripts/` содержит \~150 JS/Python/Shell gate-скриптов: `duplicate-governance-gate.js`, `enterprise-readiness-gate.js`, `wave-e-release-governance-gate.js` и т.д.

Это governance infrastructure для команды, но она создаёт false sense of security — скрипты проверяют структуру, но не бизнес-логику. При этом некоторые скрипты (`scripts/gates/`) явно дублируют функциональность eslint/stylelint.

**Рекомендация:** сократить до ≤20 критичных gate-скриптов, остальное — в eslint rules или jshint.

\---

### 4.3 🟠 `Effects` system — custom event bus поверх UI5 event system

**Факт:** Весь проект использует самописную систему `Effects` (`app/service/framework/Effects.js`, `EffectRuntime.js`, `EffectApplier.js`, `EffectBannerRouter.js`, `EffectDialogRuntime.js`, `EffectFeedbackRuntime.js`, `EffectModelRuntime.js`, `EffectToastRuntime.js`, `FeedbackBannerRuntime.js`, `FeedbackBannerState.js`) — 10 файлов только для "effects".

**ТЗ §4.2 / §22.2:** запрещено строить "самописные hidden frameworks поверх UI5". UI5 1.71 имеет: `sap.ui.core.EventBus`, `MessageManager`, `MessageToast`, `MessageBox`, `BusyDialog`. Всё это стандартные механизмы.

**Следствие:** Effects система — это самописный Redux-подобный pattern поверх UI5 без реальной необходимости.

\---

### 4.4 🟠 `CtxAdapterFactory` + `CtxRuntimeFactory` — фабрики контекстов поверх DI

Вместо стандартного UI5 подхода (controller owns dependencies, Component provides models) создан собственный DI/context механизм.

\---

### 4.5 🔵 `BehaviorRegistry` + `BehaviorResolver` — излишняя абстракция

Для SAPUI5 1.71 pattern "registry + resolver" для behaviors оверкиллинг. Стандарт: контроллер напрямую вызывает методы behavior-миксинов.

\---

## 5\. НАРУШЕНИЯ SAP BEST PRACTICE

### 5.1 🟠 `index.html` загружает кастомные CSS **до** bootstrap UI5

**Факт (index.html):**

```html
<link rel="stylesheet" href="styles/modules/05\_shell\_host.css">
```

Этот CSS загружается **до** UI5 bootstrap. В UI5 best practice: все app CSS должны регистрироваться через `manifest.json → sap.ui5.resources.css`, что гарантирует правильный порядок загрузки после UI5 theming.

**Исправление:** убрать прямой `<link>` из index.html, оставить только через manifest.

\---

### 5.2 🟠 `manifest.json`: модель `mainService` не имеет `annotationURI`

Для `sap.ui.comp.SmartTable` и `sap.ui.comp.SmartFilterBar` (которые используются в проекте согласно manifest deps) рекомендуется подключать OData annotations. Без них SmartControls работают без метаданных label/validation.

\---

### 5.3 🟠 `manifest.json`: `defaultCountMode: "Inline"` может вызвать проблемы производительности

При `Inline` count Gateway выполняет `$count` на каждый запрос. Для search с большими таблицами ТЗ §14.8 явно указывает: "total count является опциональным, нельзя заставлять backend всегда выполнять дорогой count".

**Исправление:** `defaultCountMode: "None"` и управлять count явно только там, где нужно.

\---

### 5.4 🟠 `useBatch: true` в manifest, но `callFunctionImport` создаёт операции вне batch

**Факт:** Lock operations (acquire/heartbeat/release) идут через `GatewayClient.callFunctionImport` который использует `oModel.create()` напрямую, а не через deferred group. Поэтому они выполняются вне batch несмотря на `useBatch: true`.

Это фактически правильное поведение для lock (нельзя ждать flush batch), но `useBatch: true` + manual non-batch операции — архитектурное несоответствие, требующее явного документирования.

\---

### 5.5 🟠 `GatewayClient.uploadMedia` использует `window.fetch` для media upload вместо UI5 `FileUploader`

**Факт:** Media upload идёт через bare `window.fetch`. SAP Best Practice: использовать `sap.ui.unified.FileUploader` или `sap.m.UploadCollection` (доступны в UI5 1.71).

**Исключение:** если нужен drag-and-drop с прогрессом — `window.fetch` допустим, но должен быть задокументирован как explicit deviation.

\---

### 5.6 🟠 `CopyChecklist` вызывается через `urlParameters` (GET-style POST)

**Факт:** `GatewayClient` помещает `CopyChecklist` в `DIRECT\_FUNCTION\_QUERY\_ALLOWLIST`:

```js
ensureModel().callFunction("/" + sFunctionName, {
    method: REQUEST.POST,
    urlParameters: oPayload || {},
    ...
});
```

Т.е. payload передаётся как URL-параметры в POST. Для `DB\_KEY: Edm.Binary` — это проблема, потому что binary key в URL требует специального encoding. SAP Gateway обычно требует binary в теле запроса для POST.

\---

### 5.7 🟠 `ObserverPosition`/`ObservedPosition` объявлены в metadata, но не возвращаются через `$select`

Неиспользуемые поля в OData entity — нарушение принципа minimal contract.

\---

### 5.8 🟡 `sap.viz` подключён как зависимость без проверки использования

`sap.viz` — тяжёлая библиотека. В `manifest.json` она прописана в dependencies. Нужно убедиться, что она реально используется (analytics screen), иначе убрать.

\---

### 5.9 🔵 `flexEnabled: true` без настроенного LREP/Adaptation Project

`flexEnabled: true` в manifest при отсутствии LREP backend создаёт лишние XHR-запросы к `/sap/bc/lrep/` при каждом старте приложения.

\---

## 6\. CSS / DOM НАРУШЕНИЯ

### 6.1 🟠 14 CSS-файлов в quarantine с `.sap\*` internal selectors — техдолг не закрыт

**Факт (CSS\_DOM\_VIOLATIONS.md):** 14 CSS-файлов содержат SAP-internal selectors и помещены в allowlist. Это принятый технический долг, но он **не устранён**. На production это риск при каждом обновлении UI5.

**ТЗ §22.2:** "DOM hacks" и "patching внутренних CSS-классов SAP controls" — запрещено.

\---

### 6.2 🟠 `index.html`: `class="chkAppRoot light-mode theme-motion-enabled"` на `<html>` элементе

SAP UI5 управляет классами на `<body>` через theming. Добавление классов на `<html>` — нестандартный подход, который может конфликтовать с UI5 theme engine.

\---

### 6.3 🟡 `app/styles/modules/` — 7977 строк CSS в 20+ файлах

Для UI5 1.71 приложения это избыточный CSS. SAP Best Practice: использовать UI5 theming parameters (`@sapUiBaseColor` и т.д.) через CSS variables, а не custom CSS override system.

\---

### 6.4 🔵 `<link rel="icon" href="data:,">` в index.html — пустая favico

Технически работает, но некорректно для production.

\---

## 7\. МЕЛКИЕ НЕДОРАБОТКИ

### 7.1 🔵 `manifest.json` version `1.19.0` — устаревший \_version формат

Текущий формат для UI5 1.71: `"\_version": "1.23.0"` и выше.

### 7.2 🔵 `routing.config` не имеет `bypassed.target` — нет обработки 404-route

### 7.3 🔵 `AttachmentFolderSet` объявлен в metadata, но не используется в constants

`GatewayContractConstants.ENTITY\_SETS` не содержит `ATTACHMENT\_FOLDER`. Либо это будущий функционал, либо мёртвый код.

### 7.4 🔵 `SaveChangesResponse` ключевое поле `pcct\_uuid: Edm.String` — нарушает DB\_KEY политику

ТЗ §5.1: canonical key = `DB\_KEY: Edm.Binary`. `SaveChangesResponse` использует `pcct\_uuid (Edm.String)` как ключ — это отклонение от canonical key policy. Frontend в `SaveDetailUseCase` уже компенсирует: `oInitialSavedSnapshot.pcct\_uuid || oInitialSavedSnapshot.DB\_KEY` — но это признак нерешённого несоответствия.

### 7.5 🔵 `CurrentUser.PermissionsCsv` + `PermissionRulesJson` — два способа передачи permissions

Один из них должен быть deprecated. Нужно выбрать canonical.

### 7.6 🔵 `SimpleAnalytical.RefreshedAt: Edm.String` вместо `Edm.DateTime`

DateTime fields должны использовать правильный Edm type для корректной сериализации в OData V2.

### 7.7 🔵 `AnalyticsRefreshState` datetime fields как `Edm.String` — аналогично

`RequestedAt, StartedAt, FinishedAt, LastSuccessAt` — все `Edm.String` вместо `Edm.DateTime`.

### 7.8 🔵 `ui5.yaml` — devServer конфигурация

Нужно проверить наличие CSP headers и CORS policy для production deployment.

\---

## 8\. СООТВЕТСТВИЕ ТЗ — СВОДНАЯ МАТРИЦА

|ТЗ раздел|Статус|Комментарий|
|-|-|-|
|§3 Интеграционный флаг в Root|🔴 НЕТ|`ChecklistRoot` не содержит `IntegrationFlag`|
|§5 DB\_KEY/PARENT\_KEY policy|✅ ДА|Соблюдено в metadata и constants|
|§6 Status lifecycle|✅ ДА|WorkflowContracts содержит все статусы|
|§7 Integration text fields|🔴 НЕТ|`ObserverName`/`ObservedName` (integration origin) отсутствуют|
|§8 Enter edit flow|🟡 ЧАСТИЧНО|Логика есть, но мёртвая (п.1.1)|
|§9 Create flow|✅ ДА|CreateChecklist FI, CreateSentinel pattern|
|§10 Copy flow|🟡 НЕ ВЕРИФИЦИРОВАНО|Требует проверки lock после copy|
|§11 Save/Autosave pipeline|✅ ДА|FI-based, DeltaPayload, no direct DELETE|
|§12 Locking|🟠 ПОЧТИ|ForceTakeover ✅, но releaseOnPageLeave лишний param|
|§13 Cache/LastChangeSet|🔴 ЧАСТИЧНО|toleranceMs hardcoded в 2 местах|
|§14 Search|✅ ДА|SmartTable/SmartFilterBar, режимы exact/inexact|
|§15 Detail lazy loading|✅ ДА|Skeleton + async load pattern|
|§16 Person Suggest|✅ ДА|PersonVHSet, только в edit mode|
|§17 Location hierarchy|🔴 НЕТ|HierarchyNode не содержит HIERARCHY\_RANK поля|
|§18 Attachments/ArchiveLink|✅ ДА|metadata-first, DownloadUrl/DocumentHandle|
|§19 Permissions|✅ ДА|Operation codes в constants|
|§20 Runtime Settings|✅ ДА|RuntimeSettingsSet, все backend-driven params|
|§21 Analytics|✅ ДА|SimpleAnalytical + WorkflowAnalyticsBreakdown|
|§22 SAP Best Practice|🟠 ЧАСТИЧНО|Effects system, CSS quarantine, fetch upload|

\---

## 9\. ПРОМПТ ДЛЯ CODEX НА УСТРАНЕНИЕ ВСЕХ ЗАМЕЧАНИЙ

```
SYSTEM: Ты — SAP UI5/Gateway/BOPF разработчик. Работаешь с проектом Production Control Checklist.
Стек: SAP BASIS 750 SP15, HANA2 SP6, SAP UI 754, SAPUI5 1.71, Gateway OData V2, BOPF.
Браузер: MS Edge (современный). IE не поддерживается.
Ключи сущностей: DB\_KEY (Edm.Binary, root и children), PARENT\_KEY (Edm.Binary, ссылка на root у child-узлов; у root отсутствует).

Перед началом работы:
1. Прочитай полностью AUDIT\_REPORT.md
2. Прочитай ТЗ: TZ\_2\_1\_SAP\_Gateway\_UI5\_Final\_v2.md
3. Построй/обнови TRUTH\_MATRIX.txt по фактическому состоянию кода
4. Работай только по документированным фактам, не выдумывай поведение

═══════════════════════════════════════
ЗАДАЧА 1 — BLOCKER: добавить IntegrationFlag в ChecklistRoot
═══════════════════════════════════════
Файл: app/localService/metadata.xml
Действие: добавить в EntityType ChecklistRoot поле:
  <Property Name="IntegrationFlag" Type="Edm.Boolean" sap:label="Integration flag"/>
Обоснование: ТЗ §3.2 требует флаг интеграционного происхождения в root-сущности.
Также добавить IntegrationFlag в SELECTS.CHECKLIST\_ROOT в ODataEntityContracts.js,
обновить ODataChecklistReadRuntime.js чтобы поле читалось при загрузке root.
ABAP: добавить соответствующее поле в структуру CDS view для ChecklistRoot.

═══════════════════════════════════════
ЗАДАЧА 2 — BLOCKER: добавить integration text fields в ChecklistBasicInfo
═══════════════════════════════════════
Файл: app/localService/metadata.xml
Действие: добавить в EntityType ChecklistBasicInfo:
  <Property Name="ObserverName" Type="Edm.String" sap:label="Observer integration name"/>
  <Property Name="ObservedName" Type="Edm.String" sap:label="Observed integration name"/>
Обоснование: ТЗ §7.1 — обязательные integration-owned text fields.
Обновить SELECTS.CHECKLIST\_BASIC\_INFO в ODataEntityContracts.js — добавить поля.
Обновить frontend display logic: если ObserverPernr = '' → показывать ObserverName, иначе ObserverFullname.
ABAP: добавить поля OBSERVER\_NAME / OBSERVED\_NAME в CDS view ChecklistBasicInfo, маппировать из BOPF-ноды.

═══════════════════════════════════════
ЗАДАЧА 3 — BLOCKER: исправить toleranceMs в EnterEditUseCase и OpenDetailUseCase
═══════════════════════════════════════
Файлы:
  app/service/domain/detail/usecases/EnterEditUseCase.js (строка \~131)
  app/service/domain/detail/usecases/OpenDetailUseCase.js (строка \~244)
Действие: заменить hardcoded 5500 на значение из runtime settings.
Алгоритм: 
  var iTolerance = (mCtx \&\& mCtx.runtimeSettings \&\& mCtx.runtimeSettings.cacheToleranceMs) 
                   || TimerDefaults.cacheToleranceMs.defaultValue;
  oCacheValidation.execute({ rootId: sDbKey, toleranceMs: iTolerance }, mCtx)
Убедиться, что runtimeSettings передаётся в ctx при вызове use case.
Обоснование: ТЗ §13.6.

═══════════════════════════════════════
ЗАДАЧА 4 — BLOCKER: исправить releaseOnPageLeave — убрать ForceTakeover
═══════════════════════════════════════
Файл: app/infra/adapters/LockAdapter.js
Действие: в функции releaseOnPageLeave убрать ForceTakeover из oPayload:
  oPayload = {
    DB\_KEY: sDbKey,
    SessionGuid: sToken
    // ForceTakeover НЕ является параметром LockRelease
  };
Обоснование: FunctionImport LockRelease принимает только DB\_KEY + SessionGuid.

═══════════════════════════════════════
ЗАДАЧА 5 — BLOCKER: добавить недостающие поля в HierarchyNode
═══════════════════════════════════════
Файл: app/localService/metadata.xml
Действие: добавить в EntityType HierarchyNode:
  <Property Name="HierarchyRank" Type="Edm.Int32"/>
  <Property Name="HierarchyTreeSize" Type="Edm.Int32"/>
  <Property Name="HierarchyParentRank" Type="Edm.Int32"/>
  <Property Name="HierarchyRootRank" Type="Edm.Int32"/>
Обоснование: ТЗ §17.3. Поля нужны для корректной работы TreeTable.
ABAP: добавить поля в структуру результата FM GetHierarchy, заполнять из иерархии.

═══════════════════════════════════════
ЗАДАЧА 6 — BLOCKER: унифицировать LPC\_KEY/PROF\_KEY мэппинг
═══════════════════════════════════════
Файл: app/constants/DetailContracts.js
Действие: CARD\_REQUIRED\_KEYS.lpc должен ссылаться на OData-canonical имя "basic.Lpc", 
не на "basic.LPC\_KEY". Либо создать единый маппинг в ODataChecklistPayloadMapper.js:
  { frontendKey: "LPC\_KEY", odataField: "Lpc" }
  { frontendKey: "PROF\_KEY", odataField: "Profession" }
Убрать дублирующий mapFieldDelta в AutosaveDetailUseCase — использовать общий маппер.

═══════════════════════════════════════
ЗАДАЧА 7 — HIGH: устранить дублирование TimerSanitizer
═══════════════════════════════════════
Анализ: проверить различия между TimerSanitizer.js и RuntimeTimerSanitizer.js.
Если логика идентична или один является подмножеством другого — оставить один файл.
Обновить все импорты на оставшийся файл.
Обоснование: ТЗ §4.2.

═══════════════════════════════════════
ЗАДАЧА 8 — HIGH: слить дублирующие path constants
═══════════════════════════════════════
Файлы: app/model/StatePaths.js и app/service/domain/shared/ModelPathContracts.js
Действие: ModelPathContracts должен re-export пути из StatePaths, а не дублировать их.
  export { ACTIVE\_OBJECT\_ID, SELECTED\_ID, POST\_OPEN\_HYDRATED\_ROOT\_ID } from StatePaths;
Обоснование: ТЗ §19.6 single source of truth для constants.

═══════════════════════════════════════
ЗАДАЧА 9 — HIGH: устранить дублирование STATES между DetailContracts и WorkflowContracts
═══════════════════════════════════════
DetailContracts.STATES === WorkflowContracts.PERSISTENCE\_STATES (идентичные объекты).
Действие: удалить DetailContracts.STATES, везде использовать WorkflowContracts.PERSISTENCE\_STATES.
Обновить все импортеры DetailContracts.STATES.

═══════════════════════════════════════
ЗАДАЧА 10 — HIGH: исправить manifest.json defaultCountMode
═══════════════════════════════════════
Файл: app/manifest.json
Действие: изменить "defaultCountMode": "Inline" на "defaultCountMode": "None"
Обоснование: ТЗ §14.8 — total count опционален.
Там где count нужен (search summary) — передавать $count явно через urlParameters.

═══════════════════════════════════════
ЗАДАЧА 11 — HIGH: исправить LockStatus дублирование ExpiresOn/LockExpires
═══════════════════════════════════════
Файл: app/localService/metadata.xml
Действие: определить canonical expiry поле.
  Оставить: LockExpires (используется в FunctionResult как LockExpires)
  Deprecated: ExpiresOn — помечать в комментарии или удалить если не используется backend
Обновить LockAdapter.normalizeResult:
  expiresAt: oResult.LockExpires || oResult.ExpiresOn
  → expiresAt: oResult.LockExpires   // canonical only
ABAP: использовать одно поле в ответе.

═══════════════════════════════════════
ЗАДАЧА 12 — MEDIUM: исправить index.html — убрать прямой CSS link
═══════════════════════════════════════
Файл: app/index.html
Действие: убрать <link rel="stylesheet" href="styles/modules/05\_shell\_host.css">
05\_shell\_host.css уже подключается через manifest.json → sap.ui5.resources.css → styles/app-styles.css
(если app-styles.css @import-ит 05\_shell\_host.css) или добавить его отдельной записью в manifest.
Обоснование: SAP Best Practice §5.1 настоящего аудита.

═══════════════════════════════════════
ЗАДАЧА 13 — MEDIUM: исправить CopyChecklist HTTP метод и payload strategy
═══════════════════════════════════════
Файл: app/service/backend/GatewayClient.js
DIRECT\_FUNCTION\_QUERY\_ALLOWLIST содержит CopyChecklist — это значит payload идёт как urlParameters.
Для DB\_KEY (Edm.Binary) это проблема.
Действие: перенести CopyChecklist в DIRECT\_FUNCTION\_BODY\_ALLOWLIST.
Тогда payload пойдёт в тело POST запроса через oModel.create().
ABAP: убедиться что handler CopyChecklist читает параметры из body, не из URL.

═══════════════════════════════════════
ЗАДАЧА 14 — MEDIUM: исправить AnalyticsRefreshState datetime fields
═══════════════════════════════════════
Файл: app/localService/metadata.xml
Действие: изменить типы полей в AnalyticsRefreshState:
  RequestedAt, StartedAt, FinishedAt, LastSuccessAt → Type="Edm.DateTime"
  (сейчас Edm.String — неверно для datetime в OData V2)
Аналогично SimpleAnalytical.RefreshedAt.

═══════════════════════════════════════
ЗАДАЧА 15 — MEDIUM: документировать Effects system как explicit architectural decision
═══════════════════════════════════════
Файл: RESIDUAL\_RISKS.txt или новый ARCHITECTURAL\_DECISIONS.md
Действие: задокументировать почему используется кастомный Effects/EffectRuntime pattern 
вместо стандартного UI5 EventBus + MessageManager.
Если обоснования нет — создать план постепенного перехода на стандартный UI5 approach.
Обоснование: ТЗ §22.2.

═══════════════════════════════════════
ЗАДАЧА 16 — LOW: SaveChangesResponse key — документировать pcct\_uuid vs DB\_KEY
═══════════════════════════════════════
Файл: app/localService/metadata.xml + TRUTH\_MATRIX.txt
Действие: задокументировать почему SaveChangesResponse использует pcct\_uuid (Edm.String)
а не DB\_KEY (Edm.Binary) как ключ ответа.
Frontend уже корректно обрабатывает оба варианта в SaveDetailUseCase.
Нужно либо унифицировать на DB\_KEY, либо явно задокументировать как accepted deviation.

═══════════════════════════════════════
ПОРЯДОК ВЫПОЛНЕНИЯ:
1. Построить TRUTH\_MATRIX.txt (фактическое состояние до изменений)
2. Выполнить задачи 1-6 (BLOCKER) — backend metadata + frontend logic
3. Выполнить задачи 7-11 (HIGH) — рефакторинг без изменения поведения
4. Выполнить задачи 12-15 (MEDIUM) — best practice
5. Задача 16 (LOW) — документирование
6. Обновить REQUIREMENTS\_BASELINE.txt и RESIDUAL\_RISKS.txt
7. Запустить все gate-скрипты: node scripts/qa-all.js
```

\---

## 10\. КРИТИЧЕСКИЕ РИСКИ ДЛЯ PRODUCTION

|#|Риск|Вероятность|Влияние|
|-|-|-|-|
|1|IntegrationFlag отсутствует в Root → весь confirm-dialog для integration records мёртв|ВЫСОКАЯ|КРИТИЧЕСКОЕ|
|2|toleranceMs hardcoded → неправильная freshness validation при runtime settings отличных от 5500|ВЫСОКАЯ|ВЫСОКОЕ|
|3|HierarchyNode без rank полей → TreeTable не сортируется корректно|ВЫСОКАЯ|ВЫСОКОЕ|
|4|releaseOnPageLeave с лишним param → возможна ошибка при page unload|СРЕДНЯЯ|СРЕДНЕЕ|
|5|CopyChecklist с urlParameters для Binary key → encoding issue|СРЕДНЯЯ|ВЫСОКОЕ|
|6|14 CSS files с SAP-internal selectors → сломается при обновлении UI5|СРЕДНЯЯ|СРЕДНЕЕ|
|7|defaultCountMode=Inline → производительность на больших данных|ВЫСОКАЯ|ВЫСОКОЕ|

\---

*Аудит выполнен на основании: кода проекта (app.zip), ТЗ (TZ\_2\_1\_SAP\_Gateway\_UI5\_Final\_v2.md), SAP UI5 1.71 Best Practices, SAP Gateway OData V2 guidelines, BASIS 750 SP15 constraints.*

