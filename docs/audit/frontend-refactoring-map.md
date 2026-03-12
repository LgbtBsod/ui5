# Frontend Refactoring Map

Date: 2026-03-12
Scope: `app/`
Goal: define a detailed migration path from the current mixed frontend architecture to a modular `SRP` structure with smart contracts, maximum code reuse for equivalent behavior, centralized constants/text tokens, and extension through behavior modules

## Execution Status

- `[done]` Step 0. Create the refactoring map and execution ledger
- `[done]` Step 1. Normalize constants and token ownership in the highest-risk frontend modules
- `[pending]` Step 2. Consolidate state-path authority
- `[done]` Step 3. Decompose thick controller/support orchestration
- `[done]` Step 4. Move repeated interaction behavior into behavior packs
- `[pending]` Step 5. Centralize contract mapping and normalization
- `[pending]` Step 6. Introduce progressive readiness contracts
- `[pending]` Step 7. Simplify shell and theme ownership
- `[pending]` Step 8. Split large CSS surfaces by local ownership
- `[pending]` Step 9. Add architecture guardrails to keep the target model enforceable
- `[done]` Step 10. Drain repeated model keys and operation source labels from support/framework modules

## Completed Wave: App And Detail Support Drain

- `AppControllerDomActions` physically drained into `controller/app/AppDomBehavior.js`.
- `AppControllerOverlayActions` physically drained into `controller/app/AppOverlayBehavior.js`.
- `AppShellActionRuntime` physically drained into `controller/app/AppShellActionRuntime.js`.
- `AttachmentUploadCore` physically drained into `controller/detail/AttachmentUploadCore.js`.
- `DetailValidationSummarySupport` physically drained into `controller/detail/DetailValidationSummarySupport.js`.
- Controller imports and dependent support modules now reference the capability-based app/detail paths.

## Completed Wave: Detail Viewport And Shared Helper Normalization

- `AttachmentDropZoneRuntime` physically drained into `controller/detail/AttachmentDropZoneRuntime.js`.
- `DetailActionViewportRuntime` physically drained into `controller/detail/DetailActionViewportBehavior.js`.
- `DetailPersonInputSupport` physically drained into `controller/detail/DetailPersonInputSupport.js`.
- `BindingContextReadSupport` was rehomed to `service/shared/BindingContextReadSupport.js` as a neutral shared helper instead of a controller-support artifact.
- Detail and search consumers were switched to the new detail/shared locations.

## Completed Wave: Analytics Capability Split

- `AnalyticsControllerActions` physically drained into `controller/analytics/AnalyticsControllerBehavior.js`.
- `AnalyticsYearRuntime.js` now owns year normalization, compare-year defaults, preset application, and year-picker dialog behavior.
- `AnalyticsLoadRuntime.js` now owns analytics load orchestration.
- `AnalyticsRefreshRuntime.js` now owns refresh polling semantics.
- `AnalyticsDrilldownRuntime.js` now owns drilldown intent creation and navigation handoff.
- `AnalyticsExportRuntime.js` now owns export/report dialog behavior.

## Governing Rules

### Rule 1: One owner per concern

Each concern must have exactly one primary owner:
- domain intent -> use case
- reusable interaction behavior -> behavior module
- state semantics -> state contract plus state access service
- transport mapping -> adapter and boundary mapper
- generic execution helpers -> framework support only

### Rule 2: Controllers must be thin

Controllers may:
- extract event context
- call a use case or behavior coordinator
- apply returned effects

Controllers may not:
- interpret backend contracts
- own business rules
- manage duplicate state-path logic
- build repeated busy/error/feedback choreography locally

### Rule 3: Smart contracts are hard boundaries

Contracts must be the only authoritative place for:
- workflow modes and lock states
- dialog contracts
- navigation contracts
- analytics contracts
- state readiness contracts
- view-path contracts

No local module should silently redefine those rules.

### Rule 4: Shared component behavior must live in behavior modules

Equivalent UI behavior across equivalent components must be implemented once and extended through default/override behavior handlers.

### Rule 5: All text tokens and reusable variables must be centralized

All reusable text tokens, model names, event names, state model identifiers, path literals, mode names, and similar repeated string variables must live in constants or contract modules.

Reason:
- engineers should not need to remember exact spellings
- editing should happen in one place
- drift across modules must be reduced

This applies to:
- model names such as `"state"`, `"selected"`, `"appView"`
- readiness keys
- dialog keys
- route names
- operation names
- repeated UX labels and fallback text keys
- repeated literal state paths

Inline string literals are allowed only when they are truly local and single-use.

### Canonical source-of-truth map

Use exactly one canonical source per constant family:

- shared business contracts:
  - [app/contracts/AnalyticsContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/contracts/AnalyticsContracts.js)
  - [app/contracts/NavigationContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/contracts/NavigationContracts.js)
  - [app/contracts/WorkflowContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/contracts/WorkflowContracts.js)
- service and infra aliases:
  - [app/service/contracts/AnalyticsContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/contracts/AnalyticsContracts.js)
  - [app/service/contracts/NavigationContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/contracts/NavigationContracts.js)
  - [app/service/contracts/WorkflowContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/contracts/WorkflowContracts.js)
  - [app/infra/contracts/AnalyticsContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/contracts/AnalyticsContracts.js)
  - [app/infra/contracts/NavigationContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/contracts/NavigationContracts.js)
  - [app/infra/contracts/WorkflowContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/contracts/WorkflowContracts.js)
  - these files are aliases only and must not hold independent literal truth
- shared UI state paths:
  - [app/model/StatePaths.js](/C:/Users/lgbtb/Desktop/ui5/app/model/StatePaths.js)
- domain-facing state path bridge:
  - [app/service/domain/shared/ModelPathContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/shared/ModelPathContracts.js)
- deprecated compatibility alias:
  - [app/service/domain/shared/ModelPathContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/shared/ModelPathContracts.js)
  - alias only, no own literals
- shared model names and generic UI tokens:
  - [app/service/contracts/ModelContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/contracts/ModelContracts.js)
- frontend runtime config:
  - [app/service/contracts/FrontendConfigConstants.js](/C:/Users/lgbtb/Desktop/ui5/app/service/contracts/FrontendConfigConstants.js)
  - [app/service/framework/FrontendConfigConstants.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/FrontendConfigConstants.js) remains alias-only

## Current Structural Errors

### Error A: Two competing implementation paths

Current paths:
- domain/contracts/behavior path
- controller-support/runtime orchestration path

Required change:
- make the domain/contracts/behavior path the only valid path for new feature behavior

### Error B: Support files behave like feature engines

Affected files:
- [SearchControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/search/SearchControllerBehavior.js)
- [AnalyticsControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/analytics/AnalyticsControllerBehavior.js)
- [DetailViewRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailViewBehavior.js)
- [SearchViewportRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchViewportRuntime.js)
- [SearchSelectionRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchSelectionRuntime.js)
- [SearchViewRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/search/SearchViewBehavior.js)
- [DetailChecklistRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailChecklistBehavior.js)

Required change:
- split by intent
- route interaction behavior through behavior packs and use cases

### Error C: State path authority is split

Affected files:
- [StatePaths.js](/C:/Users/lgbtb/Desktop/ui5/app/model/StatePaths.js)
- [DomainStatePaths.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/shared/ModelPathContracts.js)
- [ModelPathContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/shared/ModelPathContracts.js)

Required change:
- `StatePaths.js` becomes the canonical path source
- `ModelPathContracts.js` becomes the sanctioned domain-facing export
- `DomainStatePaths.js` should be retired or reduced to compatibility only, then removed

### Error D: Constants are inconsistent and duplicated

Signals:
- multiple constants modules exist, but not all repeated tokens route through them
- model-name and path strings still appear directly inside support modules

Required change:
- create a canonical frontend constant surface
- migrate repeated string variables into constants

## Detailed File-Level Refactoring Map

## 1. State And Constant Authority

### 1.1 [app/model/StatePaths.js](/C:/Users/lgbtb/Desktop/ui5/app/model/StatePaths.js)

Current role:
- primary state path source

Target role:
- single canonical repository for UI state paths

Actions:
- keep as canonical path file
- expand only when a new truly shared state path is introduced
- forbid duplicate path constants elsewhere

Status:
- `[not_started]` full enforcement

### 1.2 [app/service/domain/shared/ModelPathContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/shared/ModelPathContracts.js)

Current role:
- parallel path authority

Target role:
- compatibility shim only during migration, then removal

Actions:
- replace direct local consumption with [ModelPathContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/shared/ModelPathContracts.js)
- shrink to aliases if still needed
- mark as deprecated

Status:
- `[pending]`

### 1.3 [app/service/domain/shared/ModelPathContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/shared/ModelPathContracts.js)

Current role:
- partial contract bridge to `StatePaths`

Target role:
- sanctioned domain-facing model path contract

Actions:
- expand with repeated model names and commonly reused state references
- use this instead of direct literal model identifiers in support modules

Status:
- `[done]`

### 1.4 New canonical constant modules

Required new files:
- `app/service/contracts/ModelContracts.js`
- `app/service/contracts/ReadinessContracts.js`
- `app/service/contracts/OperationContracts.js`

Purpose:
- centralize repeated text tokens and reusable variables

Planned contents:
- model names: `state`, `selected`, `appView`, `i18n`
- readiness names
- operation keys and repeated orchestration labels

Status:
- `[done]` initial implementation via `ModelContracts.js`

## 2. Thick Support Modules To Cut First

### 2.1 [app/controller/search/SearchControllerBehavior.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/search/SearchControllerBehavior.js)

Problem:
- too many concerns in one module

Likely mixed responsibilities:
- bootstrap
- search execution
- selection side effects
- export
- analytics bridge
- feedback/busy handling

Target split:
- `SearchBootstrapBehavior`
- `SearchExecutionBehavior`
- `SearchSelectionBehavior`
- `SearchExportBehavior`
- `SearchFeedbackBehavior`

What to move where:
- request shaping -> search use cases
- selection interaction rules -> behavior module
- busy/error/banner handling -> shared behavior
- search-specific state writes -> search state access service

Status:
- `[pending]`

### 2.2 [app/controller/analytics/AnalyticsControllerBehavior.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/analytics/AnalyticsControllerBehavior.js)

Problem:
- analytics orchestration is too broad and likely coupled to general page logic

Target split:
- `AnalyticsLoadBehavior`
- `AnalyticsRefreshBehavior`
- `AnalyticsExportBehavior`
- `AnalyticsPresentationBehavior`

What to move where:
- payload normalization -> analytics domain normalizer
- deferred readiness -> readiness behavior
- export side effects -> dedicated analytics export behavior

Status:
- `[pending]`

### 2.3 [app/controller/detail/DetailViewBehavior.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailViewBehavior.js)

Problem:
- detail runtime is too central

Target split:
- `DetailOpenBehavior`
- `DetailHydrationBehavior`
- `DetailEditBehavior`
- `DetailValidationBehavior`
- `DetailSecondaryRailBehavior`

What to move where:
- lock/edit orchestration -> detail behavior pack
- detail hydration phases -> progressive readiness behavior
- optional pane logic -> secondary behavior module

Status:
- `[pending]`

### 2.4 [app/service/features/search/runtime/SearchViewportRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchViewportRuntime.js)

Problem:
- viewport and readiness logic are likely mixed

Target split:
- `SearchLayoutBehavior`
- `SearchReadinessBehavior`

Status:
- `[pending]`

### 2.5 [app/service/features/search/runtime/SearchSelectionRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchSelectionRuntime.js)

Problem:
- selection rules are reusable behavior and should not live as one-off support runtime

Target split:
- `ListSelectionBehavior`
- `SelectionEffectsBehavior`

Status:
- `[pending]`

### 2.6 [app/controller/detail/DetailChecklistBehavior.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailChecklistBehavior.js)

Problem:
- core detail interaction and secondary rendering likely share one file

Target split:
- `DetailChecklistInteractionBehavior`
- `DetailChecklistPresentationBehavior`

Status:
- `[pending]`

## 3. Framework Overgrowth To Reduce

### 3.1 [app/service/framework/ComponentInitRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitRuntime.js)

Current role:
- large startup orchestrator

Target role:
- startup band coordinator only

What to move out:
- capability-specific startup logic -> shared startup use cases
- readiness updates -> readiness contracts and readiness service
- optional enrichments -> deferred/background startup modules

Status:
- `[pending]`

### 3.2 [app/service/framework/ComponentCoordinatorRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentCoordinatorRuntime.js)

Target:
- only coordinate generic boot sequencing
- no hidden domain branching

Status:
- `[pending]`

### 3.3 [app/service/framework/UiBehaviorPolicy.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/UiBehaviorPolicy.js)

Target:
- remain generic policy layer
- do not let page-specific rules accumulate here

Status:
- `[pending]`

## 4. Behavior-First Reuse Model

### Existing good base to preserve

Preserve and strengthen:
- [BehaviorRegistry.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/behavior/BehaviorRegistry.js)
- [BehaviorResolver.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/behavior/BehaviorResolver.js)
- default and override handler modules

### Missing reusable behavior packs to add

Must add or formalize:
- selection behavior pack
- edit-lock behavior pack
- validation-summary behavior pack
- attachment behavior pack
- readiness behavior pack
- search result interaction behavior pack
- dialog lifecycle behavior pack

Status:
- `[pending]`

## 5. Adapter And Contract Normalization

### 5.1 [app/infra/adapters/ODataChecklistRepoAdapter.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/ODataChecklistRepoAdapter.js)

Problem:
- transport adaptation risks becoming a logic hub

Target:
- one external boundary mapper
- no UI state mutation
- no business branching beyond transport adaptation

Actions:
- split OData request/response mapping by capability:
  - search mapper
  - detail mapper
  - attachment mapper
  - analytics mapper

Status:
- `[pending]`

### 5.2 [app/service/domain/analytics/AnalyticsPayloadNormalizer.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/analytics/AnalyticsPayloadNormalizer.js)

Target:
- remain only analytics domain normalization

Actions:
- remove duplicate analytics shaping from controller actions

Status:
- `[pending]`

## 6. UX/UI Ownership Cleanup

### 6.1 [app/controls/AppShellHeader.js](/C:/Users/lgbtb/Desktop/ui5/app/controls/AppShellHeader.js)

Target:
- isolated product shell module
- no app-wide logic leakage

Actions:
- keep control focused on rendering and event emission
- move action decision logic to shell behavior modules

Status:
- `[pending]`

### 6.2 [app/service/framework/ThemeService.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ThemeService.js)

Target:
- split into:
  - theme preference constants
  - theme profile persistence
  - theme application
  - optional transition effects

Status:
- `[pending]`

## 7. Performance And Progressive Readiness

Target:
- segmented loading and independent screen readiness

Must introduce:
- readiness contracts
- capability-specific busy states
- deferred analytics
- deferred secondary rails
- on-demand dialogs

Primary implementation files:
- [ComponentInitRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitRuntime.js)
- [SearchControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/search/SearchControllerBehavior.js)
- [DetailViewRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailViewBehavior.js)
- [AnalyticsControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/analytics/AnalyticsControllerBehavior.js)

Status:
- `[pending]`

## Immediate Execution Plan

### Step 1. Normalize constants and token ownership

Implementation order:
1. introduce canonical model/token contracts
2. migrate highest-risk support modules from repeated model literals to constants
3. mark deprecated path authorities

Completion marker:
- `[done]`

### Step 2. Consolidate state authority

Implementation order:
1. route domain consumers through `ModelPathContracts`
2. deprecate `DomainStatePaths`
3. remove duplicate path references

Completion marker:
- `[pending]`

### Step 3. Start decomposing thick support files

Implementation order:
1. split search support
2. split detail support
3. split analytics support

Completion marker:
- `[pending]`

## Progress Log

- 2026-03-12 shell decomposition wave started
  - extracted `ShellLayoutRuntime` to `app/service/features/shell/runtime/ShellLayoutRuntime.js`
  - extracted `ShellViewportRuntime` to `app/service/features/shell/runtime/ShellViewportRuntime.js`
  - extracted `ShellStateRuntime` to `app/service/features/shell/runtime/ShellStateRuntime.js`
  - reduced `AppControllerLifecycleActions.js` to a controller-facing facade over shell feature runtime
  - reduced `AppControllerStateRuntimeActions.js` to a controller-facing facade over shell feature runtime
  - normalized `layout` into canonical `ModelContracts.MODELS.LAYOUT`
  - normalized remaining `appView` shell reads through `ModelContracts`
  - physically drained shell controller facades into:
    - `app/controller/app/AppLifecycleBehavior.js`
    - `app/controller/app/AppStateBehavior.js`
    - `app/controller/app/AppShellBehavior.js`
- 2026-03-12 detail behavior wave started
  - extracted reusable row/info-card behavior to `app/service/features/detail/runtime/DetailRowBehaviorRuntime.js`
  - reduced `DetailChecklistRowActions.js` to a controller-facing facade over detail row behavior
  - normalized detail operation intents through `OperationSourceContracts.DETAIL`
  - normalized `state` and `selected` model usage in `DetailChecklistRuntime.js`
  - extracted detail matched/open/layout/state patching to `app/service/features/detail/runtime/DetailMatchedRuntime.js`
  - centralized detail info-card text tokens and defaults in `app/service/contracts/DetailRuntimeContracts.js`
  - centralized person-input targets and paths in `app/service/contracts/DetailPersonContracts.js`
  - normalized `selected/state/view` model usage in person and validation support
  - extracted attachments behavior to `app/service/features/detail/runtime/DetailAttachmentRuntime.js`
  - extracted location/person value-help behavior to `app/service/features/detail/runtime/DetailValueHelpRuntime.js`
  - centralized detail autosave/value-help field tokens in `app/service/contracts/DetailFieldContracts.js`
  - physically drained `DetailChecklistRowActions.js` into `app/controller/detail/DetailChecklistRowBehavior.js`
  - physically drained `DetailAttachmentLocationActions.js` into `app/controller/detail/DetailAttachmentLocationBehavior.js`
  - extracted analytics edit restore flow to `app/service/features/detail/runtime/DetailEditRestoreRuntime.js`
  - extracted selected-field and row-context helpers to `app/service/features/detail/runtime/DetailSelectedFieldRuntime.js`
  - extracted search startup/context orchestration to `app/service/features/search/runtime/SearchStartupRuntime.js`

- `2026-03-12`: map created
- `2026-03-12`: central constant rule added to architecture plan
- `2026-03-12`: added [ModelContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/contracts/ModelContracts.js) for canonical model names and shared tokens
- `2026-03-12`: migrated first-wave heavy modules to use centralized model/token contracts:
  - [SearchControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/search/SearchControllerBehavior.js)
  - [AnalyticsControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/analytics/AnalyticsControllerBehavior.js)
  - [DetailViewRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailViewBehavior.js)
- `2026-03-12`: marked [DomainStatePaths.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/shared/ModelPathContracts.js) as deprecated compatibility surface
- `2026-03-12`: moved `AnalyticsContracts`, `NavigationContracts`, and `WorkflowContracts` to neutral canonical files under [app/contracts](/C:/Users/lgbtb/Desktop/ui5/app/contracts/AnalyticsContracts.js)
- `2026-03-12`: converted service and infra contract copies into alias-only entry points
- `2026-03-12`: removed literal duplication from [DomainStatePaths.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/shared/ModelPathContracts.js) by aliasing canonical state contracts
- `2026-03-12`: added [target-folder-map.md](/C:/Users/lgbtb/Desktop/ui5/docs/audit/target-folder-map.md) to formalize capability-based target structure and folder merge/delete strategy
- `2026-03-12`: added [OperationSourceContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/contracts/OperationSourceContracts.js) for canonical action/source labels
- `2026-03-12`: migrated shared model-name usage in:
  - [ControllerModelRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ControllerModelRuntime.js)
  - [ComponentInitRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitRuntime.js)
  - [EffectApplier.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectApplier.js)
- `2026-03-12`: migrated shell/runtime model keys in:
  - [AppControllerDomActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/app/AppDomBehavior.js)
  - [AppControllerShellActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/app/AppShellBehavior.js)
  - [AppControllerLifecycleActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/app/AppLifecycleBehavior.js)
- `2026-03-12`: fully drained local `source: "..."` labels from [SearchControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/search/SearchControllerBehavior.js)
- `2026-03-12`: created [search-view-runtime-decomposition.md](/C:/Users/lgbtb/Desktop/ui5/docs/audit/search-view-runtime-decomposition.md) with target cut map for `SearchViewRuntime`
- `2026-03-12`: completed first real `SearchViewRuntime` decomposition slice by extracting analytics rail scheduling into [SearchAnalyticsRailRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchAnalyticsRailRuntime.js)
- `2026-03-12`: established clean controller-to-capability boundary for search analytics runtime via injected runner, instead of direct controller-layer imports
- `2026-03-12`: extracted search loading feedback and pending-load settlement into [SearchLoadingFeedbackRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchLoadingFeedbackRuntime.js)
- `2026-03-12`: extracted smart table initialize and before-rebind orchestration into [SearchSmartTableRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchSmartTableRuntime.js)
- `2026-03-12`: extracted export and analytics navigation actions into [SearchActionRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchActionRuntime.js)
- `2026-03-12`: `SearchViewRuntime` is now a thinner orchestration facade over capability runtimes instead of a single mixed-responsibility module
## Completed Wave: Search And Detail Facade Drain

- `SearchControllerActions` physically drained into `controller/search/SearchControllerBehavior.js`.
- `SearchViewRuntime` physically drained into `controller/search/SearchViewBehavior.js` after runtime decomposition.
- `DetailChecklistStateActions` physically drained into `controller/detail/DetailChecklistStateBehavior.js`.
- `DetailViewBehavior` reduced to a thin facade over `DetailLayoutRuntime` and `DetailInfoCardFactory`.
- Extracted `DetailObserverCardRuntime` and `DetailSimpleCardRuntime` to standardize repeated info-card behavior.
- Added `SearchRuntimeContracts.js` as the canonical source for search defaults, persistency prefixes, analytics source token, and search mode tokens.
- `SearchViewStateRuntime.js` now consumes canonical search runtime contracts instead of owning raw persistency and analytics source literals.
- Controller entry points now bind directly to capability-based search/detail behaviors instead of legacy support facades.
- `2026-03-12`: physically drained the remaining feature-specific `controller/support` modules into target capability folders:
  - `app/controller/analytics/AnalyticsBuilderRuntime.js`
  - `app/controller/detail/DetailAccessViewState.js`
  - `app/controller/detail/DetailActionConstants.js`
  - `app/controller/detail/DetailActionDialogRuntime.js`
  - `app/controller/detail/DetailActionPinnedRailRuntime.js`
  - `app/controller/detail/DetailCommandPolicy.js`
  - `app/controller/detail/DetailFormatters.js`
  - `app/controller/detail/DetailInfoCardLayoutRuntime.js`
  - `app/controller/search/SearchCommandPolicy.js`
  - `app/controller/search/SearchLoadRuntime.js`
  - `app/controller/search/SearchRateProgress.js`
  - `app/controller/search/SearchSelectionRuntime.js`
  - `app/controller/search/SearchShortcutRuntime.js`
  - `app/controller/search/SearchViewportRuntime.js`
  - `app/controller/search/SearchViewStateRuntime.js`
- `2026-03-12`: moved controller-neutral helpers into `app/controller/shared`:
  - `app/controller/shared/ControllerResourceCleanup.js`
  - `app/controller/shared/ControllerReturnFocusRuntime.js`
- `2026-03-12`: `app/controller/support` is fully drained and no longer acts as a live architectural layer.
