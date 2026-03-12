# Service Framework Closeout

Date: 2026-03-12
Scope: `app/service/framework`
Goal: document the actual current framework structure after the refactor and remove stale references to deleted historical wrappers

## Target State

`service/framework` is governed by one active model:

- real runtime modules
- canonical framework contracts
- explicit initialization and steady-state orchestration stages
- no alias-only wrappers
- no historical `Support` naming
- no non-technical `Bootstrap` owners

This layer must stay:

- generic
- SRP-oriented
- contract-driven
- feature-neutral

## 1. Actual Outcome

### Removed historical or alias-only files

- `ComponentLockRuntime.js`
- `ComponentCoordinatorRuntime.js`
- `ComponentLifecycleRuntime.js`
- `ComponentRuntimeAttachmentBootstrap.js`
- `ComponentListenerRuntime.js`
- `ComponentListenerStateRuntime.js`
- `ComponentInitAttachmentStageRuntime.js`
- `ComponentRuntimeSupport.js`
- `ComponentInitSaveGuardSupport.js`
- `FrontendConfigConstants.js`
- `ThemeRuntime.js`

### Renamed into actual runtime or init roles

- `ComponentLockReleaseSupport` -> [ComponentLockReleaseRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentLockReleaseRuntime.js)
- `ComponentCoreBootstrapRuntime` -> [ComponentCoreInitRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentCoreInitRuntime.js)
- `ComponentFeedbackBootstrapRuntime` -> [ComponentFeedbackInitRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentFeedbackInitRuntime.js)
- `ComponentListenerBootstrapRuntime` -> [ComponentListenerInitRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentListenerInitRuntime.js)
- `ComponentModelBootstrapRuntime` -> [ComponentModelInitRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentModelInitRuntime.js)
- `ComponentRuntimeHandlerBootstrap` -> [ComponentRuntimeHandlerRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentRuntimeHandlerRuntime.js)
- `ComponentRuntimeSettingsBootstrap` -> [ComponentRuntimeSettingsRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentRuntimeSettingsRuntime.js)

## 2. Current Module Map

### Stable framework entry points

- [ComponentInitRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitRuntime.js)
  - `runInit`
- [ComponentBootRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentBootRuntime.js)
  - `runBootSequence`
- [EffectApplier.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectApplier.js)
  - `applyEffects`
- [FacadeCommandRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/FacadeCommandRuntime.js)
  - `execute*`

### Init composition and attachment runtime

- [ComponentInitStageRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitStageRuntime.js)
- [ComponentInitCompositionRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitCompositionRuntime.js)
- [ComponentAttachmentContextRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentAttachmentContextRuntime.js)
- [ComponentRuntimeAttachOrchestrator.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentRuntimeAttachOrchestrator.js)
- [ComponentModelInitRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentModelInitRuntime.js)
- [ComponentMainServiceRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentMainServiceRuntime.js)
- [ComponentCoreInitRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentCoreInitRuntime.js)
- [ComponentStateSeedRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentStateSeedRuntime.js)
- [ComponentRuntimeHandlerRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentRuntimeHandlerRuntime.js)
- [ComponentRuntimeSettingsRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentRuntimeSettingsRuntime.js)

### Boot, manager and lock runtime

- [ComponentBootContracts.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentBootContracts.js)
- [ComponentBootStateRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentBootStateRuntime.js)
- [ComponentBootStageRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentBootStageRuntime.js)
- [ComponentBootStageExecutionRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentBootStageExecutionRuntime.js)
- [ComponentManagerOrchestrationRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentManagerOrchestrationRuntime.js)
- [ComponentPollingRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentPollingRuntime.js)
- [ComponentAutosaveRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentAutosaveRuntime.js)
- [ComponentLockEventsRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentLockEventsRuntime.js)
- [ComponentLockReleaseRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentLockReleaseRuntime.js)
- [ComponentCrossTabRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentCrossTabRuntime.js)

### Listener, meta and navigation runtime

- [ComponentListenerContracts.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentListenerContracts.js)
- [ComponentDetailMetaContracts.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentDetailMetaContracts.js)
- [ComponentDetailMetaSyncRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentDetailMetaSyncRuntime.js)
- [ComponentListenerBindingRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentListenerBindingRuntime.js)
- [ComponentListenerInitRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentListenerInitRuntime.js)
- [ComponentInitListenersRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitListenersRuntime.js)
- [ComponentNavigationGuardRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentNavigationGuardRuntime.js)

### Feedback, save-guard and effect runtime

- [EffectFeedbackContracts.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectFeedbackContracts.js)
- [EffectToastRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectToastRuntime.js)
- [EffectDialogFeedbackRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectDialogFeedbackRuntime.js)
- [EffectBannerRouter.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectBannerRouter.js)
- [EffectDialogRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectDialogRuntime.js)
- [EffectFeedbackRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectFeedbackRuntime.js)
- [EffectModelRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectModelRuntime.js)
- [ComponentSaveGuardContracts.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentSaveGuardContracts.js)
- [ComponentSaveGuardPolicy.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentSaveGuardPolicy.js)
- [ComponentSaveGuardRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentSaveGuardRuntime.js)
- [ComponentPendingNavigationRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentPendingNavigationRuntime.js)
- [ComponentFeedbackInitRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentFeedbackInitRuntime.js)
- [ComponentFeedbackRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentFeedbackRuntime.js)
- [FeedbackBannerRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/FeedbackBannerRuntime.js)
- [FeedbackBannerState.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/FeedbackBannerState.js)
- [FeedbackCoordinator.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/FeedbackCoordinator.js)
- [FeedbackPolicy.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/FeedbackPolicy.js)

### Shared framework primitives

- [ComponentSessionRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentSessionRuntime.js)
- [ComponentFormattingRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentFormattingRuntime.js)
- [ComponentDetailStateRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentDetailStateRuntime.js)
- [ModelStateRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ModelStateRuntime.js)
- [SchedulingRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/SchedulingRuntime.js)
- [SecurityTokenRefresh.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/SecurityTokenRefresh.js)
- [TelemetryRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/TelemetryRuntime.js)
- [MemoryTelemetryBuffer.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/MemoryTelemetryBuffer.js)
- [UxTelemetry.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/UxTelemetry.js)
- [RootIdRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/RootIdRuntime.js)
- [ThemeService.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ThemeService.js)
- [ThemeDomRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ThemeDomRuntime.js)
- [ThemeTokenRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ThemeTokenRuntime.js)
- [ThemePhilosophy.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ThemePhilosophy.js)

## 3. Current Rules

### Rule 1

`service/framework` is not a feature layer.

Feature behavior belongs in:

- `app/service/features/*`
- `app/service/domain/*`
- `app/controller/*`

### Rule 2

No new `Support` modules are allowed in framework.

Use the actual role instead:

- `Runtime`
- `Policy`
- `Contracts`
- `Coordinator`
- `Reader`

### Rule 3

No new non-technical `Bootstrap` names are allowed in framework.

Use:

- `Init` for initialization stages
- `Runtime` for steady-state runtime modules

### Rule 4

No alias-only wrappers are allowed.

If a file only proxies another runtime and does not stabilize a real boundary, delete it and import the canonical owner directly.

### Rule 5

Framework literals and runtime semantics must live in canonical contracts:

- [ComponentBootContracts.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentBootContracts.js)
- [EffectFeedbackContracts.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectFeedbackContracts.js)
- [ComponentListenerContracts.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentListenerContracts.js)
- [ComponentDetailMetaContracts.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentDetailMetaContracts.js)
- [ComponentSaveGuardContracts.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentSaveGuardContracts.js)

## 4. Validation Baseline

Current closeout baseline:

- `python -m pytest backend/mock_gateway/tests -q`
- `node scripts/framework-token-drift-gate.js --json`
- `node scripts/adapter-factory-boundary-gate.js --json`

Expected closeout state:

- tests pass
- framework token drift gate passes
- adapter factory boundary gate passes
- no alias-only framework files return

## 5. Final Position

`service/framework` is now a real generic runtime layer, not a historical dumping ground.

The remaining work in this layer is governance and incremental cleanup, not another structural rescue. The main risk is regression, not missing architectural decomposition.
