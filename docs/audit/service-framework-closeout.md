# Service Framework Closeout

Date: 2026-03-12
Scope: `app/service/framework`
Goal: finalize the target architecture of the frontend framework layer after the structural refactor and define the rules that keep it stable

## Target State

The `service/framework` layer is now governed by one model only:

- real runtime modules
- canonical contract modules
- low-level orchestration helpers with explicit responsibility
- no alias-only files
- no duplicated token ownership

This layer is no longer allowed to behave like a historical catch-all.

It must not:

- proxy other modules without adding architectural value
- own feature-specific behavior that belongs to `service/features/*`
- redefine state, feedback, listener, or save-guard literals locally
- accumulate controller-only behavior

## Architectural Outcome

### What changed

- `ComponentInitRuntime` was reduced to a real init coordinator instead of a mixed startup blob.
- attachment bootstrapping was split into:
  - [ComponentAttachmentContextRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentAttachmentContextRuntime.js)
  - [ComponentRuntimeAttachOrchestrator.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentRuntimeAttachOrchestrator.js)
- listener logic was split into:
  - [ComponentDetailMetaSyncRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentDetailMetaSyncRuntime.js)
  - [ComponentListenerBindingRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentListenerBindingRuntime.js)
  - [ComponentListenerBootstrapRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentListenerBootstrapRuntime.js)
- save-guard logic was split into:
  - [ComponentSaveGuardRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentSaveGuardRuntime.js)
  - [ComponentSaveGuardPolicy.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentSaveGuardPolicy.js)
- feedback handling was split into:
  - [EffectToastRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectToastRuntime.js)
  - [EffectDialogFeedbackRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectDialogFeedbackRuntime.js)
  - [EffectFeedbackRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectFeedbackRuntime.js)

### What was removed

The following files were intentionally removed because they were alias-only or had no boundary value:

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

## Current Module Map

### 1. Entry points

- [ComponentInitRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitRuntime.js)
  - public entry: `runInit`
  - responsibility: coordinate init order and attach already-sliced runtime stages
- [ComponentBootRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentBootRuntime.js)
  - public entry: `runBootSequence`
  - responsibility: boot success/error sequencing and boot completion rules
- [EffectApplier.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectApplier.js)
  - public entry: `applyEffects`
  - responsibility: dispatch effects to canonical feedback/model handlers
- [FacadeCommandRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/FacadeCommandRuntime.js)
  - public entries: `execute*`
  - responsibility: facade command execution against normalized context and payload builders

### 2. Init composition slices

- [ComponentInitStageRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitStageRuntime.js)
  - model stage and core stage composition
- [ComponentInitCompositionRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitCompositionRuntime.js)
  - feedback, runtime settings, and pending navigation composition
- [ComponentAttachmentContextRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentAttachmentContextRuntime.js)
  - force-readonly, guarded-save, cross-tab, default handlers
- [ComponentRuntimeAttachOrchestrator.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentRuntimeAttachOrchestrator.js)
  - manager, lock, and listener attachment orchestration
- [ComponentModelBootstrapRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentModelBootstrapRuntime.js)
  - model registration
- [ComponentMainServiceRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentMainServiceRuntime.js)
  - main OData service setup
- [ComponentCoreBootstrapRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentCoreBootstrapRuntime.js)
  - ctx/facade/action-dispatch bootstrap
- [ComponentStateSeedRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentStateSeedRuntime.js)
  - initial runtime state seeding

### 3. Boot and manager runtime

- [ComponentBootContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentBootContracts.js)
- [ComponentBootStateRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentBootStateRuntime.js)
- [ComponentBootStageRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentBootStageRuntime.js)
- [ComponentBootStageExecutionRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentBootStageExecutionRuntime.js)
- [ComponentManagerOrchestrationRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentManagerOrchestrationRuntime.js)
- [ComponentPollingRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentPollingRuntime.js)
- [ComponentAutosaveRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentAutosaveRuntime.js)
- [ComponentLockEventsRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentLockEventsRuntime.js)

### 4. Listener and detail-meta runtime

- [ComponentListenerContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentListenerContracts.js)
- [ComponentDetailMetaContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentDetailMetaContracts.js)
- [ComponentDetailMetaSyncRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentDetailMetaSyncRuntime.js)
- [ComponentListenerBindingRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentListenerBindingRuntime.js)
- [ComponentListenerBootstrapRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentListenerBootstrapRuntime.js)
- [ComponentInitListenersRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitListenersRuntime.js)
- [ComponentNavigationGuardRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentNavigationGuardRuntime.js)

### 5. Feedback and effects

- [EffectFeedbackContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectFeedbackContracts.js)
- [EffectToastRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectToastRuntime.js)
- [EffectDialogFeedbackRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectDialogFeedbackRuntime.js)
- [EffectBannerRouter.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectBannerRouter.js)
- [EffectDialogRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectDialogRuntime.js)
- [EffectFeedbackRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectFeedbackRuntime.js)
- [EffectModelRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectModelRuntime.js)
- [EffectApplier.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectApplier.js)

### 6. Save guard and pending navigation

- [ComponentSaveGuardContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentSaveGuardContracts.js)
- [ComponentSaveGuardPolicy.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentSaveGuardPolicy.js)
- [ComponentSaveGuardRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentSaveGuardRuntime.js)
- [ComponentPendingNavigationRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentPendingNavigationRuntime.js)
- [ComponentFeedbackBootstrapRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentFeedbackBootstrapRuntime.js)
- [ComponentFeedbackRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentFeedbackRuntime.js)
- [ComponentRuntimeSettingsBootstrap.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentRuntimeSettingsBootstrap.js)

### 7. Shared low-level primitives

- [ComponentSessionRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentSessionRuntime.js)
- [ComponentFormattingRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentFormattingRuntime.js)
- [ComponentDetailStateRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentDetailStateRuntime.js)
- [ModelStateRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ModelStateRuntime.js)
- [SchedulingRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/SchedulingRuntime.js)
- [SecurityTokenRefresh.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/SecurityTokenRefresh.js)
- [TelemetryRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/TelemetryRuntime.js)
- [RootIdRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/RootIdRuntime.js)

## Canonical Source Rules

### Boot tokens

Canonical source:

- [ComponentBootContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentBootContracts.js)

Must live only there:

- boot readiness statuses
- boot stage error keys
- boot paths and config source markers

### Feedback tokens

Canonical source:

- [EffectFeedbackContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectFeedbackContracts.js)

Must live only there:

- toast classes
- dialog classes
- effect handler names
- fallback text keys
- durations
- dialog ids and variants

### Listener/detail-meta tokens

Canonical sources:

- [ComponentListenerContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentListenerContracts.js)
- [ComponentDetailMetaContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentDetailMetaContracts.js)

Must live only there:

- lifecycle event names
- full-save signal name
- listener model names
- listener path aliases
- readiness/detail-meta statuses
- validation defaults

### Save-guard tokens

Canonical source:

- [ComponentSaveGuardContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentSaveGuardContracts.js)

Must live only there:

- save-guard telemetry names
- banner level and text-key defaults
- busy/save state markers
- login/network path markers
- timer durations

## Boundary Rules

### Rule 1: `service/framework` is not a feature layer

Feature behavior belongs in:

- `app/service/features/*`
- `app/controller/*`
- domain use cases

Framework is allowed to:

- coordinate generic startup
- coordinate generic effects
- coordinate generic state/runtime listeners
- provide low-level runtime primitives

Framework is not allowed to:

- implement search/detail/analytics-specific business rules
- own view-specific branching
- embed feature-specific fallback text or labels

### Rule 2: One file, one dominant responsibility

A file may coordinate several calls only if they belong to one orchestration surface.

Valid examples:

- `ComponentInitRuntime` coordinates init order
- `ComponentBootRuntime` coordinates boot stages
- `EffectApplier` coordinates effect application

Invalid examples:

- one file that assembles attachment context, guarded save, lock attach, and listener attach
- one file that handles toast, dialog, banner, prompt, and navigation primitives plus token ownership

### Rule 3: No alias-only files

A file in `service/framework` must not exist if it only:

- imports 1-2 modules
- returns their members unchanged
- adds no stable boundary or orchestration responsibility

If a file becomes that thin, delete it and move callers to the canonical runtime.

### Rule 4: Entry points stay stable

The following public seams are stable and must not be renamed casually:

- `ComponentInitRuntime.runInit`
- `ComponentBootRuntime.runBootSequence`
- `EffectApplier.applyEffects`
- `FacadeCommandRuntime.execute*`

Refactor behind these seams, not through arbitrary churn at the seam.

### Rule 5: New literals are banned unless truly local

If a string or token is:

- reused
- semantically important
- part of runtime policy
- part of telemetry vocabulary
- part of feedback or boot semantics

then it must be added to a canonical contract file.

Do not introduce new duplicated literals in runtime modules.

## Allowed Extension Pattern

When new behavior is needed:

1. decide whether it is feature behavior or framework behavior
2. if feature behavior, place it outside `service/framework`
3. if framework behavior, decide whether it is:
   - contract/token
   - low-level primitive
   - orchestration coordinator
4. create or extend exactly one canonical source
5. wire through stable entry points without creating alias layers

## Review Checklist For Future Changes

Before merging any new `service/framework` change, verify:

- the file adds real behavior or orchestration
- no existing canonical contract already owns the token
- no feature-specific behavior leaked into framework
- no alias-only wrapper was introduced
- no second source of truth was created
- the stable public entry points remain intact

## Validation Baseline

The final refactor was validated with:

- `node scripts/architecture-gate.js --json`
- `python -m pytest backend/mock_gateway/tests -q`

Expected result at closeout:

- architecture gate passes
- backend/mock gateway tests pass
- alias-only framework files remain absent

## Final Assessment

`service/framework` is now materially closer to the intended architecture:

- modular
- SRP-oriented
- contract-driven
- no dead re-export surface
- canonical token ownership

The next engineering work in this layer should be incremental and policy-driven, not another rescue refactor.
