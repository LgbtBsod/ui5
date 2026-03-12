# Target Folder Map

Date: 2026-03-12
Scope: `app/`
Goal: normalize the frontend structure into a capability-based modular layout with SRP, smart contracts, reusable behaviors, and minimal historical layering noise

## Target Structure

### Canonical shared layers

- `app/contracts`
  - canonical shared contracts only
  - examples: workflow, navigation, analytics
- `app/model`
  - canonical UI state paths and model schemas only
- `app/behaviors`
  - reusable default and override behavior packs
- `app/framework`
  - generic infrastructure only
- `app/adapters`
  - external transport and integration adapters

### Capability layers

- `app/features/search`
  - search controllers, view behaviors, use cases, state access, formatters
- `app/features/detail`
  - detail flows, edit/lock behavior, attachment behavior, validation behavior
- `app/features/analytics`
  - analytics load, refresh, export, drilldown behavior
- `app/features/shell`
  - shell state sync, shell overlays, shell actions
- `app/features/shared`
  - shared capability helpers that are not generic enough for framework

### UI layers

- `app/views`
  - XML views and fragments
- `app/controls`
  - only real custom controls
- `app/styles`
  - component- and capability-scoped styles

## Current To Target Map

### Keep, but normalize purpose

- `app/contracts`
  - keep
  - becomes the only shared business contract source
- `app/model`
  - keep
  - remains canonical state path/schema layer

### Merge and move

- `app/service/framework/behavior` -> `app/behaviors`
  - reason: this is behavior infrastructure, not service/domain logic
- `app/infra/adapters` -> `app/adapters`
  - reason: adapters are already a clean concept and should be first-class
- `app/controller/support` -> split across `app/features/*`
  - reason: support folder is a historical catch-all and not a canonical architecture concept
- `app/view` -> `app/views`
  - reason: pluralized UI layer naming and clearer separation
- `app/control` -> `app/controls`
  - reason: same normalization as views
- `app/css` -> `app/styles`
  - reason: styling should align with component/capability ownership, not raw asset grouping

### Shrink or eliminate

- `app/service/contracts`
  - keep temporarily as controller-safe facade only
  - long-term target: remove once controller-layer rules allow direct canonical import or controllers move behind feature/application layer
- `app/service/domain/shared`
  - shrink heavily
  - split true shared contracts vs feature-specific helpers
- `app/util`
  - reduce aggressively
  - move generic runtime infra to framework
  - move feature-specific helpers into matching capability folders
- `app/ports`
  - review whether it is still a live abstraction or can be merged into capability/domain folders
- `app/localService`
  - keep only as local metadata/mock contract fixture layer

## Folder-Level Decisions

### Delete after migration

- `app/infra/contracts`
  - already removed
  - duplicate layer no longer needed

### Deprecate and drain

- `app/controller/support`
  - target state: empty then removed
- `app/service/framework`
  - target state: smaller and generic-only
- `app/util`
  - target state: minimal

## First Migration Waves

### Wave 1

- keep physical structure mostly stable
- introduce canonical contracts and token constants
- stop adding new files to `controller/support`
- stop adding new duplicated literals

### Wave 2

- move search-specific support modules into `features/search`
- move detail-specific support modules into `features/detail`
- move analytics-specific support modules into `features/analytics`
- move shell-specific support modules into `features/shell`

### Wave 3

- move reusable behavior modules into `behaviors`
- move adapters into `adapters`
- trim `service/framework` to truly generic services only

### Wave 4

- rename UI folders:
  - `view` -> `views`
  - `control` -> `controls`
  - `css` -> `styles`

## Immediate Candidate Moves

### Search

- `controller/support/SearchControllerActions.js` -> `features/search/behavior/SearchControllerActions.js` then split further
- `controller/support/SearchSelectionRuntime.js` -> `features/search/behavior/SearchSelectionBehavior.js`
- `controller/support/SearchViewportRuntime.js` -> `features/search/layout/SearchViewportBehavior.js`
- `controller/support/SearchViewRuntime.js` -> `features/search/runtime/SearchViewRuntime.js`

### Detail

- `controller/support/DetailViewRuntime.js` -> `features/detail/runtime/DetailViewRuntime.js` then split
- `controller/support/DetailChecklistRuntime.js` -> `features/detail/behavior/DetailChecklistBehavior.js`
- `controller/support/DetailChecklistRowActions.js` -> `features/detail/behavior/DetailRowBehavior.js`
- `controller/support/AttachmentUploadCore.js` -> `features/detail/attachments/AttachmentUploadBehavior.js`

### Analytics

- `controller/support/AnalyticsControllerActions.js` -> `features/analytics/behavior/AnalyticsControllerActions.js`
- `controller/support/AnalyticsBuilderRuntime.js` -> `features/analytics/runtime/AnalyticsBuilderRuntime.js`

### Shell

- `controller/support/AppControllerLifecycleActions.js` -> `features/shell/runtime/AppLifecycleRuntime.js`
- `controller/support/AppControllerShellActions.js` -> `features/shell/behavior/ShellActionBehavior.js`
- `controller/support/AppControllerStateRuntimeActions.js` -> `features/shell/state/ShellStateRuntime.js`

## Rules During Migration

- no new shared literals outside canonical contracts/constants
- no new files in deleted/deprecated folders unless they are pure temporary compatibility shims
- move by capability, not by old layer name
- delete dead alias layers as soon as imports are drained
