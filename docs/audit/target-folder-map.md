# Target Folder Map

Date: 2026-03-12
Scope: `app/`
Goal: document the actual current structural map, the completed migrations, and the remaining physical normalization work

## 1. Actual Current Structure

### Canonical active layers

- `app/contracts`
  - cross-feature business contracts
- `app/model`
  - canonical state paths and model schemas
- `app/controller`
  - entry points, controller-local behaviors, controller-shared helpers
- `app/service/domain`
  - business use cases and domain orchestration
- `app/service/features`
  - feature runtimes and feature contracts
- `app/service/framework`
  - generic runtime infrastructure and framework contracts
- `app/service/shared`
  - cross-feature shared helpers
- `app/infra/adapters`
  - transport, platform and integration seams

### Additional active normalized layers

- `app/controls`
  - custom UI5 controls
- `app/views`
  - XML views and fragments
- `app/styles`
  - application stylesheet entry and style modules
- `app/service/ports`
  - interface-style boundary contracts

### Remaining top-level folders outside the core ownership model

- `app/assets`
- `app/i18n`
- `app/localService`

These are part of the physical tree, but they are not where the main runtime architecture is now centered.

## 2. Completed Normalization

### Completed layer drainage

- `app/controller/support`
  - removed
- `app/util`
  - removed
- `app/infra/contracts`
  - removed as duplicate layer

### Completed top-level naming normalization

- `app/control` -> `app/controls`
- `app/view` -> `app/views`
- `app/css` -> `app/styles`
- `app/ports` -> `app/service/ports`

### Completed ownership moves

- shared helpers moved into `app/service/shared`
- feature contracts and runtime moved into `app/service/features/*`
- business-side orchestration moved into `app/service/domain/*`
- framework runtime consolidated into `app/service/framework`
- adapter transport seams normalized under `app/infra/adapters`

### Completed factory cleanup

Collapsed stateless adapters:

- [ClockAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/ClockAdapter.js)
- [LockAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/LockAdapter.js)
- [LastChangeSetAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/LastChangeSetAdapter.js)
- [LocationLookupAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/LocationLookupAdapter.js)
- [PersonSuggestAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/PersonSuggestAdapter.js)
- [WorkflowAnalyticsAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/WorkflowAnalyticsAdapter.js)

Retained stateful factories:

- [BrowserCacheAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/BrowserCacheAdapter.js)
- [DictAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/DictAdapter.js)
- [ODataChecklistRepoAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/ODataChecklistRepoAdapter.js)
- [SmartControlsAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/SmartControlsAdapter.js)
- [TelemetryAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/TelemetryAdapter.js)
- [Ui5StateAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/Ui5StateAdapter.js)

## 3. Ownership Rules By Folder

### `app/controller`

Allowed:

- event entry points
- route handlers
- controller-local runtime and behavior
- controller-shared helpers in `controller/shared`

Forbidden:

- backend transport logic
- cross-feature dumping grounds

### `app/service/features/*`

Allowed:

- feature runtime
- feature contracts
- feature-local reusable behavior

Forbidden:

- generic framework primitives
- backend transport integration

### `app/service/domain/*`

Allowed:

- business use cases
- business normalization
- domain-side orchestration

Forbidden:

- DOM logic
- transport implementation details

### `app/service/framework`

Allowed:

- generic runtime stages
- generic framework policies
- framework contracts
- scheduling, feedback, session, telemetry, navigation and effect infrastructure

Forbidden:

- feature business rules
- alias-only wrapper files

### `app/service/shared`

Allowed:

- cross-feature readers
- identity, clone, id and delta utilities
- shared non-framework helpers

Forbidden:

- feature orchestration
- framework-only runtime primitives

### `app/infra/adapters`

Allowed:

- integration boundaries
- transport mapping
- OData/platform normalization

Forbidden:

- controller logic
- feature business orchestration

### `app/service/ports`

Allowed:

- interface-style boundary contracts
- service-facing port definitions for adapters and domain seams

Forbidden:

- runtime logic
- transport implementation
- feature orchestration

## 4. Remaining Physical Normalization Work

### Remaining surface normalization

- keep `controls`, `views`, and `styles` as UI-surface layers only, not as new dumping grounds
- keep `service/ports` contract-only and prevent runtime drift into it

### Documentation rule

Future documents must not describe:

- `controller/support` as an active execution layer
- `util` as an active owner
- `infra/contracts` as an existing shared contract layer
- `control`, `view`, `css`, or top-level `ports` as current active owners

## 5. Current Structural Position

The core target folder model is already implemented in the active runtime path.

The remaining work is now:

- governance enforcement against regression
- keeping the new top-level map clean

The architectural migration itself is no longer hypothetical; it is the current codebase shape.
