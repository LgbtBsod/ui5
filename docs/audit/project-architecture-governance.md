# Project Architecture Governance

Date: 2026-03-12
Scope: `app/`
Goal: freeze the current target architecture into explicit rules so future development preserves `SRP`, maximum code reuse, smart contracts, and clean module ownership

## Executive Rule

All new code must follow this model:

- one dominant responsibility per module
- one canonical owner per reusable concept
- repeated behavior must be reused through shared runtime/behavior modules
- factories are allowed only where they encapsulate injected runtime dependencies
- contracts and tokens must have one source of truth

The project is no longer allowed to grow by:

- dumping helpers into `util`
- creating new `support` catch-all files
- adding alias-only wrapper modules
- keeping duplicated contract literals in feature code

## Canonical Layer Map

### `app/contracts`

Canonical owner for cross-feature business contracts.

Allowed:

- workflow contracts
- navigation contracts
- analytics contracts
- dialog contracts

Forbidden:

- feature-local runtime helpers
- UI controller logic
- transport-specific mapping

### `app/model`

Canonical owner for UI state paths and schema-like model definitions.

Allowed:

- state path ownership
- model schema ownership

Forbidden:

- business orchestration
- duplicate state constants elsewhere

### `app/controller`

Controller layer is an entry layer only.

Allowed:

- event entry points
- route hooks
- delegation to behavior/runtime/use cases

Forbidden:

- backend mapping
- long orchestration chains
- duplicated state semantics

### `app/service/features/*`

Canonical owner for feature-specific runtime, behavior, and contracts.

Allowed:

- feature orchestration
- feature-local contracts
- reusable behavior for one capability

Forbidden:

- transport-specific concerns
- generic framework primitives
- duplicated cross-feature contracts

### `app/service/domain/*`

Canonical owner for business use cases, domain normalization, and domain-side runtime helpers.

Allowed:

- use cases
- domain runtime helpers
- domain result/effect composition

Forbidden:

- controller wiring
- UI5 view dependencies
- direct DOM handling

### `app/service/framework`

Canonical owner for generic runtime primitives and orchestration stages.

Allowed:

- generic init/boot/runtime stages
- effect routing
- scheduling
- session/runtime state infrastructure
- generic feedback and guard infrastructure

Forbidden:

- feature-specific business rules
- alias-only wrappers
- duplicate token ownership

### `app/service/shared`

Canonical owner for cross-feature shared helpers that are not framework primitives.

Allowed:

- generic clone/value/id helpers
- shared delta helpers
- shared readers

Forbidden:

- feature orchestration
- UI/controller-specific behavior

### `app/infra/adapters`

Canonical owner for backend and platform integration seams.

Allowed:

- transport calls
- payload mapping
- backend-specific normalization
- stateful adapters with injected runtime dependencies

Forbidden:

- UI state logic
- feature business rules
- duplicate domain normalization

## Factory Policy

### Factory is allowed only when one of these is true

- the module needs injected models or runtime state
- the module needs injected view refs or controller refs
- the module needs injected environment or service handles
- the module must isolate a stateful boundary instance

### Factory is forbidden when all of these are true

- the module is stateless
- the module does not capture injected dependencies
- every exported method can run from direct module scope
- `create()` only returns method references or thin closures

### Allowed factories right now

- [BrowserCacheAdapter.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/BrowserCacheAdapter.js)
- [DictAdapter.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/DictAdapter.js)
- [ODataChecklistRepoAdapter.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/ODataChecklistRepoAdapter.js)
- [SmartControlsAdapter.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/SmartControlsAdapter.js)
- [TelemetryAdapter.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/TelemetryAdapter.js)
- [Ui5StateAdapter.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/Ui5StateAdapter.js)

### Forbidden examples now removed

- `ClockAdapter.create()`
- `LockAdapter.create()`
- `LastChangeSetAdapter.create()`
- `LocationLookupAdapter.create()`
- `PersonSuggestAdapter.create()`
- `WorkflowAnalyticsAdapter.create()`

## Naming Rules

### Allowed module suffixes

- `Runtime`
- `Contracts`
- `Policy`
- `Coordinator`
- `Facade`
- `UseCase`
- `Adapter`
- `Reader`
- `Factory`
- `Mixin`

### Restricted suffixes

- `Support`
  - forbidden for new code
  - rename to actual role such as `Runtime`, `Reader`, `Policy`, or `Adapter`
- `Bootstrap`
  - forbidden for new code except technical bootstrapping artifacts explicitly tied to loader/runtime startup
  - prefer `Init` for initialization stages and `Runtime` for steady-state runtime modules

### Explicit technical exception

- [ui5-bootstrap-runtime.js](/C:/Users/lgbtb/Desktop/ui5/app/ui5-bootstrap-runtime.js)
  - allowed as a technical runtime bootstrap artifact
  - do not use this as justification for new `Bootstrap` module names

## Canonical Owner Rules

### Tokens and constants

- shared business tokens: `app/contracts/*`
- feature tokens: `app/service/features/*/contracts/*`
- model path tokens: `app/model/*` and domain path bridges only where necessary
- framework runtime tokens: `app/service/framework/*Contracts.js`

If a token is reused or semantically important, it must not remain as a local literal.

### Reusable behavior

If two equivalent UI objects behave the same way, behavior must live in one reusable module.

Examples:

- attachment effects
- validation summary behavior
- lock release behavior
- search binding policies
- analytics export row shaping

### Transport mapping

Backend and OData shaping must live in adapter/runtime slices inside `infra/adapters` or adapter-shared modules.

It must not drift into:

- controllers
- feature runtime
- generic framework

## SRP and Reuse Rules

### SRP test

A module is valid only if one sentence can describe its dominant job without using `and`.

Valid:

- `DetailAuthorizationRuntime` resolves permission semantics
- `GatewayRequestRuntime` performs gateway request primitives
- `ComponentModelInitRuntime` initializes component models

Invalid:

- one module reads backend data and decides UI busy states and normalizes view tokens

### Reuse test

Before adding new logic, check whether behavior already exists in:

- feature contracts
- domain runtime
- framework runtime
- shared readers/helpers
- adapter-shared runtimes

If behavior already exists, extend or parameterize it instead of cloning it.

## Directory Rules

### Removed legacy owners

- `app/util`
- `app/controller/support`

These must not return.

### Normalized UI surface layers

- `app/controls`
- `app/views`
- `app/styles`

These are allowed only as UI surface layers. They must not become catch-all runtime owners.

### Boundary contract layer

- `app/service/ports`

This is the sanctioned owner for interface-style port contracts. It must stay contract-only.

## Review Checklist

Before merging a new module, verify:

1. it has one dominant responsibility
2. its suffix matches real responsibility
3. it does not duplicate an existing owner
4. it does not introduce local literals that belong in a contract
5. if it uses `create()`, the factory is justified by injected runtime dependencies
6. if it is stateless, it exports direct module methods instead of a factory

## Enforcement

The architecture is now backed by governance checks.

Existing:

- framework alias gate
- framework token drift gate
- feature token drift gate
- forbidden literals gate
- duplicate responsibility gate

New:

- adapter factory boundary gate

## Final Position

The frontend architecture is now governed by explicit canonical ownership:

- controllers are thin
- features own feature behavior
- domain owns business orchestration
- framework owns generic runtime
- adapters own transport boundaries
- contracts own shared semantics
- `service/features` is the only sanctioned feature owner

Any new code that violates this map is architecture debt by definition.
