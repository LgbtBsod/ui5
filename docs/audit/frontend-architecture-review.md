# Frontend Architecture Review

Date: 2026-03-12
Scope: `SAPUI5 1.71` frontend in `app/`
Review goal: assess current frontend architecture, identify structural errors, duplication, UX/UI debt, and define a refactoring path toward a modular `SRP` architecture with smart contracts, maximum code reuse, and extension through behavior modules

## Executive Conclusion

The frontend is not chaotic, but it is structurally inconsistent.

The codebase already contains the right architectural ingredients:
- domain use cases
- facades
- contracts
- adapters
- policies
- behavior modules
- runtime helpers

But these ingredients are not yet enforced as the only way to implement behavior.

As a result, the actual architecture is split into two competing systems:
- intended modular architecture in `service/domain`, `service/contracts`, `service/framework/behavior`, `infra/adapters`
- practical orchestration architecture concentrated in `controller/support` and large framework runtime modules

This is the main frontend problem.

The project is therefore in a transitional state:
- better than a raw controller-driven UI5 app
- not yet a disciplined SRP-driven modular frontend

## 1. Current Structure

### Positive structural elements already present

The frontend already has strong architectural building blocks:

- `service/domain`
  - domain-oriented use cases for search, detail, analytics, cache, shared startup
- `service/contracts`
  - explicit contracts for workflow, navigation, dialog, analytics
- `infra/adapters`
  - backend integration adapters such as [ODataChecklistRepoAdapter.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/ODataChecklistRepoAdapter.js)
- `service/framework/behavior`
  - behavior registry, resolver, default handlers, override handlers
- `service/framework`
  - effect, policy, runtime, coordination, scheduling and state helpers
- `controller/base`
  - mixins for shared capabilities

This means the codebase already aims at:
- reuse
- extension by behavior
- contract-based orchestration
- controller thinning

### Structural reality in execution

Despite the good building blocks, runtime responsibility is still concentrated in large support modules:

- [SearchControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/support/SearchControllerActions.js)
- [AnalyticsControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/support/AnalyticsControllerActions.js)
- [DetailViewRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/support/DetailViewRuntime.js)
- [SearchViewportRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/support/SearchViewportRuntime.js)
- [SearchSelectionRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/support/SearchSelectionRuntime.js)
- [SearchViewRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/support/SearchViewRuntime.js)
- [DetailChecklistRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/support/DetailChecklistRuntime.js)
- [ComponentInitRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitRuntime.js)

This creates a split architecture:
- design intent says "use use cases and smart contracts"
- implementation reality often says "orchestrate directly in support/runtime modules"

### Resulting structural shape

Current structure is best described as:
- `layer-rich`
- `module-heavy`
- `partially contract-driven`
- `insufficiently normalized`

## 2. Anti-Patterns

### Anti-pattern 1: Parallel responsibility stacks

The same business flow is frequently represented in multiple places:
- controller support module
- facade
- use case
- adapter
- framework runtime helper

Why it is a problem:
- it breaks SRP at the system level even if each individual file looks reasonable
- behavior becomes hard to trace
- changes require cross-layer hunting

Target correction:
- each concern must have one primary owner
- controllers should invoke use cases and behaviors, not recompose domain logic

### Anti-pattern 2: Oversized orchestration modules

Largest modules in `controller/support` are too big and too behavior-dense for stable SRP architecture.

Examples:
- [SearchControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/support/SearchControllerActions.js)
- [AnalyticsControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/support/AnalyticsControllerActions.js)
- [DetailViewRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/support/DetailViewRuntime.js)

Why it is a problem:
- these files become hidden sub-frameworks
- they centralize branching, side effects, state writes, and UI wiring
- they weaken the role of facades, use cases, and behavior modules

Target correction:
- split them by user intent and business capability
- keep orchestration shallow

### Anti-pattern 3: State contract split across multiple authorities

State ownership is split between:
- [StatePaths.js](/C:/Users/lgbtb/Desktop/ui5/app/model/StatePaths.js)
- [DomainStatePaths.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/shared/DomainStatePaths.js)
- controller/support modules that still know too much about concrete paths

Why it is a problem:
- state path literals and state meaning drift over time
- business rules leak into UI wiring

Target correction:
- one authoritative state contract
- one mapping layer from domain meaning to UI model paths

### Anti-pattern 4: Framework overgrowth

`service/framework` is powerful, but it is becoming a second application layer rather than a thin support layer.

Signals:
- coordinators
- runtimes
- policies
- effects
- dispatchers
- decision coordinators
- shell coordinators

Why it is a problem:
- too much framework code can make simple flows harder instead of easier
- framework abstractions start competing with domain abstractions

Target correction:
- framework should support behavior, not own business behavior

### Anti-pattern 5: Smart contracts exist, but are not fully authoritative

There are explicit contract modules:
- [WorkflowContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/contracts/WorkflowContracts.js)
- [AnalyticsContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/contracts/AnalyticsContracts.js)
- [NavigationContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/contracts/NavigationContracts.js)
- [DialogContracts.js](/C:/Users/lgbtb/Desktop/ui5/app/service/contracts/DialogContracts.js)

But behavior is still often shaped by local controller/runtime assumptions.

Why it is a problem:
- contracts stop being "source of truth"
- local orchestration starts redefining allowed behavior

Target correction:
- smart contracts must become mandatory integration boundaries
- repeated local decision logic should move into contract-aware behavior modules

### Anti-pattern 6: UX behavior is too runtime-customized

Examples:
- [AppShellHeader.js](/C:/Users/lgbtb/Desktop/ui5/app/control/AppShellHeader.js)
- [ThemeService.js](/C:/Users/lgbtb/Desktop/ui5/app/util/ThemeService.js)
- large CSS modules for search and detail screens

Why it is a problem:
- common UI behavior is implemented through custom shell/runtime code instead of stable reusable UX primitives
- responsiveness, consistency, and supportability degrade over time

Target correction:
- move repeated view behavior into reusable view-behavior modules
- leave only deliberate product differentiation in custom controls

## 3. Duplication Zones

### Zone 1: Orchestration duplication

The same flow shape appears repeatedly across search, detail, and analytics:
- collect context
- write busy state
- call service or facade
- normalize result
- write state
- show feedback
- route focus or layout effects

Problem:
- same behavioral skeleton is repeated in many controller support files

Needed action:
- extract common orchestration templates into behavior-driven action runners

### Zone 2: State mutation duplication

State updates are repeated through:
- `ModelStateRuntime`
- controller support modules
- local state path resolution helpers

Problem:
- different modules know both the path and the meaning

Needed action:
- create one state contract layer plus typed state operations per capability

### Zone 3: Normalization and mapping duplication

Recurring mapping/normalization logic exists around:
- search payloads
- analytics payloads
- detail snapshot/save flows
- OData contract adaptation

Representative files:
- [AnalyticsPayloadNormalizer.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/analytics/AnalyticsPayloadNormalizer.js)
- [ODataChecklistRepoAdapter.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/ODataChecklistRepoAdapter.js)
- detail use cases and support modules

Problem:
- contract adaptation is not centralized enough

Needed action:
- one mapper per contract boundary
- one normalizer per domain capability

### Zone 4: UI behavior duplication

Repeated UI patterns likely exist for:
- busy handling
- dialog opening
- focus restoration
- banner/feedback handling
- attachment interaction
- selection handling

Problem:
- similar components do not derive behavior from the same shared behavior module

Needed action:
- move common component behavior into reusable behavior packs
- allow override only through explicit behavior extension points

### Zone 5: Script and governance duplication

The duplicate governance gate already reports same-root duplicate clusters in scripts.

Problem:
- governance layer itself shows duplicated logic

Needed action:
- refactor script shared kernels first, because architecture governance should model the same discipline required from the app

## 4. UX/UI Problems

### Problem 1: Shell behavior is too custom

The custom shell header is a product decision, but today it also acts as a structural dependency.

Risk:
- common shell behavior, layout semantics, and action affordances become harder to standardize

Needed action:
- treat shell behavior as an extension module, not as a primary source of app-wide UX logic

### Problem 2: Theme behavior is over-engineered

Theme logic spans local storage, DOM classes, UI5 theme switching, and runtime token handling.

Risk:
- theme changes can influence startup, rendering, and maintenance complexity more than necessary

Needed action:
- separate theme preference persistence, theme application, and decorative transitions

### Problem 3: Large CSS surface means fragile screen behavior

Very large CSS modules exist for key screens.

Risk:
- visual fixes become layout regressions
- design language becomes difficult to control

Needed action:
- split CSS by component responsibility and design token domain
- remove screen-global styling where local component styling should exist

### Problem 4: Readiness and interaction are too monolithic

Search, detail, analytics, and optional enrichments are still too coupled during app startup and screen transitions.

Risk:
- app feels heavier than it needs to be
- small failures degrade the whole screen

Needed action:
- progressive readiness by screen zone
- segmented busy and error states

### Problem 5: Similar components do not yet share enough behavior

Your stated target is correct:
- identical component classes should inherit common behavior
- extensions should happen through behavior modules

Current issue:
- that architecture exists in principle, but not yet consistently across screen-level components

Needed action:
- define canonical behavior packs for repeated component families

## 5. Errors And Structural Defects

These are the main frontend architecture errors visible from the current repo.

### Error 1

There is no single authoritative architectural path for implementing new behavior.

Consequence:
- engineers can add behavior in use cases, controller support, framework runtime, facades, or adapters
- the architecture does not yet strongly constrain bad choices

### Error 2

SRP is partially present per file, but violated across flow ownership.

Consequence:
- one feature may have many owners
- debugging and refactoring cost stay high

### Error 3

Behavior modules exist, but are not yet the default extension mechanism for repeated UI behavior.

Consequence:
- local variations are implemented ad hoc
- reuse remains below potential

### Error 4

Smart contracts are not enforced as hard boundaries.

Consequence:
- duplicated rule interpretation
- hidden behavior drift

### Error 5

Controller support layer is too thick.

Consequence:
- controllers are not truly thin
- support files become feature engines

### Error 6

State model semantics are not centralized enough.

Consequence:
- path-level coupling remains high
- migration to more disciplined readiness contracts will be harder than necessary

### Error 7

Performance and readiness concerns are mixed into general orchestration instead of being modeled explicitly.

Consequence:
- startup stays broad and fragile
- optimization remains local instead of systemic

## 6. Target Frontend Architecture

### Core principles

Target frontend must be:
- modular
- SRP-oriented
- contract-driven
- behavior-extendable
- performance-aware
- view-thin

### Recommended target layers

#### 1. View layer

Contains:
- XML views
- fragments
- very small custom controls only where product differentiation requires them

Rules:
- no business decisions
- no contract interpretation
- no direct backend knowledge

#### 2. Controller layer

Contains:
- event entry points only
- context extraction only
- delegation to use case or behavior coordinator

Rules:
- controller methods should be thin
- no complex state mutation logic
- no repeated busy/error orchestration

#### 3. Behavior layer

Contains:
- shared behavior packs for reusable component families
- default behavior handlers
- override handlers
- behavior registry and resolver

Rules:
- this should be the main extension mechanism
- repeated interaction behavior must live here first

Examples of behavior packs to formalize:
- list selection behavior
- detail edit-lock behavior
- attachment interaction behavior
- dialog open/close lifecycle behavior
- feedback/banner behavior
- progressive readiness behavior

#### 4. Use case layer

Contains:
- one use case per business intention
- input normalization
- contract-based orchestration
- result/effect production

Rules:
- no view-specific path knowledge
- no direct DOM knowledge

#### 5. Contract layer

Contains:
- business contracts
- state contracts
- view-path contracts
- readiness contracts
- capability contracts

Rules:
- one authoritative contract per concept
- no duplicate meaning in local helpers

#### 6. Adapter layer

Contains:
- backend/OData adaptation
- payload mapping
- transport-specific error normalization

Rules:
- no UI-specific state logic
- one adapter boundary per external contract

#### 7. Framework support layer

Contains:
- generic infrastructure only
- scheduler
- effect applier
- telemetry
- resilience policy
- generic dispatch

Rules:
- must not become a second domain layer

## 7. What And How To Cut

### Priority A: Cut oversized support modules by user intent

Files:
- [SearchControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/support/SearchControllerActions.js)
- [AnalyticsControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/support/AnalyticsControllerActions.js)
- [DetailViewRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/support/DetailViewRuntime.js)

How to cut:
- split by intent, not by helper type
- create modules like:
  - `SearchBootstrapBehavior`
  - `SearchExecutionBehavior`
  - `SearchSelectionBehavior`
  - `DetailOpenBehavior`
  - `DetailEditBehavior`
  - `DetailValidationBehavior`
  - `AnalyticsLoadBehavior`
  - `AnalyticsExportBehavior`

Rule:
- each module should own one flow family only

### Priority B: Cut state ownership into one contract system

Files:
- [StatePaths.js](/C:/Users/lgbtb/Desktop/ui5/app/model/StatePaths.js)
- [DomainStatePaths.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/shared/DomainStatePaths.js)

How to cut:
- keep one canonical state contract
- move all semantic state operations into capability-specific state access services
- ban direct state-path knowledge from controller support modules except through those access services

### Priority C: Cut normalization logic out of controllers and runtimes

Files:
- [ODataChecklistRepoAdapter.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/ODataChecklistRepoAdapter.js)
- [AnalyticsPayloadNormalizer.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/analytics/AnalyticsPayloadNormalizer.js)
- detail save and attachment use cases

How to cut:
- one mapper per backend contract boundary
- one domain normalizer per business capability
- controllers consume only normalized domain results

### Priority D: Cut reusable component behavior into behavior modules

Target repeated behaviors:
- selection
- busy handling
- validation display
- lock/edit switching
- attachment upload interaction
- dialog orchestration
- feedback presentation

How to cut:
- create canonical default handlers
- expose override points only where product needs variation
- route all repeated component behavior through the behavior registry

### Priority E: Cut custom shell/theme logic into isolated product modules

Files:
- [AppShellHeader.js](/C:/Users/lgbtb/Desktop/ui5/app/control/AppShellHeader.js)
- [ThemeService.js](/C:/Users/lgbtb/Desktop/ui5/app/util/ThemeService.js)

How to cut:
- isolate product differentiation from infrastructure concerns
- move persistence, application, animation, and presentation into separate modules
- avoid letting theme and shell utilities shape app-wide runtime behavior

## 8. Development Plan

### Wave 1: Architectural constraint pass

Goal:
- make the desired architecture enforceable

Actions:
- define one official behavior-implementation path
- define one official state contract source
- define one official normalization boundary per external contract
- document banned patterns for new code

### Wave 2: Controller/support decomposition

Goal:
- shrink thick support files

Actions:
- split search, detail, and analytics support files by user intent
- move repeated effect logic into behavior modules
- move state access into dedicated access services

### Wave 3: Behavior-first reuse model

Goal:
- make identical components share identical behavior

Actions:
- build canonical behavior packs
- bind screens to those packs through registry/resolver
- replace local ad hoc variants with override handlers

### Wave 4: UX normalization

Goal:
- reduce fragility in shell, theme, and large-page styling

Actions:
- isolate shell behavior
- simplify theme runtime responsibilities
- split large CSS surfaces by local component ownership

### Wave 5: Performance and readiness architecture

Goal:
- introduce segmented readiness as an architectural feature

Actions:
- define readiness contracts
- split startup into critical and deferred bands
- make search/detail/analytics independently ready

## 9. Final Architectural Position

Your target statement is correct and should become the governing architecture rule:

The frontend should be a modular SRP structure with smart contracts, maximal code reuse for common behavior across equivalent components, and extension through behavior modules.

The current codebase is directionally aligned with that target, but not yet structurally compliant with it.

The main task is not inventing a new architecture.
The main task is forcing the existing good architectural ideas to become the only valid implementation path.
