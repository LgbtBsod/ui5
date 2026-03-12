# Frontend Architecture Review

Date: 2026-03-12
Scope: `SAPUI5 1.71` frontend in `app/`
Review goal: reflect the actual current frontend structure after the cleanup waves, not the historical in-flight target

## Executive Conclusion

The frontend is no longer organized around the old emergency layers.

The core execution path is now centered on:

- `controller/*` capability entry points
- `service/features/*` feature runtime and feature contracts
- `service/domain/*` business orchestration
- `service/framework/*` generic runtime infrastructure
- `service/shared/*` cross-feature shared helpers
- `infra/adapters/*` transport and platform boundaries

The architecture is materially cleaner than the original baseline. The remaining work is normalization and governance, not structural rescue.

## 1. Current Structure

### Active canonical owners

- `app/contracts`
  - shared business semantics
- `app/model`
  - model schemas and canonical state paths
- `app/controller`
  - thin controller entry points and controller-local behavior
- `app/service/features`
  - feature runtimes and feature-local contracts
- `app/service/domain`
  - use cases and business-side orchestration
- `app/service/framework`
  - generic runtime stages, feedback, navigation, scheduling, telemetry, session/runtime infrastructure
- `app/service/shared`
  - cross-feature readers, identity, delta and utility helpers
- `app/infra/adapters`
  - backend and platform integration boundaries

### Current physical reality

- `app/controller/support` is physically removed
- `app/util` is physically removed
- `control`, `view`, and `css` were normalized to `controls`, `views`, and `styles`
- top-level `ports` were normalized into `app/service/ports`
- the active code path no longer depends on historical fallback layers

### Current controller map

- `app/controller/app`
- `app/controller/search`
- `app/controller/detail`
- `app/controller/analytics`
- `app/controller/shared`
- `app/controller/base`

This is now the real controller structure. The previous `controller/support` execution layer is no longer active.

## 2. What Was Actually Fixed

### Legacy owner model is no longer dominant

The previous practical architecture relied on:

- `controller/support`
- `util`
- oversized mixed runtime modules
- alias-heavy framework seams

That is no longer the main shape of the codebase.

### Reuse is now routed through explicit owners

Representative shared owners now include:

- [BindingContextReader.js](C:/Users/lgbtb/Desktop/ui5/app/service/shared/BindingContextReader.js)
- [ChecklistIdentity.js](C:/Users/lgbtb/Desktop/ui5/app/service/shared/ChecklistIdentity.js)
- [CloneUtil.js](C:/Users/lgbtb/Desktop/ui5/app/service/shared/CloneUtil.js)
- [ClientKeyGenerator.js](C:/Users/lgbtb/Desktop/ui5/app/service/shared/ClientKeyGenerator.js)
- [DeltaPayloadBuilder.js](C:/Users/lgbtb/Desktop/ui5/app/service/shared/DeltaPayloadBuilder.js)
- [UseCaseValue.js](C:/Users/lgbtb/Desktop/ui5/app/service/domain/shared/UseCaseValue.js)

Feature reuse is also now more explicit:

- [SearchBindingPolicy.js](C:/Users/lgbtb/Desktop/ui5/app/service/features/search/contracts/SearchBindingPolicy.js)
- [SearchFilterBuilder.js](C:/Users/lgbtb/Desktop/ui5/app/service/features/search/contracts/SearchFilterBuilder.js)
- [SearchMaxResults.js](C:/Users/lgbtb/Desktop/ui5/app/service/features/search/contracts/SearchMaxResults.js)
- [AttachmentUploadPolicy.js](C:/Users/lgbtb/Desktop/ui5/app/service/features/detail/contracts/AttachmentUploadPolicy.js)
- [ValidationPathMap.js](C:/Users/lgbtb/Desktop/ui5/app/service/features/detail/contracts/ValidationPathMap.js)

### Framework is now a real runtime layer

`service/framework` now works as:

- runtime stages
- runtime policies
- effect and feedback primitives
- scheduling, session, cross-tab and telemetry runtime
- canonical framework contracts

It is no longer primarily a wrapper layer around `Support` and `Bootstrap` files.

### Adapters are cleaner boundaries

`infra/adapters` now keeps factory boundaries only where runtime DI value exists.

Allowed stateful factory boundaries:

- [BrowserCacheAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/BrowserCacheAdapter.js)
- [DictAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/DictAdapter.js)
- [ODataChecklistRepoAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/ODataChecklistRepoAdapter.js)
- [SmartControlsAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/SmartControlsAdapter.js)
- [TelemetryAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/TelemetryAdapter.js)
- [Ui5StateAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/Ui5StateAdapter.js)

Stateless adapter factories were collapsed into direct module APIs.

## 3. Remaining Structural Risks

### Risk 1: empty historical directories still exist

The most misleading legacy folders are already removed, which materially reduces the chance of future ownership drift.

### Risk 2: historical surface naming still remains

The major naming cleanup is already complete. The active ownership model now lives under `controller`, `controls`, `views`, `styles`, `service`, `model`, `contracts`, and `infra`.

### Risk 3: framework still has some large orchestration entry points

These files are valid runtime entry points, but they remain high-governance modules:

- [ComponentInitRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitRuntime.js)
- [ComponentBootRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentBootRuntime.js)
- [EffectApplier.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/EffectApplier.js)
- [FacadeCommandRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/FacadeCommandRuntime.js)

They must stay coordination-focused and must not regrow mixed feature logic.

### Risk 4: documentation can still drift faster than structure

This review replaces earlier transitional statements that were still describing removed layers as active owners. That drift risk remains real and should be treated as governance debt, not editorial trivia.

## 4. UX/UI Position

The UX-supporting architecture is much cleaner than before, but not fully normalized.

Remaining UX-adjacent structural debt:

- remaining UI runtime still spans `controls`, `views`, and `styles` rather than a more explicit UI capability map
- custom shell and theme runtime still concentrated in framework/runtime modules instead of a more explicit UI capability map

This is no longer destabilizing the codebase, but it remains part of the target cleanup.

## 5. Current Target Model

The enforced model is now:

The frontend must remain a modular SRP structure with smart contracts, maximal code reuse for equivalent behavior, and extension through explicit runtime and behavior modules.

### Practical interpretation

- controllers are entry points, not orchestration dumps
- feature behavior belongs in `service/features/*` and feature-specific controller modules
- business orchestration belongs in `service/domain/*`
- generic runtime belongs in `service/framework/*`
- shared cross-feature helpers belong in `service/shared/*`
- integration boundaries belong in `infra/adapters/*`
- repeated semantics belong in canonical contracts
- factories are allowed only when they capture real runtime dependencies

## 6. Closeout Position

Compared to the original baseline, the dominant frontend risks have changed.

The old core problems:

- thick support-folder execution
- `util` sprawl
- duplicated helper owners
- alias-heavy framework seams
- unstable boundary ownership

are no longer the primary architecture story.

The remaining work is:

- final governance hardening around the new top-level map
- governance hardening so the codebase does not regress back into mixed ownership
