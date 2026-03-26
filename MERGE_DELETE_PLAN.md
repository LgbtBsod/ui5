# Merge / Delete Plan

## 2026-03-27 Final Pass Outcome

### 2026-03-27 Final Hardening Addendum
- Additional delete completed in this addendum:
  - `app/controller/search/SearchCommandPolicy.js`
  - `app/controller/detail/DetailCommandPolicy.js`
- Focus shifted to targeted structural cleanup plus production-governance hardening:
  - removed `SearchCommandPolicy` because it only forwarded controller calls into `_facade`
  - removed `DetailCommandPolicy` because it only forwarded controller calls into `_detailService`
  - removed raw fallback UI copy from active analytics/search runtime
  - converted DOM allowlist from hardcoded JS array to reasoned `scripts/dom-hack-allowlist.json`
  - froze SAP CSS quarantine size inside `scripts/sap-internal-css-gate.js`

### Deleted Files
- `app/service/framework/ControllerRouteRuntime.js`
  - removed because it was a route attach/detach wrapper with no stable boundary
- `app/service/framework/FeedbackCoordinator.js`
  - removed because it only proxied feedback handlers/helpers without adding ownership value
- `app/controller/search/SearchCommandPolicy.js`
  - removed because it was a pass-through search facade dispatcher without its own semantic boundary
- `app/controller/detail/DetailCommandPolicy.js`
  - removed because it was a pass-through detail service dispatcher without its own semantic boundary

### Merged Files
- `app/service/framework/ControllerRouteRuntime.js`
  - destination owners:
    - `app/controller/analytics/AnalyticsLifecycleBehavior.js`
    - `app/controller/detail/DetailControllerBehavior.js`
    - `app/controller/search/SearchLifecycleBehavior.js`
  - why merged:
    - route lifecycle is controller-owned behavior, not framework-shared infrastructure
- `app/service/framework/FeedbackCoordinator.js`
  - destination owners:
    - `app/service/framework/behavior/FeedbackDefaultHandlers.js`
    - `app/service/framework/execution/behavior/FeedbackBehaviorHelpers.js`
    - consuming controllers/runtimes
  - why merged:
    - the file only re-exposed existing feedback owners and created naming overhead
- `app/controller/search/SearchCommandPolicy.js`
  - destination owners:
    - `app/controller/Search.controller.js`
    - `app/controller/search/SearchActionBehavior.js`
    - `app/controller/search/SearchLifecycleBehavior.js`
    - `app/controller/search/internal/SearchAnalyticsIntentBehavior.js`
  - why merged:
    - search command dispatch belongs to the search controller boundary and did not justify a separate policy wrapper
- `app/controller/detail/DetailCommandPolicy.js`
  - destination owners:
    - `app/controller/detail/DetailControllerRuntime.js`
    - `app/controller/detail/DetailInteractionRuntime.js`
    - `app/controller/detail/DetailPageFlow.js`
    - `app/controller/detail/internal/DetailChecklistRowBehavior.js`
    - `app/controller/detail/internal/DetailChecklistStateBehavior.js`
    - `app/service/features/detail/runtime/AttachmentUploadRuntime.js`
  - why merged:
    - detail command dispatch belongs to the detail controller/runtime boundary and did not justify a separate policy wrapper

### Remaining Follow-Up Candidates
- `ControllerModelRuntime.js`
  - still thin, but broad usage footprint makes removal higher-risk than the wrappers closed in this pass
