# Architecture Freeze Rules

Date: 2026-03-13

Purpose: treat the current repo structure as the release-candidate architecture for commercial rollout and SAP-certification preparation.

## Structural Rules

- new modules must be real owners, never alias-only re-export shells
- new literals must go through canonical contracts
- new page CSS must be introduced only through zoned submodules imported by page-level aggregators
- new fragments are allowed only when they become stable bundle owners
- no reintroduction of `support`, `bootstrap`, or `util` as active ownership layers

## Bundle Rules

- eager bundle: shell + search critical path
- lazy detail bundle: `Detail.view.xml`, detail fragments, detail page CSS
- lazy analytics bundle: `Analytics.view.xml`, analytics drilldown/report fragments, analytics CSS
- deferred dialog bundle: sort/group/report/year-picker/value-help heavy UI

## Governance Rules

- any new transport/path/token decision must be represented in a canonical contract file
- any new cross-feature runtime behavior must live in `service/framework` or `service/shared`, not inside controllers
- page-level aggregators may stay as sanctioned bundle entries even if they are pure import shells
- mock backend behavior that diverges from productive SAP semantics must be explicit, local-only, and documented

## Freeze Exceptions

- regressions found on live FLP/Gateway contour
- productive SAP evidence gaps that require code-level hardening
- performance regressions proven by readiness metrics
