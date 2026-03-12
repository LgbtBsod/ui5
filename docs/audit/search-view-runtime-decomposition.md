# SearchViewRuntime Decomposition

Date: 2026-03-12
Source module: `app/controller/support/SearchViewRuntime.js`
Goal: decompose the current search runtime into capability-oriented modules with explicit ownership boundaries

## Current Mixed Responsibilities

`SearchViewRuntime` currently mixes at least five concerns:

1. startup performance markers
2. search loading feedback and pending-load settlement
3. analytics rail refresh scheduling
4. smart table runtime wiring
5. export and analytics navigation actions

This is why the file could not be moved cleanly as a whole. Its imports span controller support, framework state helpers, and capability actions.

## Target Files List

### Capability runtime files

- `app/service/features/search/runtime/SearchLoadingFeedbackRuntime.js`
  - pending-load timers
  - working hint scheduling
  - load success/failure settlement
- `app/service/features/search/runtime/SearchAnalyticsRailRuntime.js`
  - analytics refresh timer
  - deferred analytics startup
  - analytics rail pulse/update logic
- `app/service/features/search/runtime/SearchTableRuntime.js`
  - smart table initialize wiring
  - before-rebind orchestration bridge
  - export busy state helpers
- `app/service/features/search/runtime/SearchStartupRuntime.js`
  - startup perf marks
  - first-route-ready and staged readiness instrumentation

### Remaining controller-facing facade

- `app/controller/support/SearchViewRuntime.js`
  - temporary thin facade only
  - delegates to capability runtime modules
  - should become empty then removable after controller layer is normalized

## First Real Cut

The first extracted slice is:
- analytics rail refresh and scheduling

Reason:
- cohesive responsibility
- minimal coupling to table/selection wiring
- clear performance impact
- direct fit for progressive readiness architecture

## Post-Cut Ownership

- analytics refresh cadence belongs to capability runtime, not controller support
- controller support should only call:
  - `bindAnalyticsRefreshTimer`
  - `clearAnalyticsRefreshTimer`
  - `refreshAnalyticsRail`
  - `scheduleInitialAnalytics`

## Next Cuts After This One

Completed:
- [x] analytics rail refresh and scheduling
- [x] search loading feedback and pending-load settlement
- [x] smart table runtime wiring and before-rebind orchestration
- [x] export and navigation runtime

Current extracted modules:
- [SearchAnalyticsRailRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchAnalyticsRailRuntime.js)
- [SearchLoadingFeedbackRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchLoadingFeedbackRuntime.js)
- [SearchSmartTableRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchSmartTableRuntime.js)
- [SearchActionRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchActionRuntime.js)

Remaining cuts:
1. route/startup readiness markers
2. final facade thinning and relocation strategy for `SearchViewRuntime`
