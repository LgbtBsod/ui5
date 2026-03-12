# Architecture Closeout Next Steps

## Current State
- `app/controller/support` removed
- `app/util` removed
- `detail` and `analytics` panes are now lazy-created
- `detail` and `analytics` page CSS are now lazy-loaded on first pane activation
- `service/framework` remains on `runtime + contracts only`

## Remaining Structural Hotspots
- `app/controller/search/SearchControllerBehavior.js`
  - Still too large for a pure controller-facing facade.
  - Next cut: split route/startup orchestration, filter-bar intent handlers, export/copy/open command handlers.
- `app/service/features/search/runtime/SearchViewportRuntime.js`
  - Still mixes viewport sync, selection visibility policy, sticky timing, and request-window coordination.
  - Next cut: extract viewport geometry policy and scroll/focus synchronization.
- `app/service/features/search/runtime/SearchSelectionRuntime.js`
  - Still carries selection normalization and cross-runtime coordination in one file.
  - Next cut: isolate selected-row identity policy from UI synchronization.
- `app/controller/analytics/AnalyticsControllerBehavior.js`
  - Better than before, but still too large for a final thin facade.
  - Next cut: move remaining event-normalization helpers and state validation into feature runtime.
- `app/views/Search.view.xml`
  - Still too large and likely holds multiple visual zones that should become fragments.
  - Next cut: split toolbar/filter/summary/result zones into dedicated fragments.
- `app/views/Detail.view.xml`
  - Still large and should be cut by capability region.
  - Next cut: separate access-state banner area, card rail, attachment region, and value-help-heavy subsections.
- `app/views/fragment/WorkflowAnalyticsBreakdowns.fragment.xml`
  - Large analytics fragment remains a strong candidate for lazy drilldown fragment split.
- `app/styles/modules/40_page_search.css`
  - Largest remaining CSS module.
  - Next cut: split into shell/search layout, results table, command rail, and responsive overrides.
- `app/styles/modules/41_page_detail.css`
  - Now lazy-loaded, but still too large.
  - Next cut: split into detail shell, cards, attachments, and narrow viewport overrides.
- `app/Component.js`
  - Still carries too much assembly knowledge.
  - Next cut: move remaining component composition into small runtime builders and keep `Component.js` as entry shell.
- `app/service/backend/GatewayClient.js`
  - Still large and should be reviewed for transport concerns vs request policy concerns.

## Performance / Loading Priorities
- Keep `Search` as the only eager pane.
- Keep `Detail` and `Analytics` views lazy.
- Keep `detail` and `analytics` page CSS lazy.
- Next performance wave:
  - lazy-load analytics heavy fragments/dialogs
  - split search/detail XML into smaller fragments
  - reduce eager CSS weight in `40_page_search.css`
  - review whether non-critical analytics export/report assets can move to deferred fragment/runtime loading

## Canonical Rules To Preserve
- New pane/view/style ids must come from `app/contracts/ShellPaneContracts.js`.
- Layout navigation must continue to use sanctioned page ids via `app/contracts/NavigationContracts.js`.
- Progressive pane readiness must use `app/service/contracts/ProgressiveReadinessContracts.js`.
- Theme literals must stay centralized in `app/service/framework/ThemeContracts.js`.

## Recommended Next Wave
1. Thin `SearchControllerBehavior.js`
2. Split `Search.view.xml` into fragments
3. Split `40_page_search.css`
4. Thin `AnalyticsControllerBehavior.js`
5. Split `Detail.view.xml` and `41_page_detail.css`
6. Review `Component.js` and `GatewayClient.js`
