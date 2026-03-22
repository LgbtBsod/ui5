# Final Compliance Report

## Current delta after latest refactor block
- Technical identity alias lists are no longer duplicated across active payload, snapshot, and delta mapping paths.
- Shared field-name sets now live in `app/infra/adapters/shared/ODataEntityContracts.js`.
- This reduced constants debt and truth drift without adding a new architecture layer.
- Full proof of productive `DB_KEY/PARENT_KEY` canon still remains backend-dependent and is tracked as not fully proven from repo-only evidence.
- Search action flow now has one controller-level owner in `app/controller/search/SearchActionBehavior.js`; three pass-through wrappers were removed.
- Local fallback build is hardened against transient `dist` cleanup failure on `ENOTEMPTY`.
- Detail attachment drop zone now has one owner in `app/controller/detail/AttachmentDropZoneRuntime.js`; binding/event/visual micro-runtimes were removed.
- Search selection runtime now owns focus/table behavior directly; two internal helper runtimes were removed without changing search semantics.
- Detail validation flow now has one public runtime owner in `app/controller/detail/DetailValidationSummaryRuntime.js`; state/focus/reactive wrappers were removed.
- Local preload fallback is hardened against transient `ENOENT` while `dist` is being refreshed in local non-SAP build mode.
- Search viewport/scroll/sticky logic now has one owner in `app/service/features/search/runtime/SearchViewportRuntime.js`; four helper runtimes were removed.
- Search controller cluster was reduced further by removing formatter/settings/factory wrappers and moving that logic into `SearchControllerBehavior.js` and `SearchToolbarDialogRuntime.js`.
- Search screen no longer depends on `internal/SearchStartupBehavior`, `internal/SearchViewLoadBehavior`, `internal/SearchFilterLifecycleBehavior`, or `internal/SearchRequestRuntime`; their logic now lives in `SearchLifecycleBehavior.js` and `SearchSmartTableBehavior.js`.

## Current state before next refactor block
- Truth artifacts are now present in `.md` form.
- Detail domain overengineering was reduced by merging thin wrapper layers into `DetailFacade.js`.
- Canonical contracts for lock, attachments, runtime settings, and search semantics are already documented and partially hardened in code.

## Current compliance snapshot
- Production-ready: Partial
- SAP best practices: Partial
- TZ 2.1 alignment: Partial
- Overengineering reduction: Partial
- Duplicate ownership removal: Partial
- Constants extraction: Partial

## Highest remaining gaps
- key contract transparency around `DB_KEY / PARENT_KEY` is not yet fully proven across all active flows
- detail/search/app/analytics controller fragmentation remains high
- search selection/viewport runtime ownership remains duplicated
- CSS SAP-internal override debt remains high
- raw technical string debt still exists in touched controller/runtime areas

## Next recommended implementation order
1. key contract proof and alias cleanup
2. search controller/runtime merge
3. detail attachment/dropzone and validation owner cleanup
4. app shell owner cleanup
5. CSS override reduction and residual-risk documentation
