# Final Compliance Report

## Closed In This Pass
- UI decision dispatch overengineering was reduced by removing the extra coordinator layer:
  - `app/service/framework/execution/UiDecisionCoordinator.js` was deleted
  - consumers now call the canonical owner `app/service/framework/behavior/UiDecisionDefaultHandlers.js` directly
  - `BehaviorScopes.uiDecision` was removed because no override-based consumer existed
- UI decision helper cleanup was completed by merging:
  - `app/service/framework/execution/behavior/UiDecisionBehaviorHelpers.js` into `app/service/framework/behavior/UiDecisionDefaultHandlers.js`
  - toast/error/delete-confirm helper logic now lives in the only default decision-handler owner
- Detail controller helper cleanup was completed by merging:
  - `app/controller/detail/DetailActionDialogRuntime.js` into `app/controller/detail/DetailControllerRuntime.js`
  - `app/controller/detail/DetailAdaptiveViewportRuntime.js` into `app/controller/detail/DetailControllerRuntime.js`
  - detail dialog focus plumbing and adaptive viewport syncing now live in the only controller runtime owner
- Navigation behavior helper cleanup was completed by merging:
  - `app/service/framework/execution/behavior/NavigationBehaviorHelpers.js` into `app/service/framework/behavior/NavigationDefaultHandlers.js`
  - framework default navigation handlers now depend directly on `WorkspaceRouteNavigation` instead of a single-consumer proxy helper
- Detail validation helper cleanup was completed by merging:
  - `app/controller/detail/internal/DetailValidationHelperRuntime.js` into `app/controller/detail/DetailValidationSummaryRuntime.js`
  - detail validation path/value helpers now live in the only owner that computes and applies the summary
- Search action busy wrapper cleanup was completed by merging:
  - `app/service/framework/ControllerActionBusyRuntime.js` into `app/controller/search/SearchActionBehavior.js`
  - busy toggling for search actions now lives in the only owner that uses it
- Additional component/runtime wrapper cleanup was completed by merging:
  - `app/service/runtime/component/ComponentNavigationRuntime.js` into `app/service/runtime/component/ComponentLifecycleRuntime.js`
  - `app/service/runtime/component/ComponentInternalRuntimeState.js` into `app/service/runtime/component/ComponentModelInitRuntime.js`
  - navigation intent plumbing and internal runtime state seeds now live in their real owners
- Final component bootstrap wrapper cleanup was completed by merging:
  - `app/service/runtime/component/ComponentRuntimeOptionsFactory.js` into `app/service/framework/ComponentBootstrap.js` and `app/service/runtime/component/ComponentLifecycleRuntime.js`
  - runtime option composition now lives with the two real owners that consume it
- Broken import protection was hardened in [scripts/broken-import-gate.js](C:/Users/lgbtb/Desktop/ui5/scripts/broken-import-gate.js):
  - it now fails on unresolved application modules, not only on a small deleted-module denylist
  - deleted `RootIdRuntime` remains forbidden
- Final bootstrap micro-fragmentation cleanup was completed by merging:
  - `app/service/runtime/component/ComponentMainServiceRuntime.js` into `app/service/framework/ComponentBootstrap.js`
  - `app/service/runtime/component/ComponentModelBootstrap.js` into `app/service/framework/ComponentBootstrap.js`
  - `app/service/runtime/component/ComponentGuardedSaveRuntime.js` into `app/service/runtime/component/ComponentSaveGuardRuntime.js`
- Framework context micro-fragmentation was reduced by merging:
  - `app/service/framework/CtxModelResolver.js` into `app/service/framework/CtxRuntimeFactory.js`
  - `app/service/framework/CtxCacheRuntimeFactory.js` into `app/service/framework/CtxRuntimeFactory.js`
- Mock gateway key compatibility was tightened in [gateway_canonical_api.py](C:/Users/lgbtb/Desktop/ui5/backend/mock_gateway/api/gateway_canonical_api.py):
  - child parent normalization no longer treats child `DB_KEY` as parent compatibility input
  - root filter alias normalization is centralized in a single ingress helper
- Metadata drift verification was hardened in [scripts/metadata-contract-gate.js](C:/Users/lgbtb/Desktop/ui5/scripts/metadata-contract-gate.js):
  - gate now executes `metadata_builder.py` and compares produced metadata with `app/localService/metadata.xml`
  - gate now fails on `RootKey` or `RootId` in productive ABAP DPC
- Constants ownership verification was tightened:
  - [scripts/detail-contracts-owner-gate.js](C:/Users/lgbtb/Desktop/ui5/scripts/detail-contracts-owner-gate.js) now fails on forbidden message/text members still read from `DetailContracts`
  - [scripts/proxy-constants-gate.js](C:/Users/lgbtb/Desktop/ui5/scripts/proxy-constants-gate.js) now fails on residual frontend text ownership leaking through `DetailContracts`
- SAP internal CSS governance was tightened:
  - [scripts/sap-internal-css-gate.js](C:/Users/lgbtb/Desktop/ui5/scripts/sap-internal-css-gate.js) now flags stale allowlist entries
  - stale allowlist entries for `24_switches_and_toggles.css` and `33_overflow_and_badges.css` were removed from [sap-internal-css-allowlist.json](C:/Users/lgbtb/Desktop/ui5/scripts/sap-internal-css-allowlist.json)
  - stale allowlist entry for `31_feedback_runtime.css` was also removed after the remaining private selector was deleted
- Search command dispatch hygiene was cleaned in [SearchCommandPolicy.js](C:/Users/lgbtb/Desktop/ui5/app/controller/search/SearchCommandPolicy.js):
  - removed the mismatched unused factory argument
  - command execution now uses the canonical `ControllerRuntime.buildCtx`
- Residual JS hygiene issue in [Search.controller.js](C:/Users/lgbtb/Desktop/ui5/app/controller/Search.controller.js) was closed by restoring the missing `ModelStateRuntime` import.

## Canonical Model Status
- Root entity identity stays canonical on `DB_KEY`.
- Root-facing entities still do not expose `PARENT_KEY`.
- Child entities stay on own `DB_KEY` plus parent `PARENT_KEY`.
- Compatibility aliases `RootKey` and `RootId` remain boundary-only and are not expanded deeper into runtime/domain code.
- Attachment canonical contract remains `DownloadUrl` plus `DocumentHandle`, with `ContentBase64` kept only for upload payload ingress.

## Verification
- `node scripts/broken-import-gate.js`
- `node scripts/detail-contracts-owner-gate.js`
- `node scripts/key-model-gate.js`
- `node scripts/metadata-contract-gate.js`
- `node scripts/attachment-contract-gate.js`
- `node scripts/proxy-constants-gate.js`
- `node scripts/sap-internal-css-gate.js`
- `npm.cmd run lint:js`
- `npm.cmd run lint:css`
- `npm.cmd run validate:local`
- `npm.cmd run build`

## Environment-Limited Checks
- Local browser click-through flow was not completed because `npm.cmd run start` cannot resolve SAPUI5 framework packages in this environment:
  - missing `@sapui5/distribution-metadata@1.71.70`
- Productive SAPUI5 serve/build was therefore not available for interactive Playwright verification in this pass.
