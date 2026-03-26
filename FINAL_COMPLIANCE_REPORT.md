# Final Compliance Report

## Final Production-Readiness Pass
- UI5 core facade overengineering was reduced:
  - deleted `app/service/framework/Ui5RuntimeFacade.js`
  - `app/controller/App.controller.js`, `app/controller/base/ThemeMixin.js`, `app/service/framework/ThemeService.js`, and `app/service/features/detail/runtime/DetailFormatters.js` now call `sap/ui/core/Core` directly
  - deleted `app/test/unit/framework/Ui5RuntimeFacade.qunit.js`
- Attachment mutation payload no longer leaks `value` into the productive save contract:
  - `app/service/shared/delta/DeltaFieldMappers.js` removed inline attachment `value`
  - canonical persisted attachment contract remains metadata-only (`DownloadUrl` / `DocumentHandle`)
- Attachment upload input validation no longer ships raw UI text from domain logic:
  - `app/service/domain/detail/usecases/AttachmentUploadUseCase.js` now returns machine-readable `INVALID_INPUT`
  - frontend user-facing copy moved to `app/constants/MessageKeyConstants.js` + i18n bundles via `attachmentTargetMissing`
- Raw detail formatter labels were removed from runtime:
  - `app/service/features/detail/runtime/DetailFormatters.js` no longer embeds `Date`, `Time`, `Timezone` strings
  - formatter now resolves `dateLabel`, `timeLabel`, `timezoneLabel` from i18n
- Bootstrap wrapper sprawl was reduced again:
  - deleted `app/service/runtime/component/ComponentBootstrapDependencyBuilder.js`
  - merged its grouping/flattening/manager-runtime assembly into `app/service/framework/ComponentBootstrap.js`
- Lock contract verification was hardened:
  - added `scripts/lock-contract-naming-gate.js`
  - `package.json` validate pipelines now enforce the lock naming gate in local and release modes

## Verification In This Pass
- `node scripts/attachment-contract-gate.js`
- `node scripts/lock-contract-naming-gate.js`
- `node scripts/verify-i18n-completeness.js`
- `node .\\node_modules\\eslint\\bin\\eslint.js "app/service/domain/detail/usecases/AttachmentUploadUseCase.js" "app/service/shared/delta/DeltaFieldMappers.js" "app/infra/adapters/shared/AttachmentRepoRuntime.js" "app/service/framework/ComponentBootstrap.js"`
- `node scripts/build-preload-local.js`

## Closed In This Pass
- Frontend lock contract was normalized to `DB_KEY` as the only canonical root identity on the frontend surface:
  - `app/infra/adapters/LockAdapter.js` no longer sends `ObjectUuid`
  - `app/service/domain/shared/DetailRuntimePayload.js` no longer emits `objectUuid`
  - `app/localService/metadata.xml` lock function imports no longer expose `ObjectUuid`
- Backend lock compatibility was reduced to transport-boundary fallback only:
  - `backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap` no longer publishes `ObjectUuid` on lock function imports
  - `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap` now prefers `DB_KEY` and uses `ObjectUuid` only as legacy fallback when request payloads are old
- Attachment upload boundary was de-overengineered:
  - deleted `app/service/features/detail/runtime/AttachmentValueCodec.js`
  - moved the only remaining base64 conversion into `app/infra/adapters/shared/AttachmentRepoRuntime.js`
  - base64 is no longer a shared productive attachment runtime contract; it is isolated to the gateway upload adapter boundary
- App-shell wrapper sprawl was reduced:
  - deleted `app/service/framework/AppShellCoordinator.js`
  - merged its pass-through init/theme/teardown orchestration into `app/controller/App.controller.js`
- Production gates were tightened:
  - added `scripts/dom-hack-gate.js`
  - `scripts/sap-internal-css-gate.js` now scans the full app styles tree instead of a narrow subset
  - `package.json` validate pipelines now include `private-ui5-selectors-gate` and `dom-hack-gate`
- Attachment gate was aligned to the final architecture:
  - `scripts/attachment-contract-gate.js` now flags `ContentBase64` outside the gateway upload boundary rather than relying on the deleted codec owner
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
- Lock contract is frontend-canonical on `DB_KEY`; backend may still accept legacy `ObjectUuid` only as compatibility ingress.
- Attachment canonical contract remains `DownloadUrl` plus `DocumentHandle`, with `ContentBase64` kept only inside the gateway upload adapter boundary.

## Verification
- `node scripts/broken-import-gate.js`
- `node scripts/detail-contracts-owner-gate.js`
- `node scripts/key-model-gate.js`
- `node scripts/metadata-contract-gate.js`
- `node scripts/attachment-contract-gate.js`
- `node scripts/proxy-constants-gate.js`
- `node scripts/sap-internal-css-gate.js`
- `node scripts/dom-hack-gate.js`
- `npm.cmd run lint:js`
- `npm.cmd run lint:css`
- `npm.cmd run validate:local`
- `npm.cmd run build`

## Environment-Limited Checks
- Local browser click-through flow was not completed because `npm.cmd run start` cannot resolve SAPUI5 framework packages in this environment:
  - missing `@sapui5/distribution-metadata@1.71.70`
- Productive SAPUI5 serve/build was therefore not available for interactive Playwright verification in this pass.
# Final Production Readiness Delta

- Canonical lock and copy contract now uses `DB_KEY` on the frontend and metadata surface. `SourceUuid` was removed from the active frontend copy flow and kept only as backend/mock compatibility fallback where legacy transport still exists.
- Lock naming gate now validates `CopyChecklist` together with acquire/heartbeat/release so contract drift is flagged before release packaging.
- Mock Gateway regression coverage now asserts `CopyChecklist` works with the canonical `DB_KEY` query parameter.
- Attachment productive read contract remains canonical on `DownloadUrl` and `DocumentHandle`. Base64 is still confined to the current upload transport boundary in `AttachmentRepoRuntime` and mock backend create handling; this remains a controlled residual risk, not a frontend domain contract.
