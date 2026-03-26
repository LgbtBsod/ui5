# Truth Matrix

| Concern | Canonical truth |
| --- | --- |
| Root entity key | `DB_KEY` |
| Root entity parent field | Absent |
| Child entity own key | `DB_KEY` |
| Child entity parent field | `PARENT_KEY` |
| Root filter alias compatibility | Boundary-only helper in `backend/mock_gateway/api/gateway_canonical_api.py` |
| Child parent compatibility | `PARENT_KEY` only; child `DB_KEY` is not accepted as parent fallback anymore |
| Frontend i18n owner | `app/constants/MessageKeyConstants.js` |
| Frontend machine-readable code owner | `app/constants/MessageCodeConstants.js` |
| Detail domain/workflow constants owner | `app/constants/DetailContracts.js` |
| Attachment canonical fields | `AttachmentKey`, `DB_KEY`, `PARENT_KEY`, `DownloadUrl`, `DocumentHandle` |
| Attachment upload-only payload field | `ContentBase64` |
| Attachment save delta owner | `app/service/shared/delta/DeltaFieldMappers.js` without inline `value` |
| Metadata source of truth | `app/localService/metadata.xml` |
| Mock metadata source | `backend/mock_gateway/services/metadata_builder.py` reading local metadata |
| Productive ABAP root key boundary | `DB_KEY` with `ObjectUuid` compatibility fallback |
| UI5 core access owner | direct `sap/ui/core/Core` usage in consuming owners; no extra facade |
| Component bootstrap runtime-option owner | `app/service/framework/ComponentBootstrap.js` and `app/service/runtime/component/ComponentLifecycleRuntime.js` |
| Navigation intent runtime owner | `app/service/runtime/component/ComponentLifecycleRuntime.js` |
| Internal component runtime state owner | `app/service/runtime/component/ComponentModelInitRuntime.js` |
| Search action busy owner | `app/controller/search/SearchActionBehavior.js` |
| Detail validation summary owner | `app/controller/detail/DetailValidationSummaryRuntime.js` |
| Detail controller runtime owner | `app/controller/detail/DetailControllerRuntime.js` |
| Navigation default handler owner | `app/service/framework/behavior/NavigationDefaultHandlers.js` |
| UI decision default handler owner | `app/service/framework/behavior/UiDecisionDefaultHandlers.js` |
| UI decision dispatch owner | direct `UiDecisionDefaultHandlers.handlers.*` calls from consumers |
| Deleted legacy bootstrap wrappers | `ComponentMainServiceRuntime.js`, `ComponentModelBootstrap.js`, `ComponentGuardedSaveRuntime.js` |
| Deleted thin runtime-option wrapper | `ComponentRuntimeOptionsFactory.js` |
| Deleted thin bootstrap dependency wrapper | `ComponentBootstrapDependencyBuilder.js` |
| Deleted thin UI5 core facade | `Ui5RuntimeFacade.js` |
| Deleted thin navigation wrapper | `ComponentNavigationRuntime.js` |
| Deleted thin runtime-state wrapper | `ComponentInternalRuntimeState.js` |
| Deleted thin search busy wrapper | `ControllerActionBusyRuntime.js` |
| Deleted thin detail validation helper | `DetailValidationHelperRuntime.js` |
| Deleted thin detail dialog helper | `DetailActionDialogRuntime.js` |
| Deleted thin adaptive viewport helper | `DetailAdaptiveViewportRuntime.js` |
| Deleted thin navigation behavior helper | `NavigationBehaviorHelpers.js` |
| Deleted thin UI decision helper | `UiDecisionBehaviorHelpers.js` |
| Deleted thin UI decision coordinator | `UiDecisionCoordinator.js` |
| Deleted legacy context wrappers | `CtxModelResolver.js`, `CtxCacheRuntimeFactory.js` |
- Frontend root identity owner: `DB_KEY`
- Frontend lock request owner: [app/infra/adapters/LockAdapter.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/LockAdapter.js)
- Backend lock compatibility fallback owner: [zcl_zodata_dpc_ext.clas.abap](C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap)
- Attachment upload compatibility boundary owner: [AttachmentRepoRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/shared/AttachmentRepoRuntime.js)
# Truth Matrix Delta

| Area | Canonical owner | Compatibility boundary | Final state |
| --- | --- | --- | --- |
| Root identity | `DB_KEY` (`Edm.Binary`) | Backend may still accept `ObjectUuid` in DPC fallback | Frontend canonical |
| Child relation | `PARENT_KEY` (`Edm.Binary`) | None on frontend | Canonical |
| Copy checklist function import | `DB_KEY` | Mock/backend may still read `SourceUuid` as fallback | Canonicalized on metadata/frontend |
| Lock function imports | `DB_KEY` + `SessionGuid` | Backend DPC fallback may still read `ObjectUuid` | Canonicalized on metadata/frontend |
| Attachment persisted read | `DownloadUrl` + `DocumentHandle` | None | Canonical |
| Attachment upload transport | transient `ContentBase64` boundary only | Mock/backend create boundary | Residual risk |
| Binary OData literal formatting | explicit `binary'HEX'` | None | Canonical frontend adapter boundary |
