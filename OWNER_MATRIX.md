# Owner Matrix

| Capability | Owner |
| --- | --- |
| Root key owner | Canonical `DB_KEY` across frontend metadata, mock gateway, and productive backend boundary |
| Child parent-key owner | Canonical `PARENT_KEY` for child entity filters, payloads, and metadata |
| Attachment owner | `Attachment` entity contract with `AttachmentKey`, `DB_KEY`, `PARENT_KEY`, `DownloadUrl`, `DocumentHandle` |
| Attachment upload staging owner | `app/infra/adapters/shared/AttachmentRepoRuntime.js` for transient upload payload conversion only |
| Frontend message-key owner | `app/constants/MessageKeyConstants.js` |
| Frontend message-code owner | `app/constants/MessageCodeConstants.js` |
| Backend message-code owner | `backend/sap_backend/src/zif_zodata_message_codes.intf.abap` |
| Backend message-text owner | `backend/sap_backend/src/zif_zodata_message_texts.intf.abap` |
| Detail domain constants owner | `app/constants/DetailContracts.js` |
| App shell owner | `app/controller/App.controller.js` |
| Search owner | `app/controller/Search.controller.js` |
| Analytics owner | `app/controller/Analytics.controller.js` |
| Framework bootstrap owner | `app/service/framework/ComponentBootstrap.js` |
| Lock contract naming gate owner | `scripts/lock-contract-naming-gate.js` |
| UI5 core facade owner | removed; consumers call `sap/ui/core/Core` directly |
| Framework context owner | `app/service/framework/CtxRuntimeFactory.js` |
| Component lifecycle owner | `app/service/runtime/component/ComponentLifecycleRuntime.js` |
| Runtime option owner | `app/service/framework/ComponentBootstrap.js` and `app/service/runtime/component/ComponentLifecycleRuntime.js` |
| Navigation intent runtime owner | `app/service/runtime/component/ComponentLifecycleRuntime.js` |
| Internal runtime state owner | `app/service/runtime/component/ComponentModelInitRuntime.js` |
| Search action busy owner | `app/controller/search/SearchActionBehavior.js` |
| Detail validation summary owner | `app/controller/detail/DetailValidationSummaryRuntime.js` |
| Detail controller runtime owner | `app/controller/detail/DetailControllerRuntime.js` |
| Navigation default handler owner | `app/service/framework/behavior/NavigationDefaultHandlers.js` |
| UI decision default handler owner | `app/service/framework/behavior/UiDecisionDefaultHandlers.js` |
| UI decision dispatch owner | direct consumer calls into `UiDecisionDefaultHandlers.handlers.*` |
| Save guard owner | `app/service/runtime/component/ComponentSaveGuardRuntime.js` |
- root key owner: `app/service/domain/shared/DetailRuntimePayload.js` + `app/infra/adapters/LockAdapter.js` using canonical `DB_KEY`
- child relation owner: `app/infra/adapters/shared/ODataChecklistPayloadMapper.js` on `PARENT_KEY`
- attachment owner: `app/infra/adapters/shared/AttachmentRepoRuntime.js` for upload ingress, `DownloadUrl`/`DocumentHandle` for persisted read/open
- backend human-readable text owner: `backend/sap_backend/src/zcl_zodata_message_texts.clas.abap`
- app shell owner: `app/controller/App.controller.js`
# Owner Matrix Delta

- Root key owner: `app/constants/GatewayContractConstants.js` plus `app/localService/metadata.xml` for service contract exposure.
- Binary key transport owner: `app/service/shared/ODataKeyNormalizer.js`, `app/infra/adapters/shared/ODataEntityContracts.js`, `app/localService/metadata.xml`, `backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap`.
- Binary-safe adapter boundary owner: `app/service/shared/ODataKeyNormalizer.js` plus `app/infra/adapters/shared/ODataAdapterUtils.js`.
- Copy/lock canonical transport owner: `app/infra/adapters/shared/ODataChecklistMutationRuntime.js`, `app/infra/adapters/LockAdapter.js`, `scripts/lock-contract-naming-gate.js`.
- Backend compatibility owner: `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap` and `backend/mock_gateway/api/gateway_canonical_api.py`.
- Backend human-readable text owner: `backend/sap_backend/src/zcl_zodata_message_texts.clas.abap` remains the central non-interface text provider.
