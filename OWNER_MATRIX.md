# Owner Matrix

| Capability | Owner |
| --- | --- |
| Root key owner | [`DetailRuntimePayload.js`](/Users/lgbtb/Desktop/ui5/app/service/domain/shared/DetailRuntimePayload.js) and frontend adapters on canonical `DB_KEY` |
| Child relation owner | [`ODataEntityContracts.js`](/Users/lgbtb/Desktop/ui5/app/infra/adapters/shared/ODataEntityContracts.js) on canonical `PARENT_KEY` |
| Attachment owner | [`AttachmentRepoRuntime.js`](/Users/lgbtb/Desktop/ui5/app/infra/adapters/shared/AttachmentRepoRuntime.js) for transport + metadata surface in `Attachment` entity |
| Attachment upload transport owner | [`GatewayClient.js`](/Users/lgbtb/Desktop/ui5/app/service/backend/GatewayClient.js) `uploadMedia(...)` |
| Attachment save-path enforcement owner | [`gateway_canonical_api.py`](/Users/lgbtb/Desktop/ui5/backend/mock_gateway/api/gateway_canonical_api.py) rejects base64 aggregate-save uploads |
| Frontend message-key owner | [`MessageKeyConstants.js`](/Users/lgbtb/Desktop/ui5/app/constants/MessageKeyConstants.js) |
| Frontend message-code owner | [`MessageCodeConstants.js`](/Users/lgbtb/Desktop/ui5/app/constants/MessageCodeConstants.js) |
| Backend message-code owner | [`zif_zodata_message_codes.intf.abap`](/Users/lgbtb/Desktop/ui5/backend/sap_backend/src/zif_zodata_message_codes.intf.abap) |
| Backend human-readable text owner | [`zcl_zodata_message_texts.clas.abap`](/Users/lgbtb/Desktop/ui5/backend/sap_backend/src/zcl_zodata_message_texts.clas.abap) |
| Detail domain constants owner | [`DetailContracts.js`](/Users/lgbtb/Desktop/ui5/app/constants/DetailContracts.js) |
| UI constants owner | [`ModelConstants.js`](/Users/lgbtb/Desktop/ui5/app/constants/ModelConstants.js) plus UI contract owners |
| Gateway contract constants owner | [`GatewayContractConstants.js`](/Users/lgbtb/Desktop/ui5/app/constants/GatewayContractConstants.js) |
| App shell owner | [`App.controller.js`](/Users/lgbtb/Desktop/ui5/app/controller/App.controller.js) |
| Search owner | [`Search.controller.js`](/Users/lgbtb/Desktop/ui5/app/controller/Search.controller.js) including controller-owned search command dispatch |
| Detail owner | [`DetailControllerRuntime.js`](/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailControllerRuntime.js) including controller-owned detail command dispatch |
| Analytics owner | [`Analytics.controller.js`](/Users/lgbtb/Desktop/ui5/app/controller/Analytics.controller.js) |
| Framework owner | real runtime/handler owners only; removed thin wrappers must not return |
| Component lifecycle owner | [`ComponentLifecycleRuntime.js`](/Users/lgbtb/Desktop/ui5/app/service/runtime/component/ComponentLifecycleRuntime.js) |
| CSS legacy quarantine owner | [`sap-internal-css-allowlist.json`](/Users/lgbtb/Desktop/ui5/scripts/sap-internal-css-allowlist.json) |
| DOM legacy quarantine owner | [`dom-hack-allowlist.json`](/Users/lgbtb/Desktop/ui5/scripts/dom-hack-allowlist.json) |
