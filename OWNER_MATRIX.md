# Owner Matrix

| Capability | Owner |
| --- | --- |
| Root key owner | Canonical `DB_KEY` across frontend metadata, mock gateway, and productive backend boundary |
| Child parent-key owner | Canonical `PARENT_KEY` for child entity filters, payloads, and metadata |
| Attachment owner | `Attachment` entity contract with `AttachmentKey`, `DB_KEY`, `PARENT_KEY`, `DownloadUrl`, `DocumentHandle` |
| Frontend message-key owner | `app/constants/MessageKeyConstants.js` |
| Frontend message-code owner | `app/constants/MessageCodeConstants.js` |
| Backend message-code owner | `backend/sap_backend/src/zif_zodata_message_codes.intf.abap` |
| Backend message-text owner | `backend/sap_backend/src/zif_zodata_message_texts.intf.abap` |
| Detail domain constants owner | `app/constants/DetailContracts.js` |
| App shell owner | `app/controller/App.controller.js` |
| Search owner | `app/controller/Search.controller.js` |
| Analytics owner | `app/controller/Analytics.controller.js` |
| Framework bootstrap owner | `app/service/framework/ComponentBootstrap.js` |
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
