# Owner Matrix

- Transport owner: backend Gateway/service layer
- Attachment owner: `app/service/features/detail/runtime/DetailAttachmentOpenRuntime.js`
- Key normalization owner: `app/infra/adapters/shared/ODataEntityContracts.js`
- Frontend message-key owner: `app/constants/MessageKeyConstants.js`
- Frontend message-code owner: `app/constants/MessageCodeConstants.js`
- Backend message-code owner: `backend/sap_backend/src/zif_zodata_message_codes.intf.abap`
- Backend message-text owner: `backend/sap_backend/src/zif_zodata_message_texts.intf.abap`
- Component lifecycle owner: `app/service/runtime/component/ComponentLifecycleRuntime.js`
- Component lock/listener owner: `app/service/runtime/component/ComponentLockEventsRuntime.js`
- Navigation owner: `app/infra/navigation/WorkspaceRouteNavigation.js`
