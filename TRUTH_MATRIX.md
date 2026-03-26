# Truth Matrix

- Attachment productive contract: `DownloadUrl` / `DocumentHandle` are canonical; `Value` is no longer a productive default.
- Navigation canonical keys: `activeObjectId` and `selectedId` are working keys; `RootId` is boundary-only.
- Message key ownership: `MessageKeyConstants.js` is the frontend owner.
- Message code ownership: `MessageCodeConstants.js` and `zif_zodata_message_codes.intf.abap` own machine-readable codes.
- Backend text ownership: `zif_zodata_message_texts.intf.abap` owns human-readable backend texts.
- Shell message keys no longer use a separate shell proxy owner.
