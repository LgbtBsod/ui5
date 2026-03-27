# Truth Matrix

| Domain | Final Truth |
| --- | --- |
| Root identity | `DB_KEY` |
| Root parent field | forbidden |
| Child identity | own `DB_KEY` |
| Child to root relation | `PARENT_KEY` |
| Frontend canonical detail/lock key | `dbKey` |
| Backend compatibility alias | `ObjectUuid` only as narrow ingress fallback |
| Attachment persisted contract | `AttachmentKey`, `DB_KEY`, `PARENT_KEY`, `DownloadUrl`, `DocumentHandle` |
| Attachment metadata stream capability | `Attachment` entity has `HasStream="true"` |
| Attachment upload transport | media POST body to `AttachmentSet` |
| Attachment base64 JSON payload on aggregate save | forbidden |
| Attachment read/open path | `DownloadUrl` / `DocumentHandle` / `$value` |
| Frontend message-key owner | `app/constants/MessageKeyConstants.js` |
| Frontend message-code owner | `app/constants/MessageCodeConstants.js` |
| Backend message-code owner | `backend/sap_backend/src/zif_zodata_message_codes.intf.abap` |
| Backend human-readable text owner | `backend/sap_backend/src/zcl_zodata_message_texts.clas.abap` backed by message class `ZPCCT_API` |
| UI state model | `JSONModel` |
| Transport model | `ODataModel` |
| Checklist mutations | function imports |
| Attachment binary transfer | media upload, not aggregate JSON save |
| Removed wrapper owners | `ControllerRouteRuntime`, `FeedbackCoordinator`, `SearchCommandPolicy`, `DetailCommandPolicy` |
| Deleted-wrapper governance | controller util allowlist no longer references deleted wrapper owners |
| CSS governance | remaining internal `.sap*` selectors only through documented whitelist |
| DOM governance owner | `scripts/dom-hack-allowlist.json` with bounded quarantine reasons |
| Gate status | attachment, lock, wrapper, DOM, CSS, raw-text, residual-cleanup gates pass |
