# Project Structure Map

## Active Production Owners
- `app/service/backend/GatewayClient.js`
  - canonical frontend transport boundary
  - function imports
  - media upload transport for `AttachmentSet`
- `app/infra/adapters/shared/AttachmentRepoRuntime.js`
  - canonical attachment repository
  - metadata load/delete
  - media upload entrypoint
- `app/service/domain/shared/DetailRuntimePayload.js`
  - canonical frontend `DB_KEY` resolution for detail/lock flows
- `app/controller/App.controller.js`
  - shell lifecycle and shell interaction owner
- `app/controller/Search.controller.js`
  - search intent and search toolbar/view-state owner
- `app/controller/Analytics.controller.js`
  - analytics route and interaction owner
- `backend/mock_gateway/api/gateway_canonical_api.py`
  - mock Gateway canonical OData contract
  - media-compatible attachment mock upload/read behavior
  - rejects base64 upload on aggregate save path
- `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap`
  - productive backend contract execution layer
- `backend/sap_backend/src/zcl_zodata_message_texts.clas.abap`
  - backend human-readable text provider contract
- `scripts/*.js`
  - production-readiness gates

## Governance / Quarantine Owners
- `scripts/sap-internal-css-allowlist.json`
  - explicit whitelist for remaining legacy `.sap*` selector files
- `scripts/dom-hack-allowlist.json`
  - explicit whitelist for remaining DOM-boundary owners that still require browser-level APIs
- `CSS_DOM_VIOLATIONS.md`
  - human-readable explanation of why each legacy CSS bucket is still quarantined

## Final Attachment Path
1. UI stages file in JSONModel only.
2. `AttachmentRepoRuntime` uploads binary body through `GatewayClient.uploadMedia(...)`.
3. `AttachmentSet` returns canonical attachment metadata.
4. `SaveChanges` carries metadata/delete only and rejects base64 binary payloads.
5. Detail save sync reloads attachment metadata and clears transient staging state.

## Final Lock / Key Path
1. frontend resolves current root identity as `dbKey`
2. metadata publishes `DB_KEY`
3. adapters call lock/copy using `DB_KEY`
4. backend may still accept `ObjectUuid` only as ingress compatibility fallback
5. shell state stores current root identity only as `currentChecklistDbKey`

## Removed Thin Wrappers
- `app/service/framework/ControllerRouteRuntime.js`
- `app/service/framework/FeedbackCoordinator.js`
- `app/controller/search/SearchCommandPolicy.js`
- `app/controller/detail/DetailCommandPolicy.js`

## Governance Cleanup In This Pass
- removed stale deleted-wrapper references from `scripts/internal/controller-util-allowlist.json`
- removed detail info-card raw i18n fallback copy; labels and tooltips now resolve from bundle keys only
