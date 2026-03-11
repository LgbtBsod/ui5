# mock_gateway OData V2 (Gateway-like)

Canonical service root:
`/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/`

## Run
```bash
cd /workspace/ui5/backend/mock_gateway
uvicorn main:app --reload --host 0.0.0.0 --port 8000
```

UI5 expects the mock backend on `http://localhost:8000`.

For reproducible local startup and smoke steps from this repo, use `docs/LOCAL_VALIDATION.md`.

## Canonical entity sets

- `ChecklistSearchSet`
- `ChecklistRootSet`
- `ChecklistBasicInfoSet`
- `ChecklistCheckSet`
- `ChecklistBarrierSet`
- `DictionaryItemSet`
- `PersonVHSet`
- `LastChangeSet`
- `LockStatusSet`
- `ChecklistPermissionSet`
- `ChecklistCreatePermissionSet`
- `CurrentUserSet`
- `RuntimeSettingsSet`
- `SimpleAnalyticalSet`
- `WorkflowAnalyticsBreakdownSet`
- `AnalyticsRefreshStateSet`
- `AttachmentFolderSet`
- `AttachmentSet`

## CSRF fetch
```bash
curl -i "http://localhost:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/ChecklistSearchSet?$top=1" -H "X-CSRF-Token: Fetch"
```

## Search paging/filter
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/ChecklistSearchSet?$top=20&$skip=0&$inlinecount=allpages&$filter=substringof('LPC',LpcText)%20and%20Status%20eq%20'DRAFT'&$orderby=ChangedOn%20desc"
```

## Runtime settings
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/RuntimeSettingsSet(Key='GLOBAL')"
```

Runtime settings are the canonical source for:
- timer values
- `RequiredFieldsJson`
- `FrontendVariablesJson`
- `UploadPolicyJson`

`DictionaryItemSet` is now limited to reference data domains such as `LPC`, `PROFESSION`, `TIME_ZONE`, `ATF_CAT`.

## Permission contract

`ChecklistPermissionSet` uses ACTVT semantics:
- `01` create
- `02` change
- `03` display
- `06` delete

Examples:
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/ChecklistPermissionSet('<ROOT_KEY_HEX>')"
curl "http://localhost:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/ChecklistCreatePermissionSet('CURRENT')"
```

Productive create-permission seam rules:
- request identity is always `ChecklistCreatePermissionSet('CURRENT')`
- response entity identity also stays `RootKey='CURRENT'`
- payload shape stays on the shared `ChecklistPermission` DTO: `GrantedOperations`, `CanCreate`, `CanView`, `CanEdit`, `CanDelete`, `ReasonCode`, `Message`
- denied create permission returns the same DTO shape with empty `GrantedOperations`, `CanCreate=false`, and a non-permissive `ReasonCode`; it must not expose checklist payloads
- transport or backend failures stay transport/backend failures; the frontend only normalizes successful DTO payloads in `normalizePermissionResponse()` and must not synthesize permissive fallback state
- frontend adapter alignment point is `app/infra/adapters/ODataChecklistRepoAdapter.js` via `normalizePermissionResponse()`

The frontend must not send `Uname` for create/save/copy/lock flows. The mock backend resolves identity server-side.
For local mock tests only, identity can be overridden with the `X-Mock-User` request header.

## Analytics contract

`SimpleAnalyticalSet` accepts year/source selectors.

`WorkflowAnalyticsBreakdownSet` is strict and requires `$filter`:
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/WorkflowAnalyticsBreakdownSet?$filter=SelectedYear%20eq%202026%20and%20Source%20eq%20'ALL'"
```

Do not rely on plain query params for breakdown requests.

## Additional rollout alignment points

- `LastChangeSet(RootKey='<ROOT_KEY_HEX>')`
  - returns only freshness data for optimistic reload checks
  - frontend should treat it as change-detection metadata, not as a detail payload source
- Checklist read path
  - frontend detail load composes `ChecklistRootSet`, `ChecklistBasicInfoSet`, `ChecklistCheckSet`, `ChecklistBarrierSet`
  - productive Gateway differences should be adapted in `app/infra/adapters/ODataChecklistRepoAdapter.js`, not in detail use cases
- Save/update path
  - `CreateChecklist`, `SaveChanges`, `AutoSave` return server snapshot hints; the frontend still rehydrates to canonical `selected` and `snapshot`
- Permission-denied path
  - denied permission must stay DTO-only and must not reveal business payload, cache payload, or unrelated fallback resources
- Analytics breakdown path
  - `WorkflowAnalyticsBreakdownSet` stays filter-driven and route-lazy; productive differences belong at the adapter/request boundary, not in search readiness logic

## Export contract

Selected export:
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/ReportExport" \
  -H "Content-Type: application/json" -H "X-CSRF-Token: <token>" \
  -d '{"Entity":"screen","SelectionMode":"selected","RootKeys":["<ROOT_KEY_1>","<ROOT_KEY_2>"],"Limit":200000}'
```

Export all by search contract:
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/ReportExport" \
  -H "Content-Type: application/json" -H "X-CSRF-Token: <token>" \
  -d '{"Entity":"screen","SelectionMode":"all","Limit":200000,"SearchContract":{"filterId":"","filterDateFrom":"2026-01-01","filterDateTo":"2026-12-31","filterLocationKey":"","filterLpc":"","filterProfession":"","filterStatus":"","searchMode":"EXACT","checksSegment":"ALL","barriersSegment":"ALL"}}'
```

Contract notes:

- `SelectionMode='selected'` must use `RootKeys`
- `SelectionMode='all'` must use `SearchContract`
- `filterLocationKey` is part of search membership and must remain in the export-all-found contract
- export scope must remain independent from visible table rows, `$top`, `searchFetchLimit`, and `growingPageSize`

## Batch sample
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/$batch" \
  -H "Content-Type: multipart/mixed; boundary=batch_123" -H "X-CSRF-Token: <token>" \
  --data-binary $'--batch_123\r\nContent-Type: application/http\r\nContent-Transfer-Encoding: binary\r\n\r\nGET /sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/LockStatusSet(\'<ROOT_KEY_HEX>\') HTTP/1.1\r\n\r\n--batch_123--\r\n'
```
