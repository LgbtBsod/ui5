# Mock Gateway OData Contract

This document defines the adapter-boundary contract that the frontend expects from the SAP Gateway seam exposed by the mock backend.

## Productive create-permission seam rules:

- resource name: `ChecklistCreatePermissionSet('CURRENT')`
- request identity semantics: the frontend always requests `CURRENT`
- response entity identity also stays `RootKey='CURRENT'`
- no fallback through `CurrentUserSet` or any unrelated resource
- frontend adaptation point stays in `app/infra/adapters/ODataChecklistRepoAdapter.js`
- permission normalization stays in `normalizePermissionResponse()`

## Create-permission response DTO

The create-permission payload is expected to provide these fields:

- `RootKey`
- `UserId`
- `GrantedOperations`
- `CanCreate`
- `CanView`
- `CanEdit`
- `CanDelete`
- `ReasonCode`
- `Message`

`GrantedOperations` keeps SAP activity codes:

- `01` create
- `02` change
- `03` display
- `06` delete

## Denied semantics

- denied permission stays non-permissive
- denied permission must not leak checklist business payload
- `ReasonCode` and `Message` explain denial at the permission seam
- frontend must not infer permission from unrelated resources

## Transport and failure semantics

- transport failures stay transport failures
- malformed or missing permission payloads are normalized at the repo adapter boundary
- app workflows must not grow fallback branches for Gateway divergence

## Other productive seams

### LastChangeSet

- resource: `LastChangeSet('<ROOT_KEY>')`
- freshness source for cache validation and conflict detection
- productive Gateway must return the latest aggregate change marker for the requested root key

### ReportExport all-found contract

- resource: `ReportExport`
- `SelectionMode='selected'` means `RootKeys` only
- `SelectionMode='all'` means `SearchContract` only
- `Limit` is independent from visible rows, paging, and growing size

Expected `SearchContract` membership fields:

- `filterId`
- `filterDateFrom`
- `filterDateTo`
- `filterLocationKey`
- `filterLpc`
- `filterProfession`
- `filterStatus`
- `searchMode`
- `checksSegment`
- `barriersSegment`

### Analytics breakdown

- analytics breakdown requests stay filter-driven
- productive Gateway must honor strict filter semantics instead of returning unfiltered aggregate data

### Detail read composition

Frontend detail composition expects:

- `ChecklistRootSet('<ROOT_KEY>')`
- `ChecklistBasicInfoSet?$filter=RootKey eq '<ROOT_KEY>'`
- `ChecklistCheckSet?$filter=RootKey eq '<ROOT_KEY>'`
- `ChecklistBarrierSet?$filter=RootKey eq '<ROOT_KEY>'`

### Detail update/save composition

Mutating detail flows stay on dedicated resources/functions:

- `CreateChecklist`
- `SaveChanges`
- `AutoSave`
- `SetChecklistStatus`
- `CopyChecklist`
- `ChecklistRootSet('<ROOT_KEY>')` for delete

### Denied and failure response behavior

- permission denial returns permission DTO semantics, not business payload
- transport failures and backend errors remain explicit failures
- the frontend must adapt incompatible Gateway payloads only at the adapter seam
