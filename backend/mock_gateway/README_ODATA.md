# Mock Gateway OData Contract

This document defines the frontend adapter-boundary contract for the mock Gateway and the productive SAP Gateway rollout target.

## Productive create-permission seam rules:

- resource: `ChecklistCreatePermissionSet('CURRENT')`
- request identity semantics: the frontend always asks for `CURRENT`
- response entity identity also stays `RootKey='CURRENT'`
- no fallback through `CurrentUserSet` or any unrelated resource
- frontend adaptation point: `app/infra/adapters/ODataChecklistRepoAdapter.js`
- permission normalization point: `normalizePermissionResponse()`

### Required permission DTO fields

- `RootKey`
- `UserId`
- `GrantedOperations`
- `CanCreate`
- `CanView`
- `CanEdit`
- `CanDelete`
- `ReasonCode`
- `Message`

`GrantedOperations` keeps SAP activity values:

- `01` create
- `02` change
- `03` display
- `06` delete

### Denied semantics

- denied permission stays non-permissive
- denied permission must not leak business payload
- `ReasonCode` and `Message` explain the denial at the permission seam

### Transport and failure semantics

- transport/backend failures stay explicit failures
- incompatible permission payloads are adapted only at `normalizePermissionResponse()`
- app workflows must not grow fallback branches for Gateway divergence

## LastChangeSet

- resource: `LastChangeSet('<ROOT_KEY>')`
- purpose: cache freshness and conflict validation
- expected behavior: return the latest aggregate change marker for the requested root key

## ReportExport

- resource: `ReportExport`
- `SelectionMode='selected'` means `RootKeys` only
- `SelectionMode='all'` means `SearchContract` only
- `Limit` is independent from visible rows, paging, and growing page size

### SearchContract membership fields

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

## Analytics breakdown

- analytics breakdown requests stay strictly filter-driven
- productive Gateway must not return unfiltered aggregate breakdown data for filtered requests

## Detail read composition

Frontend detail hydration expects these resources:

- `ChecklistRootSet('<ROOT_KEY>')`
- `ChecklistBasicInfoSet?$filter=RootKey eq '<ROOT_KEY>'`
- `ChecklistCheckSet?$filter=RootKey eq '<ROOT_KEY>'`
- `ChecklistBarrierSet?$filter=RootKey eq '<ROOT_KEY>'`

## Detail update and save composition

Mutating detail flows stay on dedicated resources/functions:

- `CreateChecklist`
- `SaveChanges`
- `AutoSave`
- `SetChecklistStatus`
- `CopyChecklist`
- `ChecklistRootSet('<ROOT_KEY>')` for delete

## Denied and failure response behavior

- permission denial returns permission-seam semantics, not business payload
- detail/update/export failures remain explicit failures
- productive divergence is adapted at the adapter boundary, not in app workflow code
