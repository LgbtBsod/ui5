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

## Binary key policy

- `RootKey`, `ParentKey`, and any DB checklist key are treated as RAW16/`Edm.Binary`
- frontend path builders must serialize these keys through the OData typed-literal helper, never by manual string concatenation
- `AttachmentKey` stays a string key and must not be mixed with checklist binary keys
- productive SAP Gateway and mock Gateway must expose the same binary-key typing in metadata

## LastChangeSet

- resource: `LastChangeSet(RootKey=<BINARY_LITERAL>)`
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

- `ChecklistRootSet(<BINARY_LITERAL>)`
- `ChecklistBasicInfoSet?$filter=RootKey eq <BINARY_LITERAL>`
- `ChecklistCheckSet?$filter=RootKey eq <BINARY_LITERAL>`
- `ChecklistBarrierSet?$filter=RootKey eq <BINARY_LITERAL>`
- `AttachmentSet?$filter=RootKey eq <BINARY_LITERAL>`
- `AttachmentSet(AttachmentKey='<ATTACHMENT_KEY>')` when the UI opens a stored binary through `Value`

## Detail update and save composition

Mutating detail flows stay on dedicated resources/functions:

- `CreateChecklist`
- `SaveChanges`
- `AutoSave`
- `SetChecklistStatus`
- `CopyChecklist`
- `ChecklistRootSet(<BINARY_LITERAL>)` for delete

## Attachment save contract

- attachments are staged locally in the detail draft until explicit `CreateChecklist` or `SaveChanges`
- no separate frontend media upload transport is used
- attachment rows travel inside the same OData V2 payload as the rest of the save/create contract

### Expected attachment request row

- `Key`
- `RootKey`
- `ParentKey`
- `FolderKey`
- `CategoryKey`
- `Type`
- `FileName`
- `Name`
- `MimeType`
- `Description`
- `FileSize`
- `FileSizeContent`
- `Value`

### Attachment field semantics

- `CategoryKey` and `Type` stay aligned to the attachment-type dictionary seam
- `RootKey` / `ParentKey` identify the owning checklist and stay `Edm.Binary`
- `Value` is the base64-encoded binary payload carried as OData `Edm.Binary`
- `FileSize` and `FileSizeContent` stay aligned to the decoded binary length

### Productive adaptation boundary

- if productive SAP Gateway uses different attachment payload field names, adapt only at `app/infra/adapters/ODataChecklistRepoAdapter.js`
- do not reintroduce a separate REST/media upload fallback path

## Denied and failure response behavior

- permission denial returns permission-seam semantics, not business payload
- detail/update/export failures remain explicit failures
- productive divergence is adapted at the adapter boundary, not in app workflow code
