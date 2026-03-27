# Mock Gateway OData Contract

This document defines the frontend adapter-boundary contract for the mock Gateway and the productive SAP Gateway rollout target.

Canonical service root: `/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/`
Frontend manifest alias: `mainService`
Productive UI5 baseline target: `1.71.28`

## Productive create-permission seam rules:

## Mock contour hardening rules

- `PCCT_PROFILE=local` is the only profile where mock identity and startup seeding are allowed by default.
- Any non-local profile must treat the backend as a hardened parity contour:
  - `ALLOW_MOCK_USER_HEADER = False`
  - `AUTO_MUTATE_SCHEMA_ON_STARTUP = False`
  - `AUTO_SEED_STARTUP_DATA = False`
- request body logging is disabled by default and must only be re-enabled explicitly with `PCCT_LOG_REQUEST_BODIES=1` for short-lived local diagnostics.

- resource: `ChecklistCreatePermissionSet('CURRENT')`
- request identity semantics: the frontend always asks for `CURRENT`
- response entity identity also stays `DB_KEY='CURRENT'`
- no fallback through `CurrentUserSet` or any unrelated resource
- frontend adaptation point: `app/infra/adapters/ODataChecklistRepoAdapter.js`
- permission normalization point: `normalizePermissionResponse()`

### Required permission DTO fields

- `DB_KEY`
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

- `DB_KEY`, `PARENT_KEY`, and any DB checklist key are treated as RAW16/`Edm.Binary`
- frontend path builders must serialize these keys through the OData typed-literal helper, never by manual string concatenation
- `AttachmentKey` stays a string key and must not be mixed with checklist binary keys
- productive SAP Gateway and mock Gateway must expose the same binary-key typing in metadata

## LastChangeSet

- resource: `LastChangeSet(DB_KEY=<BINARY_LITERAL>)`
- purpose: cache freshness and conflict validation
- expected behavior: return the latest aggregate change marker for the requested root key

## ReportExport

- resource: `ReportExport`
- `SelectionMode='selected'` means `DB_KEYs` only
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
- `ChecklistBasicInfoSet?$filter=DB_KEY eq <BINARY_LITERAL>`
- `ChecklistCheckSet?$filter=PARENT_KEY eq <BINARY_LITERAL>`
- `ChecklistBarrierSet?$filter=PARENT_KEY eq <BINARY_LITERAL>`
- `AttachmentSet?$filter=PARENT_KEY eq <BINARY_LITERAL>`
- `AttachmentSet(AttachmentKey='<ATTACHMENT_KEY>')` when the UI opens a stored binary through `DownloadUrl` / `DocumentHandle`

## Detail update and save composition

Mutating detail flows stay on dedicated resources/functions:

- `CreateChecklist`
- `SaveChanges`
- `AutoSave`
- `SetChecklistStatus`
- `CopyChecklist`
- `ChecklistRootSet(<BINARY_LITERAL>)` for delete

## Attachment contract

- persisted attachment upload uses only media upload to `AttachmentSet`
- `SaveChanges` and `CreateChecklist` must not carry productive base64 attachment payloads
- mock Gateway rejects save-time base64 via `ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN`
- attachment binaries are opened only through `AttachmentSet('<AttachmentKey>')/$value`, `DownloadUrl`, or `DocumentHandle`

### Required media upload headers

- `X-DB-Key`
- `X-Parent-Key`
- `X-Folder-Key`
- `X-Category-Key`
- `X-Description`
- `X-File-Name`
- `Slug`

### Attachment metadata semantics

- `DB_KEY` is the persisted checklist identity
- `PARENT_KEY` is the persisted child relation and matches the owning checklist for root attachments
- `CategoryKey` and `Type` stay aligned to the attachment-type dictionary seam
- `DownloadUrl` / `DocumentHandle` are the productive binary access seam
- canonical metadata must not expose `Value`

### Productive adaptation boundary

- if productive SAP Gateway varies by field name or header naming, adapt only at the OData/frontend adapter boundary
- do not reintroduce parallel JSON/base64 save transport

## Denied and failure response behavior

- permission denial returns permission-seam semantics, not business payload
- detail/update/export failures remain explicit failures
- productive divergence is adapted at the adapter boundary, not in app workflow code
