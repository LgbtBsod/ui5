# SAP Backend Readiness For UI5 Frontend

Date: 2026-03-12

Scope: repository audit of `app`, `backend/mock_gateway`, `backend/sap_backend`, `app/localService/metadata.xml`.

## Executive Verdict

Overall status: `PARTIALLY READY / HIGH DELIVERY RISK`.

The frontend already has a stable integration target: one OData V2 service `Z_EHS_PRODUCTION_CONTROL_CKLT_SRV` with a concrete set of entity sets, function imports, lock flows, autosave/save semantics, analytics, dictionaries, permissions, current-user profile, runtime settings, hierarchy lookup, export, and attachment transport inside the same OData payload.

The blocking issue is not the absence of a contract. The blocking issue is that the productive SAP backend visible in the repository implements only a narrow ABAP wrapper layer and does not evidence the full persistence, authorization, CDS/BOPF model, attachment architecture, analytics pipeline, or read-side query layer required by the frontend.

## What The Frontend Expects From SAP Backend

Frontend integration is centered around:

- Search list read: `ChecklistSearchSet`
- Detail composition reads: `ChecklistRootSet`, `ChecklistBasicInfoSet`, `ChecklistCheckSet`, `ChecklistBarrierSet`, `AttachmentSet`
- Reference and helper reads: `DictionaryItemSet`, `PersonVHSet`, `LastChangeSet`, `LockStatusSet`, `ChecklistPermissionSet`, `ChecklistCreatePermissionSet`, `CurrentUserSet`, `RuntimeSettingsSet`
- Analytics reads: `SimpleAnalyticalSet`, `WorkflowAnalyticsBreakdownSet`, `AnalyticsRefreshStateSet`
- Mutations via function imports: `LockAcquire`, `LockHeartbeat`, `LockRelease`, `CreateChecklist`, `CopyChecklist`, `AutoSave`, `SaveChanges`, `SetChecklistStatus`, `AnalyticsRefreshTrigger`, `ReportExport`
- Hierarchy lookup: `GetHierarchy`
- Deletes through entity endpoint: `ChecklistRootSet('<ROOT_KEY>')`
- Attachment delete/open: `AttachmentSet(AttachmentKey='<ATTACHMENT_KEY>')`

Critical behavior expectations from frontend code:

- OData V2 batch is enabled and actively used.
- CSRF token handling must work.
- Save and autosave return server timestamps and `version_number`.
- Lock lifecycle must support acquire, heartbeat, release, status polling, takeover/loss handling.
- Attachment binary is transported as `Edm.Binary Value` inside save/create payload, not through a separate REST upload API.
- Permission responses are authoritative and drive UI mode decisions.
- Analytics breakdown requires filtered server-side aggregation, not client-side post-processing.
- Runtime settings drive heartbeat, polling, autosave, cache, required fields, frontend variables, upload policy.

## Readiness Assessment By Capability

### 1. OData Contract

Status: `Amber`

What is ready:

- `app/localService/metadata.xml` defines a concrete service contract.
- `backend/mock_gateway/tests/test_gateway_contract_frontend_aliases.py` validates the canonical contract the frontend depends on.
- Frontend adapters are already normalized around that contract.

What is missing or risky on productive SAP:

- ABAP `MPC_EXT` and `DPC_EXT` in the repository do not match the full metadata used by the frontend.
- ABAP wrapper code exposes a different naming style in several places (`ObjectUuid`, `Payload`, `MplTree`) while the frontend and mock canonical service use `RootId`, `RootKey`, `GetHierarchy`, `Checklist*Set`, `AttachmentSet`, analytics sets, permissions, runtime settings.
- This means the productive ABAP code visible here is not yet the full service behind the UI contract.

Conclusion:

- The frontend contract is defined.
- The visible productive SAP implementation is not yet proven to implement that contract end-to-end.

### 2. Business Persistence Model

Status: `Red`

What the frontend needs:

- Checklist root aggregate
- Basic info projection
- Check rows
- Barrier rows
- Attachments
- Lock state and lock history
- Last-change markers
- Dictionary/reference values
- Persons/value-help
- Location hierarchy
- Analytics snapshot and breakdown data
- Runtime settings
- User/authorization profile
- Save request ledger or equivalent idempotency/audit trail

What is evidenced:

- Mock backend contains a full local persistence model.
- Productive ABAP code references custom DDIC/BOPF artifacts like `ZSTR_PCCT_SAVECHANGES_RQ`, `ZTAB_PCCT_CHECK_DELTA`, `ZTAB_PCCT_BARRIER_DELTA`, `ZTAB_PCCT_PART_DELTA`, `ZTAB_PCCT_ATTACH_DELTA`, `ZTT_PCCT_MPL_TREE`, `ZSTR_BO_ROOT`, `zif_i_bo_c=>sc_bo_key`.

What is missing:

- No CDS definitions are present in the repository.
- No DDIC table definitions are present in the repository.
- No BOPF BO definition artifacts are present in the repository.
- No read-side ABAP data-provider implementation for search/detail/reference/analytics entity sets is present in the repository.

Conclusion:

- The backend domain model is conceptually implied, but not fully delivered or evidenced in SAP artifacts available here.

### 3. Authorization And Security

Status: `Red`

Findings:

- No `AUTHORITY-CHECK` was found in accessible ABAP sources.
- Frontend depends on `ChecklistPermissionSet`, `ChecklistCreatePermissionSet`, `CurrentUserSet`.
- The permission seam is not optional: UI mode, edit rights, delete rights, create rights, access denial states all depend on it.

What must exist in productive SAP:

- Explicit authorization model by operation: create, view, edit, delete, export, analytics refresh, attachment access, lock takeover.
- Mapping to `PFCG` roles and `SU24` proposals.
- Denial semantics aligned with `ReasonCode` and `Message`.

Conclusion:

- Security model is not evidenced as ready.

### 4. Locking, Concurrency, Multi-User Safety

Status: `Red`

Findings:

- Frontend strongly depends on lock acquire, heartbeat, status polling, release, beacon release, and stale-save conflict behavior.
- Visible ABAP code shows multiple lock mechanisms (`zcl_zodata_lock_manager`, `zcl_lock_manager`, function group `zfg_zodata_lock`, async/update-task functions), which indicates architectural ambiguity.
- ETag setup in visible `MPC_EXT` is not aligned with the frontend metadata and is not enough evidence of a real optimistic concurrency contract.

What productive SAP must implement:

- One lock source of truth.
- Lock owner/session identity model.
- TTL and heartbeat semantics.
- Lock takeover semantics.
- Lost-lock behavior during autosave/save.
- Version or ETag based stale-write rejection.
- `LastChangeSet` backed by real aggregate change tracking.

Conclusion:

- This is one of the highest-risk backend areas.

### 5. Attachments

Status: `Red`

Frontend expectation:

- Attachments are part of the checklist aggregate save contract.
- The UI sends `Value` as base64 binary in `Edm.Binary`.
- Attachment reads and deletes use OData entity semantics.

Visible productive SAP status:

- No productive attachment storage implementation is visible.
- No content repository integration, virus scanning, MIME validation, retention, or audit model is evidenced.

Conclusion:

- Attachment handling is not backend-ready for productive SAP based on repository evidence.

### 6. Search, Reference Data, Value Help

Status: `Amber/Red`

Needed server capabilities:

- Search over checklist roots with server paging/filtering/sorting
- Reference dictionaries for LPC, professions, statuses, attachment categories, other domains
- Person value help
- Location hierarchy lookup by date

Visible SAP evidence:

- Only MPL/tree read helper is visible in ABAP wrapper.
- No full search provider/read entityset logic is available in the accessible SAP code.

Conclusion:

- These backend capabilities are required by frontend and only partially evidenced.

### 7. Analytics

Status: `Red`

Frontend expects:

- Summary KPIs
- Breakdown datasets filtered by selected year and source
- Refresh state polling
- Refresh trigger
- Export with search-contract semantics

Visible SAP evidence:

- No productive SAP analytics implementation or persistence objects are present in the repository.
- Only mock backend implements analytics entity sets and refresh state.

Conclusion:

- Analytics backend is not ready from a productive SAP evidence perspective.

## Productive SAP Backend Gap Summary

To safely serve the frontend, the productive SAP backend still needs:

1. Full OData service implementation matching `app/localService/metadata.xml`
2. Stable DDIC/CDS/BOPF domain model for checklist aggregate and related entities
3. Real authorization enforcement and role mapping
4. Single, supportable lock architecture
5. Real optimistic concurrency with ETag/version conflict handling
6. Reference data and person/location value-help providers
7. Attachment storage and security architecture
8. Analytics data model, aggregation logic, refresh lifecycle, export logic
9. Runtime settings provider
10. Error contract aligned to OData V2 and SAP message container behavior

## Recommended SAP Delivery Backlog

### Must Have Before Integration SIT

- Implement all entity sets and function imports from `metadata.xml`
- Freeze canonical request/response payloads
- Implement `ChecklistPermissionSet`, `ChecklistCreatePermissionSet`, `CurrentUserSet`
- Implement `LockAcquire`, `LockHeartbeat`, `LockRelease`, `LockStatusSet`
- Implement `CreateChecklist`, `AutoSave`, `SaveChanges`, `SetChecklistStatus`, delete
- Implement `ChecklistSearchSet`, `ChecklistRootSet`, `ChecklistBasicInfoSet`, `ChecklistCheckSet`, `ChecklistBarrierSet`
- Implement `DictionaryItemSet`, `PersonVHSet`, `GetHierarchy`
- Implement `RuntimeSettingsSet`
- Implement `LastChangeSet`

### Must Have Before UAT

- Implement `AttachmentSet` read/create/delete behavior with real storage
- Implement `CopyChecklist`
- Implement `ReportExport`
- Implement `SimpleAnalyticalSet`, `WorkflowAnalyticsBreakdownSet`, `AnalyticsRefreshStateSet`, `AnalyticsRefreshTrigger`
- Add authorization traces and test evidence
- Add ABAP Unit for contract mapping, permission evaluation, lock logic, save/version handling

### Must Have Before Productive Go-Live

- Transport/package governance
- PFCG roles and SU24 proposal alignment
- Virus scan/content repository integration for attachments
- Operational logging without sensitive payload leakage
- Gateway performance validation under batch/save/analytics load
- End-to-end concurrency test with real users/sessions

## Practical Final Verdict

If the question is "can the current frontend be connected to a real SAP backend with minimal backend work?", the answer is `no`.

If the question is "is the frontend contract mature enough to drive backend implementation?", the answer is `yes`.

The repository already tells backend teams what to build. What is missing is the productive SAP realization of that contract.
