# SAP Backend Task List For ABAP/CDS Developer

Date: 2026-03-12

Purpose: implementation checklist for building a productive SAP backend that can fully serve the UI5 frontend contract in this repository.

## 1. Tables

Below is the minimum physical persistence model implied by the frontend contract and mock backend.

### 1.1 Core Checklist Aggregate

- `ZPCCT_HDR`
  - Checklist root/header
  - Main key: `ROOT_KEY` / `PCCT_UUID`
  - Suggested fields:
    - `PCCT_UUID`
    - `CHECKLIST_ID`
    - `STATUS`
    - `VERSION_NUMBER`
    - `DATE_CHECK`
    - `TIME_CHECK`
    - `TIME_ZONE`
    - `LOCATION_KEY`
    - `LOCATION_NAME`
    - `LOCATION_TEXT`
    - `BUKRS`
    - `LPC`
    - `LPC_TEXT`
    - `PROFESSION`
    - `PROFESSION_TEXT`
    - `EQUIP_NAME`
    - `OBSERVER_PERNR`
    - `OBSERVER_FULLNAME`
    - `OBSERVER_POSITION`
    - `OBSERVER_ORGUNIT`
    - `OBSERVED_PERNR`
    - `OBSERVED_FULLNAME`
    - `OBSERVED_POSITION`
    - `OBSERVED_ORGUNIT`
    - `INTEGRATION_FLAG`
    - `CREATED_ON`
    - `CREATED_BY`
    - `CHANGED_ON`
    - `CHANGED_BY`
    - `IS_DELETED`

- `ZPCCT_CHECK`
  - Checklist check rows
  - Suggested fields:
    - `CHECK_KEY`
    - `ROOT_KEY`
    - `CHECKS_NUM`
    - `TEXT`
    - `COMMENT`
    - `RESULT`
    - `CHANGED_ON`
    - `CHANGED_BY`

- `ZPCCT_BARRIER`
  - Checklist barrier rows
  - Suggested fields:
    - `BARRIER_KEY`
    - `ROOT_KEY`
    - `BARRIERS_NUM`
    - `TEXT`
    - `COMMENT`
    - `RESULT`
    - `CHANGED_ON`
    - `CHANGED_BY`

### 1.2 Attachments

- `ZPCCT_ATTACH`
  - Attachment metadata
  - Suggested fields:
    - `ATTACHMENT_KEY`
    - `ROOT_KEY`
    - `PARENT_KEY`
    - `FOLDER_KEY`
    - `CATEGORY_KEY`
    - `TYPE`
    - `FILE_NAME`
    - `DISPLAY_NAME`
    - `MIME_TYPE`
    - `DESCRIPTION`
    - `FILE_SIZE`
    - `STORAGE_ID`
    - `SCAN_STATUS`
    - `SCANNED_ON`
    - `CREATED_ON`
    - `CREATED_BY`
    - `CHANGED_ON`
    - `CHANGED_BY`

- `ZPCCT_ATFOLD`
  - Optional attachment folder table if folder grouping is needed by business
  - Suggested fields:
    - `FOLDER_KEY`
    - `ROOT_KEY`
    - `TITLE`
    - `CREATED_ON`
    - `CHANGED_ON`

### 1.3 Locking / Concurrency / Audit

- `ZPCCT_LOCK`
  - One lock source of truth
  - Suggested fields:
    - `ROOT_KEY`
    - `USER_ID`
    - `SESSION_GUID`
    - `LOCKED_AT`
    - `EXPIRES_AT`
    - `LAST_HEARTBEAT`
    - `IS_KILLED`
    - `KILLED_BY`

- `ZPCCT_LOCK_LOG`
  - Lock event history
  - Suggested fields:
    - `LOG_KEY`
    - `ROOT_KEY`
    - `USER_ID`
    - `SESSION_GUID`
    - `ACTION`
    - `EVENT_AT`

- `ZPCCT_LASTCHG`
  - Aggregate freshness marker
  - Suggested fields:
    - `ROOT_KEY`
    - `AGG_CHANGED_ON`

- `ZPCCT_SAVE_LED`
  - Optional idempotency/audit ledger for create/save/autosave
  - Suggested fields:
    - `REQUEST_GUID`
    - `OPERATION`
    - `ROOT_KEY`
    - `USER_ID`
    - `RESPONSE_HASH`
    - `CREATED_ON`

### 1.4 Reference / Value Help / Runtime

- `ZPCCT_DICT`
  - Generic dictionaries
  - Suggested fields:
    - `DOMAIN`
    - `KEY`
    - `TEXT`
    - `BEGDA`
    - `ENDDA`
    - `CHANGED_ON`

- `ZPCCT_RT_CFG`
  - Runtime settings for frontend timers and policies
  - Suggested fields:
    - `CFG_KEY`
    - `CACHE_TOLERANCE_MS`
    - `HEARTBEAT_INTERVAL_SEC`
    - `STATUS_POLL_INTERVAL_SEC`
    - `LOCK_TTL_SEC`
    - `IDLE_TIMEOUT_SEC`
    - `AUTOSAVE_DEBOUNCE_MS`
    - `AUTOSAVE_INTERVAL_SEC`
    - `REQUIRED_FIELDS_JSON`
    - `FRONTEND_VARIABLES_JSON`
    - `UPLOAD_POLICY_JSON`
    - `CHANGED_ON`

### 1.5 Analytics

- `ZPCCT_AN_SNAP`
  - Analytics summary snapshot
  - Suggested fields:
    - `SNAP_KEY`
    - `SELECTED_YEAR`
    - `SOURCE_KEY`
    - `AVAILABLE_YEARS_JSON`
    - `TOTAL`
    - `MONTHLY`
    - `FAILED_CHECKS`
    - `FAILED_BARRIERS`
    - `FAILED_CHECKLIST_COUNT`
    - `FAILED_BARRIER_CHECKLIST_COUNT`
    - `CLOSED_COUNT`
    - `REGISTERED_COUNT`
    - `AVG_CHECKS_RATE`
    - `AVG_BARRIERS_RATE`
    - `HEALTHY`
    - `REFRESHED_AT`
    - `SOURCE`

- `ZPCCT_AN_BRKD`
  - Analytics breakdown snapshot
  - Suggested fields:
    - `ROW_KEY`
    - `SELECTED_YEAR`
    - `SOURCE_KEY`
    - `DIMENSION`
    - `METRIC`
    - `BUCKET_KEY`
    - `LABEL`
    - `VALUE`
    - `SORT_ORDER`
    - `CHANGED_ON`

- `ZPCCT_AN_RFST`
  - Analytics refresh state
  - Suggested fields:
    - `TASK_KEY`
    - `TASK_NAME`
    - `STATUS`
    - `IS_RUNNING`
    - `REQUESTED_AT`
    - `REQUESTED_BY`
    - `STARTED_AT`
    - `FINISHED_AT`
    - `LAST_SUCCESS_AT`
    - `LAST_ERROR`
    - `LAST_MESSAGE`
    - `ACTIVE_RUN_ID`
    - `CHANGED_ON`

## 2. CDS Views

These CDS views should back the OData read contract. Names are suggested, not mandatory.

### 2.1 Core Read Model

- `ZI_PCCT_CHECKLIST_SEARCH`
  - Source for `ChecklistSearchSet`
  - Must expose:
    - `Key`
    - `Id`
    - `DateCheck`
    - `TimeCheck`
    - `TimeZone`
    - `LocationKey`
    - `Bukrs`
    - `Lpc`
    - `LpcText`
    - `Profession`
    - `ProfessionText`
    - `EquipName`
    - `Status`
    - `SourceKey`
    - `IntegrationFlag`
    - `CreatedOn`
    - `ChangedOn`
    - `HasFailedChecks`
    - `HasFailedBarriers`
    - `SuccessChecksRate`
    - `SuccessBarriersRate`
    - `ChecksTotal`
    - `BarriersTotal`

- `ZI_PCCT_CHECKLIST_ROOT`
  - Source for `ChecklistRootSet`

- `ZI_PCCT_CHECKLIST_BASIC`
  - Source for `ChecklistBasicInfoSet`

- `ZI_PCCT_CHECKLIST_CHECK`
  - Source for `ChecklistCheckSet`

- `ZI_PCCT_CHECKLIST_BARRIER`
  - Source for `ChecklistBarrierSet`

### 2.2 Reference / Helper Views

- `ZI_PCCT_DICTIONARY`
  - Source for `DictionaryItemSet`

- `ZI_PCCT_PERSON_VH`
  - Source for `PersonVHSet`

- `ZI_PCCT_LAST_CHANGE`
  - Source for `LastChangeSet`

- `ZI_PCCT_LOCK_STATUS`
  - Source or helper view for `LockStatusSet`

- `ZI_PCCT_PERMISSION`
  - Source for `ChecklistPermissionSet`
  - Must evaluate business auth state into:
    - `UserId`
    - `GrantedOperations`
    - `CanCreate`
    - `CanView`
    - `CanEdit`
    - `CanDelete`
    - `ReasonCode`
    - `Message`

- `ZI_PCCT_CURRENT_USER`
  - Source for `CurrentUserSet`

- `ZI_PCCT_RUNTIME_SETTINGS`
  - Source for `RuntimeSettingsSet`

### 2.3 Analytics Views

- `ZI_PCCT_ANALYTICS_SUMMARY`
  - Source for `SimpleAnalyticalSet`

- `ZI_PCCT_ANALYTICS_BREAKDOWN`
  - Source for `WorkflowAnalyticsBreakdownSet`

- `ZI_PCCT_ANALYTICS_REFRESH_STATE`
  - Source for `AnalyticsRefreshStateSet`

### 2.4 Attachments Views

- `ZI_PCCT_ATTACHMENT`
  - Source for `AttachmentSet`

- `ZI_PCCT_ATTACHMENT_FOLDER`
  - Source for `AttachmentFolderSet`

## 3. OData Entity Sets To Implement

These entity sets must match [metadata.xml](C:/Users/lgbtb/Desktop/ui5/app/localService/metadata.xml).

### Mandatory Priority 1

- `ChecklistSearchSet`
- `ChecklistRootSet`
- `ChecklistBasicInfoSet`
- `ChecklistCheckSet`
- `ChecklistBarrierSet`
- `ChecklistPermissionSet`
- `ChecklistCreatePermissionSet`
- `CurrentUserSet`
- `RuntimeSettingsSet`
- `LockStatusSet`

### Mandatory Priority 2

- `DictionaryItemSet`
- `PersonVHSet`
- `LastChangeSet`
- `AttachmentSet`

### Mandatory Priority 3

- `SimpleAnalyticalSet`
- `WorkflowAnalyticsBreakdownSet`
- `AnalyticsRefreshStateSet`
- `AttachmentFolderSet`

## 4. Function Imports To Implement

### Mandatory Priority 1

- `LockAcquire`
  - acquire edit lock
  - input:
    - `RootId`
    - `SessionGuid`
    - optional force/takeover params if contract is retained
  - output:
    - `Ok`
    - `Success`
    - `ReasonCode`
    - `Owner`
    - `OwnerSession`
    - `LockExpires`
    - `IsKilled`

- `LockHeartbeat`
  - extend lock TTL

- `LockRelease`
  - release lock
  - must tolerate already-expired locks gracefully

- `CreateChecklist`
  - create aggregate with deep payload
  - must return:
    - `RootKey`
    - `Id`
    - `AggChangedOn`
    - `VersionNumber`

- `AutoSave`
  - deep delta save
  - stale version must return conflict

- `SaveChanges`
  - explicit save
  - stale version must return conflict

- `SetChecklistStatus`
  - status transition with concurrency protection

### Mandatory Priority 2

- `CopyChecklist`
- `GetHierarchy`
- `ReportExport`

### Mandatory Priority 3

- `AnalyticsRefreshTrigger`

## 5. Classes / Utilities To Implement

### OData Service Layer

- `ZCL_ZODATA_MPC_EXT`
  - metadata definition
  - must align exactly with frontend metadata contract

- `ZCL_ZODATA_DPC_EXT`
  - entityset/entity/function implementation

### Read-Side Providers

- `ZCL_PCCT_SEARCH_QRY`
  - search query builder/provider

- `ZCL_PCCT_DETAIL_QRY`
  - root/basic/check/barrier detail read

- `ZCL_PCCT_DICT_SRV`
  - dictionary resolution

- `ZCL_PCCT_PERSON_VH_SRV`
  - person suggestions

- `ZCL_PCCT_LOC_HIER_SRV`
  - location hierarchy/value help

- `ZCL_PCCT_ANALYTICS_QRY`
  - analytics summary/breakdown read

- `ZCL_PCCT_RUNTIME_CFG_SRV`
  - runtime settings provider

- `ZCL_PCCT_AUTH_SRV`
  - permission evaluation

### Write-Side / Domain Services

- `ZCL_PCCT_SAVE_SRV`
  - create/save/autosave orchestration

- `ZCL_PCCT_STATUS_SRV`
  - status transitions

- `ZCL_PCCT_COPY_SRV`
  - aggregate copy

- `ZCL_PCCT_ATTACH_SRV`
  - attachment metadata + binary/content handling

- `ZCL_PCCT_EXPORT_SRV`
  - export generation by `SelectionMode`, `RootKeys`, `SearchContract`

### Concurrency / Locking / Messaging

- `ZCL_PCCT_LOCK_SRV`
  - one consolidated lock service

- `ZCL_PCCT_ETAG_SRV`
  - ETag/version generation and validation

- `ZCL_PCCT_ODATA_ERR_SRV`
  - normalize business exceptions to OData/SAP message format

- `ZCL_PCCT_LASTCHG_SRV`
  - aggregate change marker maintenance

### Utility / Mapper Layer

- mapper class for frontend deep payload to internal structures
- attachment binary/base64 codec utility
- JSON runtime-settings serializer/deserializer
- filter parser helper for export/search if generic OData filter reuse is needed

## 6. Authorization Objects

The frontend contract strongly implies operation-based authorization. Suggested object model:

- `Z_UI5_CHKL`
  - activity values:
    - `01` create
    - `02` change
    - `03` display
    - `06` delete

Additional recommended auth coverage:

- `Z_UI5_CHKL_EXP`
  - export permission

- `Z_UI5_CHKL_ANA`
  - analytics view / refresh permission

- `Z_UI5_CHKL_ATT`
  - attachment read/upload/delete permission

- `Z_UI5_CHKL_LOC`
  - location scope restriction if needed

### Mandatory implementation rules

- Add explicit `AUTHORITY-CHECK` in productive ABAP flow.
- Reflect auth result into:
  - `ChecklistPermissionSet`
  - `ChecklistCreatePermissionSet`
  - `CurrentUserSet`
- Denied response must be non-permissive and must not leak business payload.

## 7. Order Of Implementation

### Phase 1: Contract Freeze

1. Freeze canonical metadata based on [metadata.xml](C:/Users/lgbtb/Desktop/ui5/app/localService/metadata.xml).
2. Freeze payload naming for create/save/autosave/lock/export.
3. Decide whether service remains BOPF-based or moves to another backend pattern.
4. Decide final attachment storage architecture.

### Phase 2: Persistence + Read Model

1. Create DDIC tables.
2. Create CDS read views.
3. Build search/detail/reference/runtime read providers.
4. Publish entity sets and verify against frontend reads.

### Phase 3: Security + Concurrency

1. Implement authorization objects and `AUTHORITY-CHECK`.
2. Implement one lock service only.
3. Implement ETag/version control and stale-write rejection.
4. Implement `LastChangeSet`.

### Phase 4: Write Flows

1. Implement `CreateChecklist`.
2. Implement `AutoSave`.
3. Implement `SaveChanges`.
4. Implement `SetChecklistStatus`.
5. Implement delete for `ChecklistRootSet`.
6. Implement `CopyChecklist`.

### Phase 5: Attachments

1. Implement attachment metadata model.
2. Implement binary transport and storage.
3. Implement virus scan and MIME policy.
4. Implement `AttachmentSet` read/delete.

### Phase 6: Analytics + Export

1. Implement analytics snapshots or views.
2. Implement `SimpleAnalyticalSet`.
3. Implement `WorkflowAnalyticsBreakdownSet`.
4. Implement `AnalyticsRefreshStateSet`.
5. Implement `AnalyticsRefreshTrigger`.
6. Implement `ReportExport`.

### Phase 7: Quality Gates

1. ABAP Unit for auth, lock, mapper, save/version handling.
2. Integration tests against real Gateway service.
3. End-to-end UI5 verification with real backend.
4. STAUTHTRACE / SU53 evidence for critical operations.

## 8. Definition Of Done

Backend can be called ready for this frontend only when:

- all entity sets from metadata are implemented
- all required function imports are implemented
- frontend can search, open detail, edit, autosave, save, copy, change status, delete, and manage attachments
- permissions correctly switch UI between create/view/edit/delete states
- lock heartbeat and stale save scenarios are handled correctly
- analytics screens load without mock adapters
- export works by selected rows and full search contract
- authorization, concurrency, and attachment security are evidenced in SAP

## 9. Practical Recommendation

Do not start from analytics or export first.

Build in this order:

1. contract freeze
2. root/detail/search read model
3. auth + lock + version control
4. create/save/autosave/delete
5. dictionaries/person/location/runtime settings
6. attachments
7. copy/status
8. analytics
9. export

That order reduces frontend integration risk fastest.
