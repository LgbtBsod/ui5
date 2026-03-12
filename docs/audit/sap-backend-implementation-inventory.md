# SAP Backend Implementation Inventory

Date: 2026-03-12

This file enumerates the backend-facing artifacts the UI5 frontend requires and the repository currently evidences.

## 1. OData Service Target

- Service name: `Z_EHS_PRODUCTION_CONTROL_CKLT_SRV`
- Protocol: `OData V2`
- Base URI: `/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/`
- Batch: required
- CSRF: required
- Update method: `MERGE`

## 2. Entity Types / Entity Sets Required By Frontend

### Core Checklist

- `ChecklistSearch` / `ChecklistSearchSet`
- `ChecklistRoot` / `ChecklistRootSet`
- `ChecklistBasicInfo` / `ChecklistBasicInfoSet`
- `ChecklistCheck` / `ChecklistCheckSet`
- `ChecklistBarrier` / `ChecklistBarrierSet`

### Reference / Security / Runtime

- `DictionaryItem` / `DictionaryItemSet`
- `PersonVH` / `PersonVHSet`
- `LastChange` / `LastChangeSet`
- `LockStatus` / `LockStatusSet`
- `ChecklistPermission` / `ChecklistPermissionSet`
- `ChecklistPermission` / `ChecklistCreatePermissionSet`
- `CurrentUser` / `CurrentUserSet`
- `RuntimeSettings` / `RuntimeSettingsSet`

### Analytics

- `SimpleAnalytical` / `SimpleAnalyticalSet`
- `WorkflowAnalyticsBreakdown` / `WorkflowAnalyticsBreakdownSet`
- `AnalyticsRefreshState` / `AnalyticsRefreshStateSet`

### Attachments

- `AttachmentFolder` / `AttachmentFolderSet`
- `Attachment` / `AttachmentSet`

### Function Result Types

- `FunctionResult`
- `SaveChangesResponse`
- `HierarchyNode`
- `ExportRow`

## 3. Function Imports Required By Frontend

- `LockAcquire`
- `LockHeartbeat`
- `LockRelease`
- `CreateChecklist`
- `CopyChecklist`
- `AutoSave`
- `SaveChanges`
- `SetChecklistStatus`
- `AnalyticsRefreshTrigger`
- `GetHierarchy`
- `ReportExport`

## 4. Mock Backend Physical Tables / Persistence Objects

Source: `backend/mock_gateway/models.py`

### Checklist Aggregate

- `checklist_root`
- `checklist_check`
- `checklist_barrier`
- `attachment_entry`

### Locking / Change Tracking / Audit

- `lock_entry`
- `lock_log`
- `save_request_ledger`
- `last_change_set`

### Identity / Reference Data

- `app_user_profile`
- `runtime_user_context`
- `persons`
- `dictionary_items`
- `locations`

### Analytics / Runtime Config

- `analytics_snapshot`
- `analytics_breakdown`
- `analytics_refresh_state`
- `frontend_runtime_settings`

## 5. Mock Backend API Modules

These modules show the functional surface the productive SAP backend must cover:

- `api/analytics_api.py`
- `api/batch_api.py`
- `api/capabilities_api.py`
- `api/checklist_api.py`
- `api/dictionary_api.py`
- `api/gateway_canonical_api.py`
- `api/hierarchy_api.py`
- `api/location_api.py`
- `api/lock_entity_api.py`
- `api/lock_history_api.py`
- `api/metadata_api.py`
- `api/person_api.py`
- `api/reference_api.py`
- `api/search_api.py`
- `api/settings_api.py`

## 6. Productive SAP ABAP Objects Present In Repository

### Classes

- `zcl_lock_manager`
- `zcl_zodata_bopf_mapper`
- `zcl_zodata_bopf_msg_helper`
- `zcl_zodata_dpc_ext`
- `zcl_zodata_lock_manager`
- `zcl_zodata_mpc_ext`
- `zcl_zodata_odata_util`
- `zcl_zodata_rtti_cache`
- `zcx_lock_error`
- `zcx_zodata_error`

### Interfaces

- `zif_zodata_bopf_mapper`
- `zif_zodata_lock_manager`

### Function Groups / Function Modules

- `zfg_zodata_lock`
- `zodata_lock_control`
- `z_lock_regs_async`
- `z_unlock_regs_update`

## 7. Implied SAP DDIC / Deep Payload Types Referenced By ABAP

These are referenced in ABAP and therefore must exist in SAP even though their definitions are not included in this repo:

- `ZSTR_PCCT_SAVECHANGES_RQ`
- `ZSTR_PCCT_SAVECHANGES_RS`
- `ZSTR_PCCT_LOCK_ACQUIRE_RQ`
- `ZSTR_PCCT_LOCK_ACQUIRE_RS`
- `ZSTR_PCCT_LOCK_HEARTBEAT_RQ`
- `ZSTR_PCCT_LOCK_HEARTBEAT_RS`
- `ZSTR_PCCT_LOCK_RELEASE_RQ`
- `ZSTR_PCCT_LOCK_RELEASE_RS`
- `ZSTR_PCCT_SERVICE_MSG`
- `ZTAB_PCCT_CHECK_DELTA`
- `ZTAB_PCCT_BARRIER_DELTA`
- `ZTAB_PCCT_PART_DELTA`
- `ZTAB_PCCT_ATTACH_DELTA`
- `ZTT_PCCT_MPL_TREE`
- `ZSTR_BO_ROOT`

## 8. Implied SAP Technical Objects That Are Still Missing From Repo Evidence

### DDIC Tables Likely Needed

- Header table for checklist root aggregate
- Child table for check items
- Child table for barrier items
- Child table for attachments or attachment links
- Lock header/table if custom lock storage is retained
- Last-change marker table
- Runtime settings/config table
- Analytics snapshot and analytics breakdown tables or views
- User permission/profile projection tables or authorization views
- Dictionary/reference customizing tables or views
- Location hierarchy tables/views
- Person value-help source view/table

### CDS Views Likely Needed

- Search projection CDS for `ChecklistSearchSet`
- Detail root/basic projection CDS
- Dictionary CDS by domain/date
- Person value help CDS
- Location hierarchy CDS or hierarchy provider
- Analytics summary CDS
- Analytics breakdown CDS
- Runtime settings CDS
- Current-user projection CDS
- Permission projection CDS

### BOPF / RAP / Business Object Artifacts Likely Needed

- Root node definition
- Check node definition
- Barrier node definition
- Participant node definition
- Attachment node definition
- Determinations/validations/actions for save/status/copy
- Lock integration with BO lifecycle

## 9. Frontend-Driven Backend Responsibilities

### Required Read Providers

- Checklist search with server filters/paging
- Detail aggregate read composition
- Dictionary by domain
- Person suggestion/value help
- Hierarchy lookup by date/method
- Current user profile
- Permission evaluation
- Runtime settings
- Last change marker
- Analytics summary/breakdown/refresh state
- Attachment binary read

### Required Write Providers

- Create checklist
- Save checklist
- Autosave checklist
- Copy checklist
- Change checklist status
- Delete checklist
- Delete attachment
- Trigger analytics refresh
- Acquire heartbeat/release lock

### Required Utility/Platform Services

- OData batch parser/executor
- CSRF issuance/validation
- ETag/version conflict control
- OData error envelope and SAP message mapping
- Binary/base64 attachment codec
- Audit/correlation logging
- Timer/runtime configuration provider

## 10. Implementation Priority For SAP Backend Team

### Priority 1

- `ChecklistSearchSet`
- `ChecklistRootSet`
- `ChecklistBasicInfoSet`
- `ChecklistCheckSet`
- `ChecklistBarrierSet`
- `ChecklistPermissionSet`
- `ChecklistCreatePermissionSet`
- `CurrentUserSet`
- `RuntimeSettingsSet`
- `LockAcquire`
- `LockHeartbeat`
- `LockRelease`
- `LockStatusSet`
- `CreateChecklist`
- `AutoSave`
- `SaveChanges`
- `SetChecklistStatus`
- delete `ChecklistRootSet`

### Priority 2

- `DictionaryItemSet`
- `PersonVHSet`
- `GetHierarchy`
- `LastChangeSet`
- `AttachmentSet`
- `CopyChecklist`
- `ReportExport`

### Priority 3

- `SimpleAnalyticalSet`
- `WorkflowAnalyticsBreakdownSet`
- `AnalyticsRefreshStateSet`
- `AnalyticsRefreshTrigger`
- `AttachmentFolderSet`

## 11. Key Conclusion

The frontend contract is already specific enough to serve as a backend delivery specification.

For productive SAP, you still need to deliver:

- physical persistence model
- CDS/read model
- save/update business logic
- lock/concurrency model
- permission model
- attachment model
- analytics model
- runtime configuration model

Without these, the frontend cannot be considered fully supported by SAP backend.
