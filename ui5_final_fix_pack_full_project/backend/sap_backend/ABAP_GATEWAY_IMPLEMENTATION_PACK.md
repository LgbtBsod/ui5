# ABAP Gateway Implementation Pack

Date: 2026-03-17

Purpose: define the concrete ABAP object pack required to support the current UI5 frontend contract on a real SAP Gateway backend.

This pack complements the source files in:
- `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap`
- `backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap`
- `backend/sap_backend/src/zcl_zodata_lock_manager.clas.abap`
- `backend/sap_backend/src/zfg_zodata_lock.fugr.abap`
- `backend/sap_backend/src/zcl_zodata_contract_constants.clas.abap`
- `backend/sap_backend/src/zcl_zodata_contract_service.clas.abap`

## 1. Object Inventory

### Service provider classes

- `ZCL_ZODATA_DPC_EXT`
  - `LOCKACQUIRE_CREATE_ENTITY`
  - `LOCKHEARTBEAT_CREATE_ENTITY`
  - `LOCKRELEASE_CREATE_ENTITY`
  - `AUTOSAVE_CREATE_ENTITY`
  - `SAVECHANGES_CREATE_ENTITY`
  - `MPLTREESET_GET_ENTITYSET`
  - plus productive read handlers for:
    - `CHECKLISTROOTSET_GET_ENTITY`
    - `CHECKLISTBASICINFOSET_GET_ENTITYSET`
    - `CHECKLISTCHECKSET_GET_ENTITYSET`
    - `CHECKLISTBARRIERSET_GET_ENTITYSET`
    - `CHECKLISTPERMISSIONSET_GET_ENTITY`
    - `CHECKLISTCREATEPERMISSIONSET_GET_ENTITY`
    - `CURRENTUSERSET_GET_ENTITY`
    - `RUNTIMESETTINGSSET_GET_ENTITY`

- `ZCL_ZODATA_MPC_EXT`
  - deep save/autosave types
  - lock FI metadata
  - runtime/permission/current-user entities
  - `ChecklistRoot` ETag binding

### Domain and helper classes

- `ZCL_ZODATA_BOPF_MAPPER`
  - deep delta to `/BOBF/T_FRW_MODIFICATION`

- `ZCL_ZODATA_LOCK_MANAGER`
  - central session-aware lock orchestration
  - canonical methods:
    - `ACQUIRE`
    - `HEARTBEAT`
    - `RELEASE`
    - `STATUS`
    - `ENSURE_SESSION_LOCK`

- `ZCL_ZODATA_CONTRACT_CONSTANTS`
  - single source of truth for:
    - auth object and operation codes
    - lock reason codes
    - frontend runtime timer defaults

- `ZCL_ZODATA_CONTRACT_SERVICE`
  - fills DDIC-specific response structures dynamically by contract field names
  - canonical response assembly for:
    - lock responses
    - save responses
    - permission responses
    - current user
    - runtime settings

- recommended additional classes for productive system:
  - `ZCL_ZODATA_AUTHORIZATION_SRV`
  - `ZCL_ZODATA_RUNTIME_SETTINGS_SRV`
  - `ZCL_ZODATA_CURRENT_USER_SRV`
  - `ZCL_ZODATA_DETAIL_READ_SRV`
  - `ZCL_ZODATA_ANALYTICS_SRV`

### Interfaces

- `ZIF_ZODATA_LOCK_MANAGER`
  - canonical contract for lock lifecycle methods

- `ZIF_ZODATA_BOPF_MAPPER`
  - deep delta mapping seam

### Function groups and function modules

- function group `ZFG_ZODATA_LOCK`
  - `ZODATA_LOCK_CONTROL`
  - recommended productive split:
    - `Z_PCCT_LOCK_ACQUIRE`
    - `Z_PCCT_LOCK_HEARTBEAT`
    - `Z_PCCT_LOCK_RELEASE`
    - `Z_PCCT_LOCK_STATUS`
    - `Z_PCCT_LOCK_VALIDATE`
    - `Z_PCCT_LOCK_CLEANUP`

- legacy helpers still present in repo:
  - `Z_LOCK_REGS_ASYNC`
  - `Z_UNLOCK_REGS_UPDATE`

## 2. Productive Contract Coverage

### Lock contract

The frontend expects the backend to support canonical lock outcomes:

- `LOCK_OK`
- `LOCK_MISSING`
- `LOCK_EXPIRED`
- `LOCK_STOLEN`
- `LOCK_NOT_OWNED_BY_SESSION`
- `PERMISSION_DENIED`

The response payload should be able to expose:

- `ok`
- `success`
- `code`
- `reason_code`
- `action`
- `owner`
- `owner_session`
- `tab_session_id`
- `object_uuid`
- `lock_expires`
- `lock_expires_at`
- `server_now`
- `lock_refreshed`
- `owner_session_match`

### Save / autosave contract

The frontend save path expects:

- `pcct_uuid`
- `changed_on`
- `version_number`
- `code`
- `is_autosave`
- `no_changes`
- `messages`
- `lock_refreshed`
- `lock_expires_at`
- `server_now`
- `request_id`
- `reason_code`

Save and autosave must:

1. validate active lock ownership by `session_guid`
2. reject stale or stolen sessions before any modify
3. commit immediately
4. return the authoritative persisted version/timestamp from `ZTODATA_HDR`
5. emit `code=LOCK_OK`, `lock_refreshed=true`, `request_id`, `server_now`

Delete must:

1. rollback on modify failure
2. best-effort unlock on exception path
3. avoid stranding lock ownership until TTL expiry

### Permission contract

The frontend uses:

- `AuthObject`
- `CreateOperation`
- `ViewOperation`
- `ChangeOperation`
- `DeleteOperation`
- `GrantedOperations`
- `CanCreate`
- `CanView`
- `CanEdit`
- `CanDelete`
- `ReasonCode`
- `Message`

Reason codes to support:

- `AUTHORIZED`
- `NO_CREATE_AUTH`
- `NO_VIEW_AUTH`
- `READ_ONLY_AUTH`
- `NO_EDIT_AUTH`
- `NO_DELETE_AUTH`

### Current user contract

The frontend shell expects:

- `Key='CURRENT'`
- `FullName`
- `PermissionsCsv`
- `PermissionRulesJson`
- `CanView`
- `CanEdit`
- `CanDelete`
- `SummaryText`

### Runtime settings contract

The frontend runtime expects:

- `Key='GLOBAL'`
- `Environment`
- `HeartbeatMs=270000`
- `IdleMs=570000`
- `AutoSaveIntervalMs=150000`
- `LockRefreshCooldownMs=150000`
- `AnalyticsRefreshMs=900000`
- `GcdIntervalMs=30000`
- `NetworkGraceMs=15000`
- `CacheToleranceMs=5500`

Profile selection remains environment-driven; no end-user runtime toggle is part of the productive contract.

## 3. Required DDIC Objects

### Concrete namespace proposal

- service namespace:
  - `ZCL_ZODATA_DPC_EXT`
  - `ZCL_ZODATA_MPC_EXT`
  - `ZCL_ZODATA_CONTRACT_CONSTANTS`
  - `ZCL_ZODATA_CONTRACT_SERVICE`
  - `ZCL_ZODATA_LOCK_MANAGER`
  - `ZIF_ZODATA_LOCK_MANAGER`
- transparent tables:
  - `ZTODATA_HDR`
  - `ZPCCT_CHECK`
  - `ZPCCT_BARRIER`
  - `ZPCCT_PART`
  - `ZPCCT_ATTACH`
- DDIC response structures:
  - `ZSTR_PCCT_PERMISSION_RS`
  - `ZSTR_PCCT_CURRENT_USER_RS`
  - `ZSTR_PCCT_RUNTIME_SETTINGS_RS`
  - `ZSTR_PCCT_LAST_CHANGE_RS`
  - `ZTT_PCCT_MPL_TREE`

### Save delta types

- `ZSTR_PCCT_SAVECHANGES_RQ`
- `ZSTR_PCCT_SAVECHANGES_RS`
- `ZSTR_PCCT_ROOT_DELTA`
- `ZTAB_PCCT_CHECK_DELTA`
- `ZTAB_PCCT_BARRIER_DELTA`
- `ZTAB_PCCT_PART_DELTA`
- `ZTAB_PCCT_ATTACH_DELTA`
- `ZSTR_PCCT_SERVICE_MSG`

### Lock request/response types

- `ZSTR_PCCT_LOCK_ACQUIRE_RQ`
- `ZSTR_PCCT_LOCK_ACQUIRE_RS`
- `ZSTR_PCCT_LOCK_HEARTBEAT_RQ`
- `ZSTR_PCCT_LOCK_HEARTBEAT_RS`
- `ZSTR_PCCT_LOCK_RELEASE_RQ`
- `ZSTR_PCCT_LOCK_RELEASE_RS`

### Read and shell types

- `ZSTR_PCCT_PERMISSION_RS`
- `ZSTR_PCCT_CURRENT_USER_RS`
- `ZSTR_PCCT_RUNTIME_SETTINGS_RS`
- `ZSTR_PCCT_LAST_CHANGE_RS`
- `ZTT_PCCT_MPL_TREE`

### Transparent tables

- `ZTODATA_HDR`
  - includes lock/session columns:
    - `PCCT_UUID` RAW16 key
    - `CHECKLIST_ID` CHAR20
    - `LPC` CHAR10
    - `LPC_TEXT` CHAR60
    - `STATUS` CHAR20
    - `INTEGRATION_FLAG` ABAP_BOOL
    - `DATE_CHECK` DATS
    - `TIME_CHECK` TIMS
    - `TIME_ZONE` CHAR40
    - `EQUIPMENT` CHAR80
    - `BUKRS` BUKRS
    - `OBSERVER_FULLNAME` CHAR120
    - `OBSERVER_PERNER` PERSNO / PERNR_D
    - `OBSERVER_POSITION` CHAR80
    - `OBSERVER_ORGUNIT` CHAR80
    - `OBSERVED_FULLNAME` CHAR120
    - `OBSERVED_PERNER` PERSNO / PERNR_D
    - `OBSERVED_POSITION` CHAR80
    - `OBSERVED_ORGUNIT` CHAR80
    - `LOCATION_KEY` CHAR40
    - `LOCATION_NAME` CHAR80
    - `LOCATION_TEXT` CHAR120
    - `CREATED_ON` TIMESTAMPL
    - `CREATED_BY` SYUNAME
    - `CHANGED_ON` TIMESTAMPL
    - `CHANGED_BY` SYUNAME
    - `VERSION_NUMBER` INT4
    - `LOCK_OWNER`
    - `LOCK_SESSION`
    - `TAB_SESSION_ID`
    - `LAST_TOUCH_AT`
    - `LAST_TOUCH_BY`
    - `LOCK_EXPIRES_AT`

- business tables:
  - `ZPCCT_CHECK`
    - `CHECK_UUID` RAW16 key
    - `PCCT_UUID` RAW16 foreign key
    - `CHECKS_NUM` INT4
    - `CHECK_TEXT` CHAR255 / STRING
    - `COMMENT_TEXT` CHAR255 / STRING
    - `RESULT` ABAP_BOOL
    - `CHANGED_ON` TIMESTAMPL
  - `ZPCCT_BARRIER`
    - `BARRIER_UUID` RAW16 key
    - `PCCT_UUID` RAW16 foreign key
    - `BARRIERS_NUM` INT4
    - `BARRIER_TEXT` CHAR255 / STRING
    - `COMMENT_TEXT` CHAR255 / STRING
    - `RESULT` ABAP_BOOL
    - `CHANGED_ON` TIMESTAMPL
  - `ZPCCT_PART`
  - `ZPCCT_ATTACH`

### DDIC response layouts expected by current read handlers

- `ZSTR_PCCT_PERMISSION_RS`
  - `ROOTKEY` RAW16
  - `USERID` SYUNAME
  - `AUTHOBJECT` CHAR30
  - `CREATEOPERATION` CHAR2
  - `VIEWOPERATION` CHAR2
  - `CHANGEOPERATION` CHAR2
  - `DELETEOPERATION` CHAR2
  - `GRANTEDOPERATIONS` STRING
  - `CANCREATE` ABAP_BOOL
  - `CANVIEW` ABAP_BOOL
  - `CANEDIT` ABAP_BOOL
  - `CANDELETE` ABAP_BOOL
  - `REASONCODE` CHAR40
  - `MESSAGE` STRING

- `ZSTR_PCCT_CURRENT_USER_RS`
  - `KEY` CHAR20
  - `FULLNAME` STRING
  - `PERMISSIONSCSV` STRING
  - `PERMISSIONRULESJSON` STRING
  - `CANVIEW` ABAP_BOOL
  - `CANEDIT` ABAP_BOOL
  - `CANDELETE` ABAP_BOOL
  - `SUMMARYTEXT` STRING

- `ZSTR_PCCT_RUNTIME_SETTINGS_RS`
  - `KEY` CHAR20
  - `ENVIRONMENT` CHAR20
  - `HEARTBEATMS` INT4
  - `IDLEMS` INT4
  - `AUTOSAVEINTERVALMS` INT4
  - `LOCKREFRESHCOOLDOWNMS` INT4
  - `ANALYTICSREFRESHMS` INT4
  - `GCDINTERVALMS` INT4
  - `NETWORKGRACEMS` INT4
  - `CACHETOLERANCEMS` INT4

## 4. Recommended Activation Sequence

1. Create DDIC structures/table types for save, lock, permission, current user, runtime settings.
2. Create or extend transparent lock truth table with session-aware columns.
3. Activate `ZCL_ZODATA_CONTRACT_CONSTANTS`.
4. Activate `ZCL_ZODATA_CONTRACT_SERVICE`.
5. Activate `ZIF_ZODATA_LOCK_MANAGER`.
6. Activate `ZFG_ZODATA_LOCK` and underlying function modules.
7. Activate `ZCL_ZODATA_LOCK_MANAGER`.
8. Activate `ZCL_ZODATA_BOPF_MAPPER`.
9. Extend `ZCL_ZODATA_MPC_EXT` metadata for permission/current-user/runtime-settings entities.
10. Extend `ZCL_ZODATA_DPC_EXT` read handlers and lock/save flows.
11. Register and reload the Gateway service.

## 5. Productive Acceptance Checklist

- `LockAcquire`, `LockHeartbeat`, `LockRelease`, `LockStatus` all prove the same runtime truth.
- save/autosave fail immediately on stale/stolen session.
- permission seam returns explicit reason codes instead of implicit denial.
- current user payload is shell-ready.
- runtime settings are served from ABAP, not hardcoded on the frontend.
- `ChecklistRoot` ETag is bound to real root entity and `If-Match` is enforced.
- seeded/mock-only assumptions are absent from productive code.
