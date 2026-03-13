# Backend Implementation And Evidence Backlog

Date: 2026-03-13

Purpose: capture the backend work that is still required for productive SAP rollout and SAP certification-style review, split into:
- code-level work that can safely be implemented in repo-backed ABAP/Python assets
- system-level work that cannot be completed without a real SAP landscape

## Already Implemented In Repo

### Python mock contour hardening

- mock identity is now local-profile only
- startup schema mutation is now local-profile only
- startup seed data is now local-profile only
- request body logging is disabled by default
- capabilities endpoint exposes:
  - `profile`
  - `identityMode`
  - `startupMutation.schema`
  - `startupMutation.seedData`

Relevant files:
- `backend/mock_gateway/config.py`
- `backend/mock_gateway/main.py`
- `backend/mock_gateway/api/capabilities_api.py`
- `backend/mock_gateway/tests/test_gateway_backend_enterprise_readiness.py`

### Explicit delta contract for SaveChanges / AutoSave

- frontend now targets a unified delta shape for save/autosave:
  - `root`
  - `checks`
  - `barriers`
  - `participants`
  - `attachments`
  - `client_version`
- mutable rows are expected to carry explicit `edit_mode = C|U|D`
- mapper root handling is temporarily backward-compatible and falls back to `U` when `root-edit_mode` is missing

Repo-side owners:
- `app/service/contracts/DeltaContracts.js`
- `app/service/shared/DeltaPayloadBuilder.js`
- `app/service/shared/delta/DeltaFieldMappers.js`
- `app/infra/adapters/shared/ODataChecklistPayloadMapper.js`
- `backend/sap_backend/src/zcl_zodata_bopf_mapper.clas.abap`
- `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap`

Repo-side hardening completed:
- DPC validates deep delta contract before BOPF mapping:
  - `root.pcct_uuid` required
  - `checks[].edit_mode` required
  - `barriers[].edit_mode` required
  - `participants[].edit_mode` required
  - `attachments[].edit_mode` required
- MPC documentation now explicitly states that root, participant, and attachment DDIC delta structures are expected to expose `EDIT_MODE`

## ABAP Code Work Still Required

These items are code tasks, but they cannot be completed safely in this repo alone because they depend on DDIC objects, auth objects, customizing, and live service metadata.

### 1. Authorization enforcement in DPC_EXT

Target files:
- `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap`
- optional helper class to be added in SAP system, e.g. `ZCL_ZODATA_AUTHORIZATION`

Required implementation:
- enforce explicit authorization before:
  - detail open
  - edit/lock acquire
  - autosave/savechanges
  - delete
  - export/report
  - hierarchy/MPL reads if role-restricted
- use productive authorization objects agreed with Security/PFCG
- return business exceptions with traceable denial reason codes

Why this is not completed here:
- the repo does not contain the productive authorization object set
- the repo does not contain SU24/PFCG mapping
- hardcoding speculative `AUTHORITY-CHECK` objects here would be unsafe and likely wrong

### 2. Productive lock architecture consolidation

Target files:
- `backend/sap_backend/src/zcl_zodata_lock_manager.clas.abap`
- `backend/sap_backend/src/zcl_lock_manager.clas.abap`
- `backend/sap_backend/src/zfg_zodata_lock.fugr.abap`
- `backend/sap_backend/src/z_lock_regs_async.fugr.abap`
- `backend/sap_backend/src/z_unlock_regs_update.fugr.abap`

Required implementation:
- choose one authoritative lock architecture
- align heartbeat, timeout, stale-lock cleanup, and takeover semantics
- remove duplicate lock truth stores if both are active
- prove beacon/heartbeat/release behavior on real system

Why this is not completed here:
- the repo shows multiple lock mechanisms but not the final productive ownership model
- safe consolidation requires real SM12/object/table evidence and business takeover rules

### 3. ETag and optimistic concurrency hardening

Target files:
- `backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap`
- `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap`

Required implementation:
- publish the ETag on the real root entity name exposed by `$metadata`
- enforce `If-Match`
- align `version_number` with server truth
- prove conflict behavior with concurrent save traces

Current repo risk:
- `configure_etag` currently targets entity name `Checklist`, while frontend metadata and audit findings indicate productive root exposure around `ChecklistRoot`

### 4. ABAP Unit coverage

Required test targets:
- mapper build/change conversion
- lock manager exception paths
- save response/version assembly
- message translation helper

Why this is not completed here:
- ABAP Unit classes are not represented in the current repo artifact set
- final unit seams depend on the productive lock/auth design

### 5. DDIC alignment for explicit delta semantics

Target DDIC artifacts in SAP system:
- `ZSTR_PCCT_ROOT_DELTA`
- `ZTAB_PCCT_CHECK_DELTA`
- `ZTAB_PCCT_BARRIER_DELTA`
- `ZTAB_PCCT_PART_DELTA`
- `ZTAB_PCCT_ATTACH_DELTA`

Required alignment:
- `ZSTR_PCCT_ROOT_DELTA` must expose `EDIT_MODE`
- participant and attachment delta structures must expose the fields consumed by the mapper and unified frontend payload
- actual BO node constants and DDIC field names must stay aligned with:
  - `zif_zodata_bopf_mapper`
  - `zcl_zodata_bopf_mapper`

Why this is not completed here:
- repo ABAP sources cannot create or activate DDIC artifacts in the real SAP system
- final field list still must be validated against actual BO node structures and customizing

## System Evidence Work That Cannot Be Solved By Repo Code

### Gateway / FLP / Basis

- `/IWFND/MAINT_SERVICE`
- `/IWBEP/REG_SERVICE`
- SICF path
- system alias
- productive UI5 delivery source for `1.71.x`
- FLP target mapping and catalog assignment
- STRUST / TLS evidence

### Security / PFCG

- authorization object matrix
- role-to-persona matrix
- SU53 / STAUTHTRACE evidence for:
  - allowed open
  - denied open
  - allowed edit
  - denied edit
  - export
  - lock/takeover

### Operations

- ST05 / SAT / ST12 traces for:
  - search
  - detail open
  - save
  - export
- monitoring ownership
- attachment storage / scanning / retention architecture

## Required Final Acceptance Before SAP-Certification Claim

- no unresolved `P1` findings remain
- productive authorization enforcement is proven, not assumed
- productive lock and concurrency behavior is proven, not inferred
- Gateway/FLP registration evidence is complete
- commercialization wording is legally cleared for the chosen GTM contour

## Recommended Execution Order

1. Security/PFCG decides productive authorization model
2. ABAP team implements authorization checks and lock consolidation
3. Basis/Gateway team provides service registration and FLP evidence
4. ABAP team proves ETag/conflict/save/lock traces
5. Product/legal team confirms wording for:
   - SAP-compatible sale
   - official SAP-certified/partner route
