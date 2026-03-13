# System-Side Execution Checklist

Date: 2026-03-13

Purpose: convert the remaining SAP-system-side readiness work into an execution-ready checklist with owners, inputs, expected evidence, and definition of done.

This file is the operational companion to:
- `backend/sap_backend/IMPLEMENTATION_AND_EVIDENCE_BACKLOG.md`
- `docs/audit/sap-readiness-evidence-request-pack.md`
- `docs/audit/commercial-certification-readiness-dossier.md`

## Workstream 1: DDIC And Gateway Contract Alignment

### Owner
- ABAP developer

### Task
- Align DDIC artifacts with the unified save/autosave delta contract.

### Inputs
- `backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap`
- `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap`
- `backend/sap_backend/src/zif_zodata_bopf_mapper.intf.abap`
- `backend/sap_backend/src/zcl_zodata_bopf_mapper.clas.abap`

### Required SAP objects
- `ZSTR_PCCT_SAVECHANGES_RQ`
- `ZSTR_PCCT_ROOT_DELTA`
- `ZTAB_PCCT_CHECK_DELTA`
- `ZTAB_PCCT_BARRIER_DELTA`
- `ZTAB_PCCT_PART_DELTA`
- `ZTAB_PCCT_ATTACH_DELTA`
- `ZSTR_PCCT_SAVECHANGES_RS`

### Required output
- `root/checks/barriers/participants/attachments` are all present in DDIC.
- `EDIT_MODE` exists on:
  - root delta
  - check delta
  - barrier delta
  - participant delta
  - attachment delta
- live Gateway `$metadata` export from the real service.

### Definition of done
- DDIC objects are active in SAP.
- Gateway metadata shows the intended deep request/response contract.
- DPC can deserialize the live payload without SAP type mismatch dumps.

### Blocking dependencies
- access to the target SAP development system
- ability to activate DDIC and Gateway artifacts

## Workstream 2: Authorization Enforcement

### Owner
- ABAP developer
- Security/PFCG owner

### Task
- Implement and prove productive authorization enforcement for all sensitive flows.

### Inputs
- business personas
- role design
- productive authorization object list
- operation matrix from Product/Security

### Required operations
- search/open
- enter edit / lock acquire
- autosave/savechanges
- delete
- export/report
- MPL/hierarchy reads if restricted

### Required output
- productive `AUTHORITY-CHECK` or equivalent enforcement path in ABAP.
- persona-to-role-to-operation matrix.
- trace evidence for allow and deny scenarios.

### Evidence
- SU53
- STAUTHTRACE
- screenshots or exported trace files

### Definition of done
- every protected operation has one explicit authorization decision point.
- denied cases return controlled business errors, not dumps.
- trace evidence exists for:
  - allowed open
  - denied open
  - allowed edit
  - denied edit
  - export

### Blocking dependencies
- productive authorization object decision
- PFCG role ownership

## Workstream 3: Locking And Concurrency

### Owner
- ABAP developer

### Task
- Consolidate lock truth and prove optimistic concurrency behavior.

### Inputs
- `backend/sap_backend/src/zcl_zodata_lock_manager.clas.abap`
- `backend/sap_backend/src/zcl_lock_manager.clas.abap`
- `backend/sap_backend/src/zfg_zodata_lock.fugr.abap`
- `backend/sap_backend/src/z_lock_regs_async.fugr.abap`
- `backend/sap_backend/src/z_unlock_regs_update.fugr.abap`
- `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap`
- `backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap`

### Required output
- one authoritative lock model
- documented heartbeat / timeout / takeover semantics
- real ETag publication on productive root entity
- `If-Match` handling
- conflict response model

### Evidence
- save conflict trace
- lock conflict trace
- stale-lock cleanup trace
- lock heartbeat trace

### Definition of done
- concurrent save conflict is reproducible and controlled
- stale locks can be released by documented rule
- lock release does not leave orphaned ownership
- ETag and `If-Match` are proven on live Gateway

### Blocking dependencies
- real SAP system with concurrent users/sessions

## Workstream 4: FLP / Gateway / Basis Readiness

### Owner
- Basis/Gateway owner

### Task
- Prove productive deployment contour for the target on-prem FLP + Gateway landscape.

### Required output
- `/IWFND/MAINT_SERVICE`
- `/IWBEP/REG_SERVICE`
- SICF service path
- system alias
- FLP target mapping
- productive UI5 delivery source for `1.71.x`
- TLS/STRUST evidence

### Evidence
- service registration screenshots/exports
- target mapping screenshots/exports
- FLP launch proof
- TLS configuration proof

### Definition of done
- app launches from real FLP
- metadata and runtime requests hit the intended productive Gateway service
- no unresolved alias/routing ambiguity remains

### Blocking dependencies
- access to Basis/Gateway configuration

## Workstream 5: Save / Autosave Live Proof

### Owner
- ABAP developer
- QA / Functional tester

### Task
- Prove that the unified delta contract survives the real SAP stack end-to-end.

### Required scenarios
- root update with `edit_mode = U`
- create-save with `root.edit_mode = C`
- check create/update/delete
- barrier create/update/delete
- participant create/update/delete if active in UI
- attachment create/delete
- autosave using same contract as save

### Evidence
- request payload samples
- Gateway payload deserialization proof
- BOPF `modify` success proof
- returned version/change timestamp proof

### Definition of done
- ABAP mapper builds `lt_modification` from explicit `C/U/D` rows without heuristic guessing
- save and autosave both succeed on the live service
- backward compatibility fallback for missing `root.edit_mode` is either no longer needed or explicitly accepted for migration only

### Blocking dependencies
- live Gateway service
- active DDIC contract
- working lock/auth model

## Workstream 6: Operations And Compliance

### Owner
- Product owner
- Security
- Operations

### Task
- Prepare the evidence pack required for enterprise rollout and certification-style review.

### Required output
- support model
- monitoring owner
- attachment storage/scanning/retention model
- namespace/package ownership
- third-party license review / SBOM
- approved product wording

### Evidence
- support RACI
- monitoring dashboard ownership
- storage/scanning architecture note
- package/namespace decision
- legal wording approval

### Definition of done
- no ambiguity remains about who supports the product
- no ambiguity remains about how attachments are stored, scanned, and retained
- go-to-market wording does not imply SAP endorsement unless formally granted

### Blocking dependencies
- product/legal/security sign-off

## Release Sign-Off Gate

The project should only be treated as release-ready for SAP-compatible enterprise rollout when all of the following are true:

- repo baseline remains green
- DDIC and Gateway contract are active in SAP
- authorization enforcement is proven with traces
- lock and concurrency behavior is proven with traces
- FLP/Gateway deployment proof exists
- support/ops/legal evidence pack is complete

The project should only be treated as SAP-certified / SAP-branded when the above is true and formal SAP partner/certification/branding approval is also complete.
