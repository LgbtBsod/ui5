# Target DDIC / CDS / BO Specification Draft

Date: 2026-03-13

Status: draft for review and correction.

Purpose: provide a concrete implementation specification for SAP-side data modeling around the Production Control Checklist solution. This draft is intentionally aligned to the current repo contract and BOPF mapper expectations, but it is not yet the final productive SAP truth. It should be reviewed and corrected against the real BO, namespace, and CDS strategy.

This file is the design companion to:
- `backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap`
- `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap`
- `backend/sap_backend/src/zcl_zodata_bopf_mapper.clas.abap`
- `backend/sap_backend/src/zif_zodata_bopf_mapper.intf.abap`
- `app/service/contracts/DeltaContracts.js`
- `app/service/shared/DeltaPayloadBuilder.js`

## 1. Scope

This draft covers:
- deep save/autosave contract for:
  - root
  - checks
  - barriers
  - participants
  - attachments
- target DDIC structures and table types
- proposed transparent table ownership
- proposed CDS stack
- BOPF node mapping assumptions

This draft does not claim to define:
- final productive authorization objects
- final package/namespace
- final attachment repository/storage technology
- final transport landscape

## 2. Canonical Business Object Shape

### BO root
- Business object key: `PCCT_UUID`
- Object identity in frontend contract:
  - `pcct_uuid`
  - legacy aliases may still exist in transport payloads, but SAP-side canonical field should be `PCCT_UUID`

### Child nodes
- `CHECKS`
- `BARRIERS`
- `PARTICIPANTS`
- `ATTACHMENTS`

### Deep delta contract

The sanctioned mutable payload shape is:

- `ROOT`
- `CHECKS`
- `BARRIERS`
- `PARTICIPANTS`
- `ATTACHMENTS`
- `CLIENT_VERSION`

All mutable rows carry:
- `EDIT_MODE = C | U | D`

Compatibility rule:
- `ROOT-EDIT_MODE` may be initial during migration and temporarily defaults to `U` in mapper logic

## 3. Proposed Transparent Tables

These are the proposed persistence owners. Names are placeholders and should be adjusted to the real namespace.

### 3.1 Root table

Proposed table:
- `ZPCCT_HDR`

Purpose:
- one row per checklist root

Key:
- `MANDT`
- `PCCT_UUID` RAW16

Core fields:
- `CHECKLIST_ID` CHAR / NUMC depending on business rule
- `STATUS`
- `CHANGED_ON` TIMESTAMPL
- `CREATED_ON` TIMESTAMPL
- `VERSION_NUMBER` INT4
- `DATE_CHECK`
- `TIME_CHECK`
- `TIME_ZONE`
- `LPC`
- `EQUIPMENT`
- `LOCATION_KEY` RAW16 or CHAR depending master model
- `LOCATION_NAME`
- `LOCATION_TEXT`
- `CHECKS_NUMBER`
- `BARRIERS_NUMBER`
- `OBSERVER_FULLNAME`
- `OBSERVER_PERNR`
- `OBSERVER_POSITION`
- `OBSERVER_ORGUNIT`
- `OBSERVED_FULLNAME`
- `OBSERVED_PERNR`
- `OBSERVED_POSITION`
- `OBSERVED_ORGUNIT`
- audit/admin fields as required

### 3.2 Checks table

Proposed table:
- `ZPCCT_CHECK`

Key:
- `MANDT`
- `CHECK_UUID` RAW16

Foreign key:
- `PCCT_UUID` -> `ZPCCT_HDR-PCCT_UUID`

Core fields:
- `CHECKS_NUM`
- `TEXT`
- `COMMENT`
- `RESULT` ABAP_BOOL / XFELD
- `CHANGED_ON`

### 3.3 Barriers table

Proposed table:
- `ZPCCT_BARRIER`

Key:
- `MANDT`
- `BARRIER_UUID` RAW16

Foreign key:
- `PCCT_UUID`

Core fields:
- `BARRIERS_NUM`
- `TEXT`
- `COMMENT`
- `RESULT`
- `CHANGED_ON`

### 3.4 Participants table

Proposed table:
- `ZPCCT_PART`

Key:
- `MANDT`
- `PART_UUID` RAW16

Foreign key:
- `PCCT_UUID`

Core fields:
- `PART_NUM`
- `ROLE_CODE`
- `FULLNAME`
- `PERNR`
- `POSITION`
- `ORGUNIT`
- `CHANGED_ON`

Note:
- if participants are semantically split into observer / observed / approver, role modeling can stay in `ROLE_CODE` instead of creating multiple child tables

### 3.5 Attachments table

Proposed table:
- `ZPCCT_ATTACH`

Key:
- `MANDT`
- `ATTACH_UUID` RAW16

Foreign key:
- `PCCT_UUID`

Core fields:
- `PARENT_KEY` RAW16
- `FOLDER_KEY` RAW16
- `CATEGORY_KEY` RAW16 or CHAR depending content model
- `FILE_NAME`
- `MIME_TYPE`
- `DESCRIPTION`
- `FILE_SIZE`
- `CONTENT_REF` or repository handle
- `CHANGED_ON`

Important:
- binary content should not necessarily be stored directly in the same table if ArchiveLink/DMS/KPro/content server is planned

## 4. Proposed DDIC Structures For Deep Delta Contract

These structures match the current repo contract and mapper assumptions.

### 4.1 Save request root structure

Structure:
- `ZSTR_PCCT_SAVECHANGES_RQ`

Fields:
- `ROOT` TYPE `ZSTR_PCCT_ROOT_DELTA`
- `CHECKS` TYPE `ZTAB_PCCT_CHECK_DELTA`
- `BARRIERS` TYPE `ZTAB_PCCT_BARRIER_DELTA`
- `PARTICIPANTS` TYPE `ZTAB_PCCT_PART_DELTA`
- `ATTACHMENTS` TYPE `ZTAB_PCCT_ATTACH_DELTA`
- `CLIENT_VERSION` TYPE INT4

### 4.2 Root delta structure

Structure:
- `ZSTR_PCCT_ROOT_DELTA`

Minimum fields:
- `PCCT_UUID`
- `EDIT_MODE`
- `STATUS`
- `CHECKLIST_ID`
- `LPC`
- `DATE`
- `TIME_CHECK`
- `TIME_ZONE`
- `EQUIPMENT`
- `LOCATION_KEY`
- `LOCATION_NAME`
- `LOCATION_TEXT`
- `CHECKS_NUMBER`
- `BARRIERS_NUMBER`
- `OBSERVER_FULLNAME`
- `OBSERVER_PERNR`
- `OBSERVER_POSITION`
- `OBSERVER_ORGUNIT`
- `OBSERVED_FULLNAME`
- `OBSERVED_PERNR`
- `OBSERVED_POSITION`
- `OBSERVED_ORGUNIT`

### 4.3 Check delta structure

Structure:
- `ZSTR_PCCT_CHECK_DELTA`

Fields:
- `CHECK_UUID`
- `CLIENT_ROW_ID`
- `EDIT_MODE`
- `CHECKS_NUM`
- `TEXT`
- `COMMENT`
- `RESULT`

Table type:
- `ZTAB_PCCT_CHECK_DELTA`

### 4.4 Barrier delta structure

Structure:
- `ZSTR_PCCT_BARRIER_DELTA`

Fields:
- `BARRIER_UUID`
- `CLIENT_ROW_ID`
- `EDIT_MODE`
- `BARRIERS_NUM`
- `TEXT`
- `COMMENT`
- `RESULT`

Table type:
- `ZTAB_PCCT_BARRIER_DELTA`

### 4.5 Participant delta structure

Structure:
- `ZSTR_PCCT_PART_DELTA`

Fields:
- `PART_UUID`
- `CLIENT_ROW_ID`
- `EDIT_MODE`
- `PART_NUM`
- `ROLE_CODE`
- `FULLNAME`
- `PERNR`
- `POSITION`
- `ORGUNIT`

Table type:
- `ZTAB_PCCT_PART_DELTA`

### 4.6 Attachment delta structure

Structure:
- `ZSTR_PCCT_ATTACH_DELTA`

Fields:
- `ATTACH_UUID`
- `CLIENT_ROW_ID`
- `EDIT_MODE`
- `ROOT_KEY`
- `PARENT_KEY`
- `FOLDER_KEY`
- `CATEGORY_KEY`
- `FILE_NAME`
- `MIME_TYPE`
- `DESCRIPTION`
- `FILE_SIZE`
- `VALUE`

Table type:
- `ZTAB_PCCT_ATTACH_DELTA`

Note:
- `VALUE` in the request contract is the transport field for staged binary payloads
- productive implementation may transform it into repository storage before persistence

### 4.7 Save response structure

Structure:
- `ZSTR_PCCT_SAVECHANGES_RS`

Fields:
- `PCCT_UUID`
- `CHANGED_ON`
- `VERSION_NUMBER`
- `IS_AUTOSAVE`
- `NO_CHANGES`
- `MESSAGES` TYPE `ZTAB_PCCT_SERVICE_MSG`

## 5. Proposed BOPF Node Mapping

Current repo assumptions from mapper:

- root node:
  - ext: `ZSTR_PCCT_ROOT_DELTA`
  - int: `ZSTR_BO_ROOT`
- checks node:
  - ext: `ZSTR_PCCT_CHECK_DELTA`
  - int: `ZSTR_BO_CHECK`
- barriers node:
  - ext: `ZSTR_PCCT_BARRIER_DELTA`
  - int: `ZSTR_BO_BARRIER`
- participants node:
  - ext: `ZSTR_PCCT_PART_DELTA`
  - int: `ZSTR_BO_PARTICIPANT`
- attachments node:
  - ext: `ZSTR_PCCT_ATTACH_DELTA`
  - int: `ZSTR_BO_ATTACHMENT`

Required BOPF constants:
- `SC_NODE-ROOT`
- `SC_NODE-CHECKS`
- `SC_NODE-BARRIERS`
- `SC_NODE-PARTICIPANTS`
- `SC_NODE-ATTACHMENTS`
- `SC_ASSOCIATION-ROOT-CHECKS`
- `SC_ASSOCIATION-ROOT-BARRIERS`
- `SC_ASSOCIATION-ROOT-PARTICIPANTS`
- `SC_ASSOCIATION-ROOT-ATTACHMENTS`

Review required:
- confirm exact node names in productive BO
- confirm whether attachments are directly under root or under a generic document node

## 6. Proposed CDS Stack

User noted that CDS is planned. This section is therefore intentionally structured as a draft stack.

### 6.1 Interface views

Proposed:
- `ZI_PCCT_HDR`
- `ZI_PCCT_CHECK`
- `ZI_PCCT_BARRIER`
- `ZI_PCCT_PART`
- `ZI_PCCT_ATTACH`

Purpose:
- stable technical projection over transparent tables
- no UI semantics

### 6.2 Composite / join views

Proposed:
- `ZI_PCCT_ROOT_WITH_COUNTS`
- `ZI_PCCT_SEARCH`
- `ZI_PCCT_DETAIL`
- `ZI_PCCT_EXPORT`

Purpose:
- compose root with counters, text joins, status texts, master data names

### 6.3 Consumption views

Proposed:
- `ZC_PCCT_SEARCH`
- `ZC_PCCT_DETAIL`
- `ZC_PCCT_ANALYTICS`
- `ZC_PCCT_MPL_TREE`

Purpose:
- consumption/public projection for Gateway/OData or analytical use

### 6.4 Analytics views

If analytics are to be moved toward CDS:
- `ZI_PCCT_KPI_MONTH`
- `ZI_PCCT_KPI_BY_SOURCE`
- `ZI_PCCT_KPI_BY_LOCATION`
- `ZC_PCCT_ANALYTICS_DASHBOARD`

These should remain separate from transactional save structures.

## 7. OData Contract Recommendations

### Read entities
- `ChecklistRoot`
- `ChecklistChecks`
- `ChecklistBarriers`
- `ChecklistParticipants`
- `ChecklistAttachments`

### Function imports
- `LockAcquire`
- `LockHeartbeat`
- `LockRelease`
- `AutoSave`
- `SaveChanges`
- `MplTree`

Recommendation:
- keep save/autosave as function imports if BOPF modify orchestration remains custom
- freeze request/response shape after SAP-system validation

## 8. Authorization Model Draft

This is not the final auth-object design, only the operational split.

Operations requiring explicit auth decision:
- display/search/open
- edit/lock acquire
- autosave/savechanges
- delete
- export
- analytics if sensitive

Possible auth dimensions:
- plant / location
- object status
- action type
- persona

Final auth object design must be confirmed by Security/PFCG.

## 9. Locking / Concurrency Draft

Required productive truths:
- one authoritative lock model
- heartbeat and expiry rule
- stale lock cleanup
- takeover rule
- ETag on root
- `If-Match` enforcement on save

The repo currently documents these expectations, but productive proof still requires SAP-system traces.

## 10. Open Decisions For Review

These are the items the user should correct in this draft.

### Data model
- Are attachments direct root children, or linked through a document node?
- Are participants true child rows, or only flattened observer/observed fields on root?
- Are `LOCATION_KEY`, `FOLDER_KEY`, `CATEGORY_KEY` binary in SAP, or business CHAR IDs?
- Is `CHECKLIST_ID` business-generated or derived from `PCCT_UUID`?

### CDS
- Will search/detail/export be served from CDS directly, or through custom Gateway provider logic?
- Will analytics stay custom, or move to CDS analytical stack?
- Are RAP/CDS projections planned later, or is classic Gateway+BOPF the fixed target?

### Persistence
- Where does attachment binary content live:
  - same table
  - content server
  - ArchiveLink
  - DMS/KPro
- What is the retention/scanning model?

## 11. Recommended Next Review Pass

Review this file and mark:
- correct field names
- fields that should be removed
- fields that are missing
- which parts should become CDS interface/composite/consumption views
- which keys are RAW16 vs CHAR
- which children really exist in the productive BO

After that, this draft can be converted into:
- final DDIC inventory
- final CDS inventory
- final BO node/config matrix
- final implementation task list for ABAP
