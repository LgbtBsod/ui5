# SAP Evidence Runbook

Date: 2026-03-14

Purpose: provide a single execution runbook for the live SAP evidence session so the team can collect `EV-003`, `EV-004`, `EV-005`, `EV-006`, and `EV-010` without argument about sequence, owners, or acceptable outputs.

## Scope

- `EV-003` authorization allow / deny
- `EV-004` lock lifecycle
- `EV-005` optimistic concurrency
- `EV-006` FLP launch
- `EV-010` accessibility keyboard / focus

## Session Roles

- Solution architect: controls entry / exit criteria and confirms evidence quality
- ABAP owner: runs lock, payload, and concurrency steps and captures backend traces
- Basis / Gateway owner: proves FLP launch, service registration, runtime source
- Security / PFCG owner: proves allow / deny scenarios
- UX / QA owner: runs keyboard / sticky walkthrough
- Recorder: fills proof records live and stores screenshot, trace, and timestamp references

## Pre-Flight Gate

Before the live session starts, confirm:

- target SAP system is reachable
- FLP tile and target mapping exist
- Gateway service is active
- two test users or two sessions are available
- `STAUTHTRACE`, `SU53`, and `SAT` or `ST12` are available
- browser network capture can be exported
- the following proof records are opened and ready to fill:
  - [EV-003_AUTHORIZATION_ALLOW_DENY.md](/C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/proof_records/EV-003_AUTHORIZATION_ALLOW_DENY.md)
  - [EV-004_LOCK_LIFECYCLE.md](/C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/proof_records/EV-004_LOCK_LIFECYCLE.md)
  - [EV-005_OPTIMISTIC_CONCURRENCY.md](/C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/proof_records/EV-005_OPTIMISTIC_CONCURRENCY.md)
  - [EV-006_FLP_LAUNCH.md](/C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/proof_records/EV-006_FLP_LAUNCH.md)
  - [EV-010_ACCESSIBILITY_KEYBOARD_FOCUS.md](/C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/proof_records/EV-010_ACCESSIBILITY_KEYBOARD_FOCUS.md)

If any pre-flight item is missing, do not start evidence capture.

## Recommended Execution Order

1. `EV-006` FLP launch
2. `EV-003` authorization allow / deny
3. `EV-004` lock lifecycle
4. `EV-005` optimistic concurrency
5. `EV-010` accessibility keyboard / focus

## Execution Steps

### Step 1: FLP Launch (`EV-006`)

- open the FLP tile
- capture:
  - tile
  - catalog / target mapping
  - launched app state
  - network proof that runtime comes from SAP system, not public CDN
  - service registration evidence
- write result into [EV-006_FLP_LAUNCH.md](/C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/proof_records/EV-006_FLP_LAUNCH.md)

### Step 2: Authorization (`EV-003`)

- run allow user through search/open, edit / lock acquire, and export
- run deny user through blocked path
- capture:
  - `STAUTHTRACE`
  - `SU53`
  - browser response code and business message
- write result into [EV-003_AUTHORIZATION_ALLOW_DENY.md](/C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/proof_records/EV-003_AUTHORIZATION_ALLOW_DENY.md)

### Step 3: Lock Lifecycle (`EV-004`)

- user A opens detail and acquires lock
- user B attempts takeover or blocked edit
- exercise heartbeat
- exercise normal release
- exercise timeout or documented stale-lock cleanup
- capture request sequence, backend trace IDs, and both session screenshots
- write result into [EV-004_LOCK_LIFECYCLE.md](/C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/proof_records/EV-004_LOCK_LIFECYCLE.md)

### Step 4: Optimistic Concurrency (`EV-005`)

- session A reads entity and captures ETag
- session B updates same entity
- session A saves stale payload with stale `If-Match`
- capture initial GET with ETag, successful update, rejected stale update, and user-visible conflict handling
- write result into [EV-005_OPTIMISTIC_CONCURRENCY.md](/C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/proof_records/EV-005_OPTIMISTIC_CONCURRENCY.md)

### Step 5: Accessibility (`EV-010`)

- run keyboard-only search flow
- run `search -> detail -> search` flow
- run mobile-width sticky walkthrough
- confirm no sticky overlap, no focus trap, no hidden primary action
- capture walkthrough video or annotated screenshots
- write result into [EV-010_ACCESSIBILITY_KEYBOARD_FOCUS.md](/C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/proof_records/EV-010_ACCESSIBILITY_KEYBOARD_FOCUS.md)

## File Naming Rule

Use a single naming pattern for exported artifacts:

- `YYYYMMDD_<EVIDENCE_ID>_<LANDSCAPE>_<SHORT_DESC>`

## Acceptance Rule

The session is only considered complete when:

- every executed evidence ID has a filled proof record
- every proof record contains actual artifact names or paths
- no result remains `TO_BE_FILLED`
- the recorder updates:
  - [EVIDENCE_ACCEPTANCE_MATRIX.md](/C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/EVIDENCE_ACCEPTANCE_MATRIX.md)
  - [OWNER_SIGNOFF_TRACKER.md](/C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/OWNER_SIGNOFF_TRACKER.md)

## Failure Rule

Stop the session and raise a blocker if:

- runtime still points to public CDN
- auth denies the baseline allow persona
- lock lifecycle leaves orphaned ownership
- stale `If-Match` update succeeds
- sticky rails hide focused controls in approved runtime
