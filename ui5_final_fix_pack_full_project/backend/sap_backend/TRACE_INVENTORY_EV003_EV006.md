# Trace Inventory For EV-003 / EV-004 / EV-005 / EV-006

Date: 2026-03-14

Purpose: define the minimum acceptable trace and screenshot inventory for the highest-risk sale-readiness evidence items.

This file removes ambiguity about what counts as sufficient proof for:

- `EV-003` Authorization allow/deny
- `EV-004` Lock lifecycle
- `EV-005` Optimistic concurrency
- `EV-006` FLP launch

## General Rules

- Screenshot-only proof is not sufficient for `EV-003`, `EV-004`, or `EV-005`.
- Each evidence item must have at least one filled proof record based on `SYSTEM_PROOF_REGISTER_TEMPLATE.md`.
- Each proof set must identify:
  - landscape
  - service root
  - user/role used
  - app version or transport state
- If a trace is captured from multiple tools, the proof record must say which tool is authoritative.

## EV-003 Authorization Allow / Deny

### Minimum acceptable proof

- one allow trace for display/open
- one deny trace for display/open
- one allow trace for edit/lock acquire
- one deny trace for edit/lock acquire
- one allow or deny trace for export, depending on business role model

### Acceptable artifacts

- `STAUTHTRACE` export or screenshots
- `SU53` evidence for denied case
- Gateway request screenshot or payload excerpt showing the denied/allowed operation
- controlled business error response for deny case

### Not sufficient on its own

- UI toast screenshot only
- backend code screenshot only
- role spreadsheet without runtime trace

## EV-004 Lock Lifecycle

### Minimum acceptable proof

- lock acquire success
- heartbeat success
- normal lock release success
- stale-lock cleanup or timeout release
- takeover or deny-on-conflict scenario

### Acceptable artifacts

- Gateway request/response capture for `LockAcquire`, `LockHeartbeat`, `LockRelease`
- ABAP trace from SAT/ST12 or equivalent
- timestamped test protocol showing timeout window and resulting state
- conflict response payload or screenshot

### Not sufficient on its own

- single happy-path lock screenshot
- code comment claiming timeout behavior
- mock backend run

## EV-005 Optimistic Concurrency

### Minimum acceptable proof

- root entity publishes the expected ETag on live Gateway
- save with valid `If-Match` succeeds
- save with stale `If-Match` fails in a controlled way
- concurrent-user conflict is demonstrated with two sessions

### Acceptable artifacts

- HTTP request/response capture with ETag and `If-Match`
- Gateway trace or browser network capture
- conflict screenshot showing business-safe failure
- version/timestamp comparison before and after save

### Not sufficient on its own

- metadata claim without request capture
- single-user save trace only
- code review statement without two-session proof

## EV-006 FLP Launch

### Minimum acceptable proof

- app launches from real FLP tile or target mapping
- semantic object/action matches agreed inbound target
- runtime requests hit the intended Gateway service
- runtime source for UI5 is the productive SAP system delivery, not the temporary public CDN

### Acceptable artifacts

- FLP tile screenshot
- target mapping screenshot/export
- launch URL capture
- browser network capture or Gateway trace showing service root
- runtime source proof for `sap-ui-core.js`

### Not sufficient on its own

- standalone app `index.html` launch
- local dev server screenshot
- target mapping screenshot without launch proof

## Review Rule

An evidence item `EV-003`, `EV-004`, `EV-005`, or `EV-006` cannot be marked `ACCEPTED` until:

- the minimum acceptable proof list is complete
- artifacts are linked from a proof record
- the accountable reviewer confirms that the proof came from the real SAP landscape, not the local mock contour
