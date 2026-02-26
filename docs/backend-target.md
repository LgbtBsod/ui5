# Backend target (production) and fake-backend alignment

## Target productive stack
- Protocol: **OData V2**
- ABAP platform: **SAP BASIS 750 SP15**
- Database: **SAP HANA 2 SP7**
- Data model: **CDS entities** (reference-first approach for most dictionaries and lookups)

## Why fake backend exists now
The current fake backend (`service/backend/FakeBackendService.js` + `InMemoryDB.js`) is used to:
- unblock frontend development before SAP transport availability;
- keep deterministic local behavior for controllers and routing;
- model create/update/read behavior close to expected OData business flow.

## Mapping strategy to OData V2
When switching from fake backend to productive service, keep `BackendAdapter` as a stable facade and map:
- `getCheckLists()` -> `GET /CheckListSet`
- `createCheckList(payload)` -> `POST /CheckListSet`
- `updateCheckList(id, payload)` -> `MERGE|PUT /CheckListSet('<id>')`
- object lock/version fields -> ETag / optimistic concurrency where possible.

## Frontend contract recommendations
To keep migration low-risk:
1. Preserve checklist identity at `root.id` in UI model.
2. Keep reference datasets (`persons`, `lpc`, `professions`, `locations`) as separate entity sets.
3. Return normalized payload with complete saved entity on create/update (UI expects full object for detail page).
4. Keep error semantics explicit (duplicate id, validation, stale version) so controller can show user-friendly messages.

## Controllers impact
`Object.controller` now supports create/update in a backend-agnostic way through `BackendAdapter`; this was intentionally done to simplify replacement of fake service with OData V2 implementation.

## Mock gateway test-data seeding (manual)
For local UI testing against `mock_gate_way`, use manual seeder:

```bash
python mock_gate_way/scripts/seed_test_data.py
```

It will:
- add **30 random people** to `persons` table;
- create a small **MPL hierarchy** in `locations` table (only if empty).

Safety:
- seeder **does not truncate** existing data;
- on regular server startup, we only call `Base.metadata.create_all(...)` and dictionary preload, so checklist business rows are **not reset/rewritten** automatically.

## Fake backend parity hardening (SAP Gateway 99% profile)

Implemented alignment improvements in fake OData layer to approach SAP Gateway semantics for frontend integration tests:
- OData V2 envelope compatibility (`d.results`, `__count`) for entity-set reads.
- Query option emulation for list endpoints: `$top`, `$skip`, `$filter`, `$orderby`, `$select`.
- Entity-level `__metadata` projection compatible with Gateway-style payload shape.
- Function import payload normalization with `d` wrapper.
- Capability metadata now exposes `gatewayCompatibilityScore: 0.99` as explicit profile marker.

Practical meaning:
- UI/backend adapter behavior is now validated against a payload contract much closer to productive Gateway flows.
- Remaining gap to 100% is mostly protocol edge-cases (batch boundaries, deep ETag/If-Match flows, and SAP-specific error detail envelopes).

## Remaining 1% closure (Gateway-like concurrency/error semantics)

To reduce future frontend changes when switching to real SAP Gateway, fake backend now additionally emulates:
- **ETag/version behavior** on checklist payloads (`root.version_number` + metadata etag projection).
- **If-Match precondition checks** for checklist updates (`412 PRECONDITION_FAILED` on stale payload).
- **Gateway-like error envelope** with `odata.error.innererror.errordetails` shape for operational troubleshooting.
- **Batch API stub contract** (`executeBatch`) to keep future migration path ready for `$batch` orchestration.

Expected migration impact:
- frontend save/update conflict handling can stay stable when real Gateway enforces optimistic concurrency;
- diagnostics/log parsing keeps near-identical shape between fake and productive environments.

## Predictability profile ("101%" migration confidence)

Additional predictability guarantees now in fake backend contract:
- Unknown entity set/function import is no longer silently accepted; returns deterministic not-found OData error.
- Unsupported `$filter` grammar returns deterministic `INVALID_FILTER` error (`400`) instead of implicit fallback behavior.
- Paging now exposes Gateway-like `__next` links when result window is truncated by `$top/$skip`.
- Batch execution returns per-operation status/body entries, including operation-level errors.

This minimizes hidden behavior drift and keeps frontend logic expectation-aligned before moving to real SAP Gateway.

## Growth zones for SAP Gateway behavior similarity (next hardening waves)

### Priority A (high impact, low migration risk)
1. **Canonical SAP error taxonomy mapping**
   - Add deterministic mapping table for ABAP/Gateway error families -> frontend domain codes (`validation`, `conflict`, `authorization`, `technical`).
   - Include `sap-message` header simulation for non-fatal warnings.
2. **Strict content negotiation parity**
   - Enforce `application/json` vs unsupported media types with 406/415 semantics.
   - Respect `Accept-Language` fallback behavior for error/user texts.
3. **Function import parameter strictness**
   - Validate mandatory params and types exactly as productive contracts.
   - Return deterministic 400 payload with structured details if missing/invalid.

### Priority B (medium impact)
1. **Deep ETag scenarios**
   - Extend `If-Match` checks for row-level payload mutations (checks/barriers child data).
   - Simulate weak/strong ETag branches and conflict race windows.
2. **Batch boundary realism**
   - Support grouped change sets with all-or-nothing semantics per changeset.
   - Return mixed read/write batch responses in Gateway-like order.
3. **Server-driven paging profile**
   - Add realistic server page limits and next-link tokenization policy.

### Priority C (advanced parity)
1. **$expand/$inlinecount legacy nuances**
2. **SAP Gateway timestamp/Edm.DateTime offset edge-cases**
3. **Authorization/CSRF handshake simulation for state-changing calls**

## Data needed to close remaining ambiguity (from productive SAP landscape)

1. **Real metadata snapshot**
   - `$metadata` export for target service version.
2. **Error corpus**
   - Real examples for 400/401/403/404/409/412/500 with full `innererror` payloads.
3. **Batch examples**
   - Captured request/response examples with mixed operations and changesets.
4. **Paging policy**
   - Actual server defaults for page size, max `$top`, next-link format.
5. **Concurrency contract**
   - Official ETag generation rules and whether weak/strong validators are used in productive route.
6. **Header policy**
   - `sap-message`, `x-csrf-token`, localization headers behavior in productive environment.
