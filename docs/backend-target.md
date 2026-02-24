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
