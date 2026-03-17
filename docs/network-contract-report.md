# Network Contract Verifier Report

Mode: INTENT
Trace source: synthetic intent

| Check | Status |
|---|---|
| metadata | PASS |
| batch list | PASS |
| server-side params | PASS |
| segments | PASS |
| no expand | PASS |
| no REST | PASS |
| duplicate requests | SKIPPED |
| runtime forbidden patterns | PASS |

## Duplicate Requests
- SKIPPED in INTENT mode (real runtime trace not found).

## Trace artifact
- `docs/network-trace.json`

## Runtime Contract Note
- Lock, save, and autosave responses are expected to expose canonical fields such as `ok`, `code`, `lock_expires_at`, `server_now`, and `lock_refreshed`.
- Cache validation is stamp-based and compares backend aggregate change stamp to cached snapshot stamp with tolerance.
- Migration to a real SAP Gateway backend should preserve these field names and timing semantics to avoid frontend drift.
