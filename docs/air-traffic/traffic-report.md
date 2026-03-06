# Architecture Air Traffic Report

## Queue state
- flights: 3
- next: FP-2026-03-02-002

## Conflicts
- total conflict pairs: 1
- deadlocks: 1
- FP-2026-03-02-001 <-> FP-2026-03-02-003 => Merge FP-2026-03-02-001 first with reduced scope, then rebase FP-2026-03-02-003

## Scheduling decisions
- exclusive: FP-2026-03-02-002 (HIGH-risk exclusive window)
- sequential: FP-2026-03-02-003 (MED-risk sequential merge)
- sequential: FP-2026-03-02-001 (post-exclusive sequencing)

## Architecture impact
- Flights per week: 3
- Denied flights: 0
- Conflict rate: 0.33
- Architecture score trend (ADT): 90

## Merge coordinator clearance
- FP-2026-03-02-002: ALLOW_MERGE
