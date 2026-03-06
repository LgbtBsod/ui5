# Architecture Evolution Engine (AEE) Spec

## Goal
AEE analyzes architecture history, detects architectural drift, discovers stable/deprecated patterns, and proposes advisory RFCs for governance.

## Guarantees
- AEE does **not** modify runtime code.
- AEE does **not** mutate constitution automatically.
- AEE is an architectural advisor only: RFC + mission + policy-change proposals.

## Inputs
- flight logs (`docs/air-traffic/open-flights.json`)
- economy ledger (`udos/economy/ledger/ledger.jsonl`)
- telemetry (`udos/memory/events/udos-events.jsonl`)
- risk profiles (`udos/memory/models/module-risk-profiles.json`)
- duplication reports (`udos/dashboards/duplication.md`)
- court decisions (`udos/reports/court-verdict.md`)

## Outputs
- `udos/evolution/models/architecture-history.json`
- `udos/evolution/models/pattern-library.json`
- `udos/evolution/reports/drift-report.md`
- `udos/evolution/reports/pattern-report.md`
- `udos/evolution/reports/evolution-rfc.md`
- `udos/dashboards/evolution-dashboard.md`

## CI behavior
AEE never fails CI because of drift itself; it generates advisory outputs and RFC proposals.
