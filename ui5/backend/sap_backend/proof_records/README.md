# Proof Records

This folder is the execution pack entry point for landscape-only evidence.

## Scope

- `EV-003_AUTHORIZATION_ALLOW_DENY.md`
- `EV-004_LOCK_LIFECYCLE.md`
- `EV-005_OPTIMISTIC_CONCURRENCY.md`
- `EV-006_FLP_LAUNCH.md`
- `EV-010_ACCESSIBILITY_KEYBOARD_FOCUS.md`

## How To Use

1. Copy the prepared starter record for the target landscape execution.
2. Replace every `TO_BE_FILLED` value during live SAP validation.
3. Attach screenshots, traces, payload captures, and transport references in the exact structure required by [TRACE_INVENTORY_EV003_EV006.md](/C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/TRACE_INVENTORY_EV003_EV006.md).
4. Reflect the resulting status in [OWNER_SIGNOFF_TRACKER.md](/C:/Users/lgbtb/Desktop/ui5/backend/sap_backend/OWNER_SIGNOFF_TRACKER.md).

## Rules

- Local mock results are not acceptable substitutes for `EV-003/004/005/006`.
- Each record must end in a concrete `Pass / fail` value.
- Missing trace IDs or screenshot paths mean the record is incomplete even if the scenario was manually observed.
