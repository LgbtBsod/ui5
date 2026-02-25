# Usecase/Coordinator Migration Guide

## Goal
Incrementally move orchestration, decision and policy logic from UI controllers to dedicated `service/usecase` and `util/*Coordinator` modules.

## Layering Rules
- `controller/*`: UI events, model binding, navigation only.
- `service/usecase/*`: business decisions and command flow orchestration.
- `util/*`: pure helpers/formatters/coordinators without UI ownership.

## Migration Checklist
1. Identify repeated branch logic in controller (`if/else` policy blocks, retries, conflict flows).
2. Extract as pure function in usecase module with dependency injection via callback args.
3. Keep controller as adapter: pass callbacks/models to usecase and apply result.
4. Add/extend smoke tests in `scripts/unit-smoke.js` for each new usecase.
5. Validate via `node scripts/unit-smoke.js --json` and `node scripts/ci-smoke-gate.js`.

## Recommended Commit Pattern
- Commit 1: extract usecase with no behavior change.
- Commit 2: switch controller to use the usecase.
- Commit 3: add smoke tests and update roadmap docs.
