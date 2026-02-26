# Rollback playbook (Search/Detail critical flows)

## Triggers
- Lock lifecycle regression (acquire/release/killed handling).
- Save conflict branch regressions.
- Search smart/fallback parity degradation.

## Rollback steps
1. Revert latest wave commit(s) in reverse order (E -> D) using atomic commit boundaries.
2. Run smoke baseline:
   - `node scripts/unit-smoke.js`
   - `node scripts/unit-smoke.js --json > /tmp/unit-smoke-report.json`
   - `node scripts/ci-smoke-gate.js /tmp/unit-smoke-report.json`
3. Re-run governance gates:
   - `node scripts/wave-d-regression-gate.js /tmp/unit-smoke-report.json`
   - `node scripts/wave-e-release-governance-gate.js`
4. Publish incident note with root cause, affected flows, and restoration timestamp.

## Safe-state policy
- If lock/edit uncertainty exists, force READ mode + disabled mutating actions until next stable deployment.
