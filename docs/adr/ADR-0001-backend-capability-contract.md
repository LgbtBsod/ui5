# ADR-0001: Backend capability contract and negotiation baseline

## Status
Accepted

## Context
The master enterprise plan requires B1/B2 baseline: explicit capability contract, versioning policy, and adapter-level negotiation between fake and real backends.

The current project already supports dual backend modes, but lacked a formalized capabilities response and deterministic adapter negotiation API.

## Decision
1. Introduce a normalized capability contract exposed by backend services and unified by `BackendAdapter`.
2. Add adapter API:
   - `getCapabilities()` → returns normalized capability payload.
   - `negotiateCapabilities(requiredFeatures[])` → returns `{ ok, missingFeatures, capabilities }`.
3. Use semantic contract version `1.0.0` and compatibility metadata (`minUiContractVersion`, `maxUiContractVersion`).
4. Keep behavior-preserving fallback defaults to avoid functional regression when backend capability endpoint is unavailable.
5. Enforce semantic UI contract compatibility and policy validity:
   - min/max policy must be valid (`min <= max`, and range format must be parseable),
   - supported max formats: exact semver (`1.2.3`) or major range (`1.x`),
   - `ensureContractCompatibility(...)` returns deterministic reason codes,
   - `enforceContractCompatibility(...)` fail-fast throws on incompatibility.
6. Enforce fail-fast contract check during `BackendAdapter.init()` using configured UI contract version.

## Semver policy (production-grade)
- **Policy owner**: `service/backend/BackendAdapter.js` only (no controller-level duplication).
- **Validation reasons**:
  - `compatible`
  - `ui_contract_below_min`
  - `ui_contract_out_of_supported_range`
  - `invalid_semver_metadata`
  - `invalid_semver_policy_range`
- **Fail-fast behavior**:
  - startup must fail before backend init side effects if configured UI contract is incompatible,
  - invalid backend policy shape is treated as incompatible contract.

## Migration notes
1. Existing backends that already return `minUiContractVersion`/`maxUiContractVersion` with `1.x` continue to work without behavior change.
2. Backends can gradually migrate `maxUiContractVersion` to exact semver when pinning upper bound is required.
3. CI/pre-push now runs dedicated semver gate (`scripts/backend-semver-policy-gate.js`) in addition to contract drift gate.
4. Contract drift fixtures/gate include exact-upper-bound semver checks (e.g., `maxUiContractVersion = 1.2.3`).
5. Contract drift/smoke include malformed semver metadata branch (`invalid_semver_metadata`) for deterministic rejection.
6. Dedicated semver policy gate also validates malformed semver metadata rejection path.
7. For rollout safety, keep `uiContractVersion` explicit in adapter configuration when app contract is bumped.

## Consequences
### Positive
- Capability negotiation becomes explicit and testable.
- Fake/real backend parity can be validated deterministically.
- Startup incompatibilities are surfaced immediately (fail-fast).

### Trade-offs
- Requires maintenance of capability metadata in both backends.
- Contract version bumps require synchronized update of backend policy and UI configured version.

## Rollout
- Phase 1 (implemented): baseline API and smoke coverage.
- Phase 2 (implemented): contract drift gate with fixtures (`scripts/backend-contract-drift-gate.js`, `scripts/backend-capability-fixtures.json`) and pre-push wiring.
- Phase 3 (implemented): semver compatibility enforcement + supported/unsupported UI version tests.
- Phase 4 (implemented): strict semver policy validation + fail-fast init enforcement + dedicated semver policy gate.

## References
- `service/backend/BackendAdapter.js`
- `service/backend/FakeBackendService.js`
- `service/backend/RealBackendService.js`
- `scripts/unit-smoke.js`
- `scripts/backend-contract-drift-gate.js`
- `scripts/backend-semver-policy-gate.js`
