# SAP Readiness Remediation Backlog

Date: 2026-03-12

Sequence principle:

1. fix landscape and contract blockers
2. harden ABAP security and concurrency
3. isolate Python to local-dev-only use
4. add regression evidence
5. complete Basis/Gateway evidence for productive go-live and certification-style review

## Wave 1 - Productive Runtime Model

| ID | Priority | Stream | Action | Acceptance | Owner |
| --- | --- | --- | --- | --- | --- |
| UI5-01 | P1 | UI5 | Replace CDN bootstrap with a productive UI5 delivery model served by the target SAP landscape or approved internal distribution. | No productive runtime dependency on `ui5.sap.com`; bootstrap path documented and version-controlled. | UI5, Basis/Gateway |
| UI5-02 | P1 | UI5 | Introduce a standard build/deploy chain for UI5 1.71. | Repository contains the chosen build descriptor and produces deterministic preloadable artifacts for deployment. | UI5 |
| UI5-03 | P1 | UI5 | Align app shell and navigation with on-prem FLP. | FLP launch, back navigation, intent handling, and shell responsibilities are documented and tested. | UI5, Basis/Gateway |
| ARC-01 | P1 | Architecture | Freeze the productive contract boundary: canonical OData service, supported entities, function imports, concurrency rules, error taxonomy, and decommission scope for Python aliases. | One approved contract document exists and the frontend no longer depends on non-canonical mock-only behavior. | Architecture, UI5, ABAP |

## Wave 2 - ABAP Gateway Hardening

| ID | Priority | Stream | Action | Acceptance | Owner |
| --- | --- | --- | --- | --- | --- |
| ABAP-01 | P1 | ABAP | Make authorization enforcement explicit and evidence-backed. | Productive operations have documented authorization objects, PFCG mapping, and runtime trace evidence. | ABAP, Security |
| ABAP-02 | P1 | ABAP | Rationalize the mutation model around Gateway best practice for the current stack. | Each mutation endpoint has documented semantics, CSRF, versioning, and error behavior; unsupported legacy paths are removed or formally deprecated. | ABAP |
| ABAP-03 | P1 | ABAP | Unify locking and LUW behavior into one supported design. | One lock mechanism remains, timeout and takeover behavior are documented, and no duplicate lock stores remain active. | ABAP |
| ABAP-04 | P1 | ABAP | Publish and enforce real optimistic concurrency. | Root entity exposes a real ETag, `If-Match` is enforced, and concurrent update tests pass. | ABAP, UI5 |
| ABAP-05 | P2 | ABAP | Harden attachment architecture for productive SAP operations. | Storage target, MIME policy, size policy, virus scan profile, retention, and audit trail are documented and implemented. | ABAP, Basis/Gateway |
| ABAP-06 | P2 | ABAP | Add ABAP Unit coverage for the audited wrapper layer. | Unit tests cover mapper, lock manager, message conversion, and save response helpers. | ABAP |

## Wave 3 - Python Backend Containment

| ID | Priority | Stream | Action | Acceptance | Owner |
| --- | --- | --- | --- | --- | --- |
| PY-01 | P1 | Python | Declare the mock backend as `local-dev-only` and enforce that boundary in config and docs. | Startup fails outside local-dev profile, and no productive deployment path references the mock backend. | Python |
| PY-02 | P2 | Python | Remove startup schema mutation and seed behavior from normal app boot. | Schema evolution and fixture generation run only from explicit scripts. | Python |
| PY-03 | P2 | Python | Disable `X-Mock-User` and marker-based authorization outside isolated dev scenarios. | Identity is fixture-driven only in local mode and cannot be overridden by simple headers in shared environments. | Python |
| PY-04 | P2 | Python | Remove verbose payload logging from default runtime. | Only redacted, correlation-friendly logs remain enabled by default. | Python |
| PY-05 | P2 | Python | Re-scope attachment handling to local parity testing only. | Local upload storage is clearly marked as non-productive and excluded from any productive architecture claims. | Python |

## Wave 3.5 - Frontend Structure and UX Debt

| ID | Priority | Stream | Action | Acceptance | Owner |
| --- | --- | --- | --- | --- | --- |
| UI5-04 | P2 | UI5 | Break down oversized `controller/support` and `service/framework` orchestration modules. | Search, analytics, detail, and component bootstrap logic are split into smaller cohesive units with explicit ownership. | UI5 |
| UI5-05 | P2 | UI5 | Converge state-path ownership and remove overlapping state abstractions. | One authoritative path contract exists per model and new raw state-path literals are blocked. | UI5 |
| UI5-06 | P2 | UI5 | Collapse duplicated payload normalization into dedicated adapter boundaries. | Controller support modules no longer perform transport-shape normalization that belongs in adapters/mappers. | UI5 |
| UX-01 | P3 | UI5 UX | Rationalize custom shell, theme, and viewport behaviors into a small reusable interaction system. | Core UX patterns are documented, reusable, and independently testable. | UI5 UX |

## Wave 4 - Test and Evidence Upgrade

| ID | Priority | Stream | Action | Acceptance | Owner |
| --- | --- | --- | --- | --- | --- |
| QA-01 | P2 | UI5 QA | Add QUnit coverage for critical runtime helpers and adapter logic. | Basic unit coverage exists for request coordination, contract normalization, and concurrency helpers. | UI5, QA |
| QA-02 | P2 | UI5 QA | Add OPA end-to-end smoke coverage for search, detail, lock, save, and access-denied flows. | Test suite runs against the supported backend test target and blocks regressions in core user flows. | UI5, QA |
| QA-03 | P2 | Python | Restore or replace the missing `docs/LOCAL_VALIDATION.md` invariant. | `python -m pytest backend/mock_gateway/tests -q` runs cleanly with zero failures. | Python, DevEx |
| QA-04 | P2 | Contract | Add metadata parity checks against the real Gateway service. | Frontend and mock parity checks fail on contract drift before release. | UI5, Python, ABAP |
| QA-05 | P3 | DevEx | Repair or retire broken governance gates such as the current a11y gate dependency on missing artifacts. | All retained quality gates are reproducible from a clean checkout. | DevEx, UI5 UX |

## Wave 5 - Basis and Productization Evidence

| ID | Priority | Stream | Action | Acceptance | Owner |
| --- | --- | --- | --- | --- | --- |
| BAS-01 | P1 | Basis/Gateway | Register and evidence the productive Gateway service path. | `/IWFND/MAINT_SERVICE`, `/IWBEP/REG_SERVICE`, system alias, and SICF evidence are captured and approved. | Basis/Gateway |
| BAS-02 | P1 | Security | Finalize role/catalog/group setup for FLP and Gateway. | Catalogs, groups or spaces/pages, PFCG roles, and authorization traces are complete. | Basis/Gateway, Security |
| BAS-03 | P2 | Basis/Gateway | Provide productive transport/package and namespace governance. | Package hierarchy, transport layer, namespace ownership, and deployment path are documented. | Basis/Gateway, Architecture |
| BAS-04 | P2 | Operations | Provide operational controls for TLS, logging, dumps, performance, and virus scanning. | STRUST, ICM, application logging, monitoring, and content scanning evidence is available. | Basis/Gateway, Operations |
| PROD-01 | P2 | Productization | Normalize app and service identity for a governed release. | Productive namespace, app ID, service naming, and legal/license evidence are approved. | Architecture, Product |

## Exit Criteria

- All `P1` backlog items are complete.
- Real Gateway metadata, auth trace, and FLP launch evidence have been captured.
- The frontend runs through the productive bootstrap and deployment model, not the current local bootstrap.
- The Python backend is clearly outside the productive path.
- Regression coverage exists for the critical end-to-end flows.
