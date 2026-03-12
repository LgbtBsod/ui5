# SAP Readiness Findings Register

Date: 2026-03-12

Rating model:

- `P1`: blocks certification-style readiness or safe migration to real SAP Gateway
- `P2`: significant gap against SAP best practice or production supportability
- `P3`: lower-severity quality, UX, or governance improvement

## P1 Findings

### P1-01 Non-standard UI5 bootstrap and missing build chain block transportability

- Severity: `P1`
- Area: `UI5 / Delivery model`
- Evidence: `app/index.html:15` to `app/index.html:16`, `app/ui5-bootstrap-runtime.js:5`, `app/ui5-bootstrap-runtime.js:114` to `app/ui5-bootstrap-runtime.js:137`
- Evidence: recursive scan found no `package.json`, `ui5.yaml`, `mta.yaml`, or `pom.xml`
- Impact: the app has no deterministic build artifact story for on-prem FLP deployment, depends on an external CDN, and cannot currently demonstrate SAP-style transportability or reproducible delivery.
- SAP criterion violated: deterministic packaging, platform-aligned deployment, supportable UI5 delivery
- Required remediation: adopt a standard UI5 build/deploy chain, bootstrap UI5 from the target SAP system or an approved internal distribution, generate preload artifacts, and define the productive deployment package for FLP.
- Owner: `UI5`, `Basis/Gateway`

### P1-02 FLP adaptation and personalization readiness are effectively disabled

- Severity: `P1`
- Area: `UI5 / FLP integration`
- Evidence: `app/manifest.json:176`, `app/views/App.view.xml:23` to `app/views/App.view.xml:38`
- Evidence: targeted search across `app` found zero matches for `sap.ushell` or `CrossApplicationNavigation`
- Impact: the app does not demonstrate FLP-native navigation or key-user adaptation readiness and currently replaces platform shell responsibilities with a custom header control.
- SAP criterion violated: FLP integration, adaptation enablement, key-user extensibility
- Required remediation: decide whether this app must be adaptation-enabled; if yes, enable Flex, align navigation with FLP intent handling, and remove or sharply limit custom shell behavior that duplicates FLP responsibilities.
- Owner: `UI5`, `Architecture`, `Basis/Gateway`

### P1-03 Authorization enforcement is not evidenced in the accessible ABAP scope

- Severity: `P1`
- Area: `ABAP / Security`
- Evidence: targeted search across `backend/sap_backend/src` found zero matches for `AUTHORITY-CHECK`
- Evidence: `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap:159` to `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap:266` accept user/session payloads and execute lock and save flows without visible authorization checks
- Impact: the audit cannot prove least-privilege enforcement for create, display, change, delete, export, or lock operations. For certification-style review, lack of evidence is effectively a failure.
- SAP criterion violated: traceable authorization model, role-based access control, auditable enforcement
- Required remediation: implement or document explicit authorization enforcement in Gateway and/or the underlying business object, provide SU24/PFCG mapping, and capture runtime evidence such as SU53 or STAUTHTRACE for each business operation.
- Owner: `ABAP`, `Security`, `Basis/Gateway`

### P1-04 The OData mutation contract is heavily custom and increases migration risk

- Severity: `P1`
- Area: `ABAP Gateway / OData contract`
- Evidence: `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap:435` to `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap:438`
- Evidence: `backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap:105` to `backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap:150`
- Impact: all state-changing behavior is routed through custom function imports and deep payload structures, which makes the service harder to validate, version, and support in a productive Gateway landscape.
- SAP criterion violated: stable service contract, maintainable Gateway design, predictable integration semantics
- Required remediation: freeze and formally document the contract if it must remain custom, or refactor selected operations to standard entity semantics where feasible on the current stack. In either case, produce a contract document with request/response, CSRF, concurrency, error, and versioning rules.
- Owner: `ABAP`, `UI5`, `Architecture`

### P1-05 ETag and optimistic concurrency are not reliably evidenced

- Severity: `P1`
- Area: `ABAP Gateway / Concurrency`
- Evidence: `backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap:157` to `backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap:169` attempt ETag setup against entity name `Checklist`
- Evidence: `app/localService/metadata.xml:10` and `app/localService/metadata.xml:28` expose `ChecklistRoot` and `ChecklistRootSet`, while targeted metadata search found no `m:etag` or `sap:etag`
- Evidence: `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap:402` to `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap:410` increment version in custom save response after immediate commit
- Impact: the productive service may be relying on client-side version tracking without a correctly published OData concurrency contract, which creates lost-update risk in multi-user or multi-tab scenarios.
- SAP criterion violated: optimistic concurrency, Gateway contract correctness
- Required remediation: publish a real ETag on the root entity, enforce `If-Match`, align `version_number` with server truth, and prove behavior with live Gateway traces.
- Owner: `ABAP`, `UI5`

### P1-06 Lock handling is split across multiple custom mechanisms

- Severity: `P1`
- Area: `ABAP / Locking and LUW`
- Evidence: `backend/sap_backend/src/zcl_zodata_lock_manager.clas.abap:39` to `backend/sap_backend/src/zcl_zodata_lock_manager.clas.abap:54`
- Evidence: `backend/sap_backend/src/zfg_zodata_lock.fugr.abap:16` to `backend/sap_backend/src/zfg_zodata_lock.fugr.abap:48`
- Evidence: `backend/sap_backend/src/zcl_lock_manager.clas.abap:72` to `backend/sap_backend/src/zcl_lock_manager.clas.abap:118`
- Evidence: `backend/sap_backend/src/z_lock_regs_async.fugr.abap:12` to `backend/sap_backend/src/z_lock_regs_async.fugr.abap:29`
- Impact: the accessible code shows two lock concepts: one built around enqueue objects and `ztodata_hdr`, another around `zlock_regs` with async RFC and explicit commits. That ambiguity is a major operational and concurrency risk.
- SAP criterion violated: single source of transactional truth, supportable locking design, predictable LUW boundaries
- Required remediation: consolidate on one lock architecture, document heartbeat and timeout semantics, remove duplicate lock stores, and prove cleanup/takeover behavior in Gateway runtime tests.
- Owner: `ABAP`

## P2 Findings

### P2-00 Runtime structure is over-layered and concentrates logic in oversized orchestration modules

- Severity: `P2`
- Area: `UI5 / Architecture`
- Evidence: `app/controller/search/SearchControllerBehavior.js`, `app/controller/analytics/AnalyticsControllerBehavior.js`, `app/controller/detail/DetailViewBehavior.js`, and `app/service/framework/ComponentInitRuntime.js` remain the highest current orchestration-weight frontend modules
- Evidence: the top-weight files are concentrated in `controller/support` and `service/framework`, despite the presence of multiple architectural layers and facades
- Impact: the project looks layered on paper, but significant behavioral ownership is concentrated in large orchestration files. This increases onboarding cost, regression risk, and duplication probability when features evolve.
- SAP criterion violated: maintainable UI architecture, clear separation of concerns, supportable extension model
- Required remediation: split orchestration files into smaller cohesive modules by use case or policy, define hard ownership boundaries between `controller/support`, `service/framework`, `infra/adapters`, and `util`, and enforce file-size and responsibility thresholds.
- Owner: `UI5`, `Architecture`

### P2-01 Current proxy descriptor is misaligned with the target on-prem landscape

- Severity: `P2`
- Area: `Landscape / Routing`
- Evidence: `app/xs-app.json:5` to `app/xs-app.json:15`
- Impact: the repository still carries an approuter-oriented route model with `html5-apps-repo-rt`, `xsuaa`, and `csrfProtection: false`. Even if this is only transitional, it creates deployment ambiguity and a security risk if reused incorrectly.
- SAP criterion violated: landscape consistency, secure routing configuration
- Required remediation: separate local/BTP descriptors from the target on-prem FLP deployment model and treat the current `xs-app.json` as non-productive unless it is fully hardened.
- Owner: `Basis/Gateway`, `UI5`

### P2-02 The Python backend mutates schema and seed data on startup

- Severity: `P2`
- Area: `Python / Transitional backend`
- Evidence: `backend/mock_gateway/main.py:114` to `backend/mock_gateway/main.py:199`
- Evidence: `backend/mock_gateway/main.py:323` to `backend/mock_gateway/main.py:357`
- Evidence: `backend/mock_gateway/database.py:6`
- Impact: test runs can silently change the local contract and data profile, which weakens parity with the productive SAP backend and can hide migration defects.
- SAP criterion violated: controlled change management, deterministic test baseline
- Required remediation: move all schema evolution and seed behavior into explicit local-only scripts, freeze fixtures for parity testing, and fail fast if the backend is started in a non-local profile.
- Owner: `Python`

### P2-03 Mock identity and fallback authorization logic are too permissive

- Severity: `P2`
- Area: `Python / Security parity`
- Evidence: `backend/mock_gateway/config.py:31` to `backend/mock_gateway/config.py:32`
- Evidence: `backend/mock_gateway/services/current_user_service.py:72` to `backend/mock_gateway/services/current_user_service.py:75`
- Evidence: `backend/mock_gateway/services/authorization_service.py:170` to `backend/mock_gateway/services/authorization_service.py:223`
- Impact: identity and authorization behavior can be changed through headers or username markers such as `readonly`, `viewonly`, and `denyedit`, which is useful for local demos but invalid for productive parity.
- SAP criterion violated: realistic authorization simulation, secure identity handling
- Required remediation: hard-disable mock identity outside local dev, replace marker-based authorization with explicit fixture roles, and maintain a clear mapping to productive PFCG concepts.
- Owner: `Python`

### P2-04 Request logging can leak sensitive mutation payloads

- Severity: `P2`
- Area: `Python / Logging`
- Evidence: `backend/mock_gateway/main.py:34` to `backend/mock_gateway/main.py:41`
- Evidence: `backend/mock_gateway/main.py:97` to `backend/mock_gateway/main.py:109`
- Impact: request body logging for mutation traffic and batch operations can expose personnel, checklist comments, and potentially attachment metadata in logs.
- SAP criterion violated: secure logging, privacy-aware operations
- Required remediation: redact or disable payload logging by default, log only correlation IDs and route signatures, and document any exceptional debug mode separately.
- Owner: `Python`

### P2-05 Attachment handling is local-file based and not production-grade

- Severity: `P2`
- Area: `Python / Content handling`
- Evidence: `backend/mock_gateway/api/gateway_canonical_api.py:197` to `backend/mock_gateway/api/gateway_canonical_api.py:214`
- Evidence: `backend/mock_gateway/api/gateway_canonical_api.py:264` to `backend/mock_gateway/api/gateway_canonical_api.py:300`
- Impact: the mock backend writes attachment binaries directly to a filesystem folder and does not evidence the productive SAP concerns of content repository integration, virus scanning, retention, or audit logging.
- SAP criterion violated: secure content handling, operational compliance
- Required remediation: keep this logic local-only, and define the real SAP attachment architecture with storage repository, virus scanning, MIME policy, and retention controls.
- Owner: `Python`, `ABAP`, `Basis/Gateway`

### P2-06 UI regression automation is missing

- Severity: `P2`
- Area: `UI5 / Testability`
- Evidence: targeted search across `app` found zero matches for `QUnit`, `opaTest`, `opaQunit`, or `journeyRunner`
- Impact: the migration to productive Gateway lacks a stable UI regression safety net for navigation, save, lock, and authorization states.
- SAP criterion violated: release quality discipline, regression protection
- Required remediation: add a minimal QUnit suite for runtime helpers and an OPA smoke suite for search, detail, lock, save, and access-denied flows.
- Owner: `UI5`, `QA`

### P2-07 ABAP Unit coverage is absent in the accessible service wrapper scope

- Severity: `P2`
- Area: `ABAP / Testability`
- Evidence: targeted search across `backend/sap_backend/src` found zero matches for `FOR TESTING`, `ABAP Unit`, or `cl_abap_unit_assert`
- Impact: mapper, message helper, lock manager, and DPC helper behavior are not currently backed by executable unit tests in the audited code.
- SAP criterion violated: backend verification discipline
- Required remediation: add ABAP Unit around contract mapping, lock exception handling, message conversion, and save response building.
- Owner: `ABAP`

### P2-08 Productization namespace and app identity are not ready

- Severity: `P2`
- Area: `Productization`
- Evidence: `app/manifest.json:4` to `app/manifest.json:6`, `app/manifest.json:32`
- Impact: the current app ID and namespace style look like an implementation placeholder rather than a governed product namespace. This is weak for transport governance and unacceptable for formal productization without a namespace decision.
- SAP criterion violated: namespace ownership, package governance, product identity hygiene
- Required remediation: define the productive app namespace, align service and package names, and prepare transport/package ownership documentation.
- Owner: `Architecture`, `ABAP`, `UI5`

### P2-09 State and path ownership are split across multiple parallel abstractions

- Severity: `P2`
- Area: `UI5 / State management`
- Evidence: `app/controller/app/AppStateBehavior.js` contains one of the highest current concentrations of state-path usage in the scan
- Evidence: `app/model/StatePaths.js` and `app/service/domain/shared/DomainStatePaths.js` both exist and carry overlapping responsibility for path centralization
- Impact: the codebase has partially centralized state paths, but runtime logic still relies on multiple state-path abstractions and repeated direct path usage. This makes refactoring state shape or debugging binding defects harder than necessary.
- SAP criterion violated: stable view-model design, maintainable application state handling
- Required remediation: converge on one authoritative state-path contract per model, prohibit new raw path literals outside that contract, and separate UI-view state from business workflow state more sharply.
- Owner: `UI5`

### P2-10 Backend contract normalization is duplicated across frontend layers

- Severity: `P2`
- Area: `UI5 / Integration architecture`
- Evidence: high concentrations of `normalize`, `resolve`, `build`, `map`, and primitive coercion patterns appear in `AnalyticsPayloadNormalizer.js`, `WorkflowAnalyticsAdapter.js`, `ODataChecklistRepoAdapter.js`, `ChecklistSnapshotMapper.js`, `SaveDetailUseCase.js`, and multiple controller support files
- Evidence: `app/controller/search/SearchControllerBehavior.js` and `app/infra/adapters/ODataChecklistRepoAdapter.js` both still contain contract-shaping and normalization pressure points
- Impact: mapping, normalization, and fallback rules are not confined to one adapter boundary. This raises the risk of frontend/backend contract drift and inconsistent behavior between search, detail, analytics, and save flows.
- SAP criterion violated: single integration boundary, predictable contract handling
- Required remediation: move all payload normalization and alias compatibility logic into a small number of adapter/mapper modules and keep controller/support code free of transport-shape knowledge.
- Owner: `UI5`

## P3 Findings

### P3-01 Custom shell and theme runtime increase maintenance cost

- Severity: `P3`
- Area: `UI5 / UX supportability`
- Evidence: `app/views/App.view.xml:23` to `app/views/App.view.xml:38`
- Evidence: `app/ui5-bootstrap-runtime.js:23` to `app/ui5-bootstrap-runtime.js:68`
- Impact: theme mode persistence, theme prefetching, and custom shell chrome create extra maintenance surface that FLP and standard UI5 theming already cover in many enterprise scenarios.
- SAP criterion violated: simplicity, maintainability, UX consistency
- Required remediation: reduce custom shell and theme responsibilities to what the business process truly needs, and prefer platform-native behavior where possible.
- Owner: `UI5 UX`

### P3-03 UX consistency is at risk because major UX behavior is implemented in hand-written runtime code

- Severity: `P3`
- Area: `UX / UI implementation`
- Evidence: `app/controls/AppShellHeader.js:122` to `app/controls/AppShellHeader.js:218` manually assembles and synchronizes shell header content
- Evidence: `app/service/framework/ThemeService.js:145` to `app/service/framework/ThemeService.js:219` directly manipulates document classes, storage, and theme transitions
- Evidence: `app/controller/search/SearchControllerBehavior.js` still manually synchronizes parts of SmartFilterBar state and analytics drilldown behavior
- Impact: key UX behavior is implemented through custom runtime code instead of narrowly-scoped reusable patterns. This creates uneven interaction quality, harder accessibility verification, and higher regression risk during visual changes.
- SAP criterion violated: UX consistency, accessible behavior reuse, maintainable interaction model
- Required remediation: identify the 5-10 core UX interaction patterns and formalize them as reusable components/services with clear contracts, instead of continuing to grow hand-coded behavior in support files.
- Owner: `UI5 UX`, `UI5`

### P3-04 The repository's UX/a11y gate itself is not currently reproducible

- Severity: `P3`
- Area: `Engineering governance`
- Evidence: `node scripts/a11y-gate.js --json` failed because required artifacts `docs/DEVELOPMENT_PLAN.md` and `css/claude-hyper.css` are missing
- Impact: the project claims UX/a11y governance through scripts, but at least one gate depends on missing artifacts and therefore cannot currently be trusted as a release signal.
- SAP criterion violated: repeatable quality governance
- Required remediation: either restore the missing artifacts or retire the obsolete gate so that governance scripts reflect the real repository state.
- Owner: `DevEx`, `UI5 UX`

### P3-02 Repository self-check is weakened by a missing validation document

- Severity: `P3`
- Area: `Engineering governance`
- Evidence: `python -m pytest backend/mock_gateway/tests -q` produced `39 passed, 1 failed`
- Evidence: failure reason is missing file `docs/LOCAL_VALIDATION.md`
- Impact: local governance signals lose credibility when the documented validation path is missing from the repository.
- SAP criterion violated: engineering readiness, repeatable validation
- Required remediation: restore `docs/LOCAL_VALIDATION.md` or remove the failing invariant test if the document is obsolete.
- Owner: `DevEx`, `Python`

## Observations That Matter But Need System Evidence

- The repository does not provide enough evidence to confirm real Gateway service registration, system aliasing, PFCG role design, SICF activation, TLS setup, virus scan configuration, package ownership, or transport governance.
- Those items are treated as `evidence gaps`, not as assumed failures. They remain blockers for any formal readiness statement until the evidence request pack is fulfilled.
