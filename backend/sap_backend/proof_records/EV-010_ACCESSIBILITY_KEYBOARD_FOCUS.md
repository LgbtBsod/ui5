# Proof Record: EV-010 Accessibility Keyboard Focus

- Evidence ID: EV-010
- Date: 2026-03-14
- Landscape: TO_BE_FILLED
- Service / App version: TO_BE_FILLED
- Owner: UX / QA
- Reviewer: Product owner
- Status: OPEN

## Scenario

- Business scenario: keyboard-only and focus-stable usage across search and detail
- Technical scenario: sticky rail, route transition, and mobile-width accessibility proof in supported runtime
- Preconditions:
  - FLP-hosted runtime available
  - supported desktop browser available
  - one mobile-width validation path agreed
- Users / roles involved:
  - QA persona: TO_BE_FILLED
  - product reviewer: TO_BE_FILLED

## Execution

- Launch path: approved FLP target
- Request sequence:
  - open search
  - execute search and move through sticky filters/actions/results
  - open detail from results
  - navigate sections and action rail
  - return to search and confirm focus continuity
- Reference checklist:
  - [STICKY_KEYBOARD_MOBILE_CHECKLIST.md](/C:/Users/lgbtb/Desktop/ui5/docs/audit/STICKY_KEYBOARD_MOBILE_CHECKLIST.md)
- Mandatory artifact minimum:
  - one desktop keyboard walkthrough capture
  - one mobile-width walkthrough capture
  - one defect log entry set or explicit `no defect` statement
  - one accepted mapping from findings to fixed build/version

## Collected Artifacts

- Screenshot paths: TO_BE_FILLED
- Walkthrough videos: TO_BE_FILLED
- Defect log references: TO_BE_FILLED
- Build / transport references: TO_BE_FILLED
- Related incident / defect IDs: TO_BE_FILLED

## Local Desktop Acceptance Baseline

- Repo-side validation scope:
  - startup shell and skip link contract
  - search single-column runtime
  - `search -> detail -> fullscreen/analytics -> back`
  - detail section jump and sticky rail behavior
  - compact/cozy-safe shell offset and focus path
- Expected local command path:
  - start local runtime via `node scripts/start-local-server.js`
  - run browser validation via `python scripts/interaction-smoke.py http://127.0.0.1:8080/index.html`
- Acceptance rule:
  - local result may be recorded as `PASS_LOCAL_BASELINE` only when the interaction smoke exits `0`
  - SAP / FLP evidence remains mandatory and must still be collected in productive landscape
  - SAP walkthrough result must be recorded as one of `PASS_SAP_EVIDENCE`, `BLOCKED_SAP_ENV`, `FAIL_PRODUCT_CONTRACT`
- Local evidence fields:
  - local smoke timestamp: 2026-03-19
  - local browser/runtime: Playwright Chromium against `http://127.0.0.1:8080/index.html`
  - local result class: `BLOCKED_BACKEND`
  - local result summary: local smoke contract is repaired and reproducible; shell render, resize telemetry, shell-to-analytics route, and direct detail route all pass on static localhost, while backend-dependent detail data remains blocked by missing SAP `$metadata`
  - local artifact path or console capture: `python scripts/interaction-smoke.py http://127.0.0.1:8080/index.html`
  - local key checks passed:
    - `checklist_app_comp---app--mainFcl` present
    - `checklist_app_comp---app--appShellHeaderHost` present
    - `checklist_app_comp---searchTargetPage--searchWorkbenchDock` present
    - `checklist_app_comp---searchTargetPage--searchFilterCard` present
    - `checklist_app_comp---searchTargetPage--searchResultsShell` present
    - `checklist_app_comp---searchTargetPage--searchResultsToolbarHost` present
    - shell analytics route opens and returns `routeName=analytics`
    - direct detail route opens with `routeName=detail` and `layout=TwoColumnsMidExpanded`
  - local blocker class:
    - SAP OData metadata unavailable on static localhost: `/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/$metadata` returned `502`
    - SAP OData metadata unavailable on static localhost: `/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/$metadata?sap-language=EN` returned `502`

## Starter Record

- Prepared by repo: yes
- Ready for live execution: yes
- Missing landscape-only inputs:
  - FLP host and browser versions
  - mobile-width device or emulator used
  - final accepted walkthrough captures
  - actual defect IDs or explicit zero-defect statement

## Result

- Expected result: keyboard-only flow completes without focus trap, hidden action, or sticky overlap
- Actual result: repo-side UI contract is hardened to zero private UI5 selectors in runtime/CSS/OPA; local browser rendering and local interaction smoke now work against the current app contract and classify the environment honestly as `BLOCKED_BACKEND` when productive metadata is unavailable
- Pass / fail: local pre-flight currently `BLOCKED_BACKEND`; final SAP walkthrough must end as one of `PASS_SAP_EVIDENCE`, `BLOCKED_SAP_ENV`, `FAIL_PRODUCT_CONTRACT`
- Residual risks: local static runtime still cannot prove end-to-end detail/search accessibility, lock, and personalization flows because OData metadata is unavailable outside SAP-backed runtime
- Follow-up action: run the same EV-010 checklist on SAP / FLP runtime and update this record with final artifact paths and PASS/FAIL status
