# SAP Readiness Evidence Request Pack

Date: 2026-03-12

Purpose: collect the minimum system evidence required to convert this repository audit into a decision-complete SAP readiness statement for `on-prem FLP + SAP Gateway`.

Rule: each requested item should be delivered as either a screenshot, exported configuration, runtime trace, transport reference, or short written architecture note. Verbal confirmation is not sufficient for readiness sign-off.

## 1. System Identity and Stack Evidence

- `System -> Status` screenshot showing SAP_BASIS, SAP_GWFND, UI add-on levels, and kernel release.
- Confirmation of the exact productive or pre-productive system where the app will run.
- Confirmation of the productive SAPUI5 delivery source for version `1.71.x`.

## 2. Gateway Service Registration

- `/IWFND/MAINT_SERVICE` evidence for `Z_EHS_PRODUCTION_CONTROL_CKLT_SRV`
- `/IWBEP/REG_SERVICE` evidence for backend registration and version
- system alias assignment used by the service
- SICF node path for the OData service under `/sap/opu/odata/sap/...`
- a live `$metadata` export from the real Gateway service
- one live sample request and response for each public interface:
- `LockAcquire`
- `LockHeartbeat`
- `LockRelease`
- `AutoSave`
- `SaveChanges`
- `SetChecklistStatus`
- `ReportExport`
- `GetHierarchy` or `MplTree`

## 3. FLP Launch and Navigation Evidence

- target mapping or intent definition used to launch the app in FLP
- catalog assignment and role assignment for the app tile
- screenshot or export proving the productive FLP entry exists
- back-navigation behavior note: how the app returns to FLP or related apps
- clarification whether adaptation, personalization, or key-user changes are expected in scope

## 4. Authorization and Role Evidence

- list of authorization objects used for display, create, change, delete, export, and lock actions
- PFCG role names and a short role matrix by business persona
- SU24 proposal evidence if maintained for the service transaction flow
- SU53 or STAUTHTRACE evidence for:
- successful open
- denied open
- successful edit
- denied edit
- delete
- export
- lock or takeover action
- explanation of where the productive authorization check happens if not in the Gateway wrapper code

## 5. Concurrency and Locking Evidence

- the final productive lock architecture note naming the authoritative lock object or table
- SM12 evidence for the chosen lock object if enqueue is used
- cleanup strategy for stale locks
- timeout and heartbeat configuration source
- conflict scenario proof:
- user A edits
- user B is blocked
- lock expires or is released
- user B can take over or retry
- If-Match or version-conflict proof for concurrent save

## 6. Attachment Handling Evidence

- productive attachment storage architecture
- content repository or storage target
- virus scan profile or scanning architecture
- allowed MIME types and file size policy
- retention and deletion policy
- auditability of upload and delete actions

## 7. Package, Namespace, and Transport Governance

- productive package hierarchy for UI5 and ABAP objects
- transport layer and transport path
- object namespace decision
- proof of customer namespace or registered vendor namespace where applicable
- mapping of repository objects to SAP packages and transports

## 8. Operations and Supportability

- STRUST or equivalent TLS setup evidence for productive endpoints
- ICM or web dispatcher note if reverse proxying is involved
- logging and monitoring strategy for Gateway errors and business errors
- ST22 and SM21 monitoring ownership
- performance evidence for core user flows
- ST05, SAT, or ST12 traces for:
- search
- detail open
- save
- export

## 9. Productization and Certification-Style Evidence

- clarification whether the target is only internal productive use or a formal SAP-partner product release
- package of third-party dependency evidence and SBOM approval
- NOTICE and license review sign-off
- product owner sign-off on namespace, app identity, and support model
- if SAP Store or partner certification is targeted: the internal checklist used for that path

## 10. Expected Deliverables From the SAP Team

- one zipped evidence package for Basis/Gateway
- one zipped evidence package for Security/PFCG
- one zipped evidence package for ABAP/OData
- one short architecture note that explains any intentional deviation from standard Gateway or FLP patterns

## Acceptance Standard

- Every `P1` finding in the repository audit must have matching system evidence or a completed remediation.
- Missing evidence is treated as `not ready`, not as `assumed compliant`.
