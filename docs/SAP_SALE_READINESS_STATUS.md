# SAP Sale-Readiness Status

This document records which sale-readiness remediations are implemented inside this repository and which still require execution in a real SAP landscape.

## Implemented In Repo

- Standard UI5 tooling contour added with `package.json` and `ui5.yaml`.
- Bootstrap switching is now explicit: local validation currently uses the public UI5 1.71.70 CDN, and the repo documents that this must be switched back to the SAP system runtime before Gateway deployment.
- Local Python dev helper remains available and can proxy UI5 resources for development.
- Descriptor copy moved to business-facing text with FLP `crossNavigation` inbound.
- `flexEnabled` is aligned with smart control personalization scenarios.
- Productive shell is aligned to a single Horizon-first visual profile and the theme toggle is hidden.
- Custom shell header no longer mutates internal DOM of UI5 controls.
- Smart filter labels and mock metadata labels were upgraded to business-readable semantics.
- Unload-based `sendBeacon` lock release was disabled; normal in-app release flow remains authoritative.
- QUnit and OPA5 scaffolding was added as the primary frontend qualification contour for this repo.

## Requires Real SAP System Execution

- DDIC objects, CDS alignment, and final Gateway metadata activation.
- FLP catalog, target mapping, and launchpad runtime proof on the target landscape.
- Authorization traces for allow/deny paths on display, edit, delete, export, and attachments.
- ETag and `If-Match` conflict proof with real concurrent users against the Gateway service.
- Attachment storage, malware scanning, retention, and operational ownership sign-off.
- TTL cleanup, lock takeover, and background lifecycle validation in the productive system topology.
- Accessibility verification against the actual FLP-hosted runtime and supported browsers.

## Evidence Position

- The repository now contains buildable frontend packaging and reproducible frontend test entry points.
- Legacy gates remain supplementary diagnostics only. They are not the release source of truth for sale-readiness.
- Any claim that the application is SAP-sale-ready still requires landscape evidence from `backend/sap_backend` deployment and FLP execution, not only local mock results.
- The current workspace keeps the CDN bootstrap only as a temporary local-development concession. It is not the target productive contour.
