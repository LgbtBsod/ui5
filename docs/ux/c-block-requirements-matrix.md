# C-block requirements matrix (C1-C4)

| Requirement | Implementation | Evidence files |
|---|---|---|
| C1 checklist mandatory + state coverage + visual baseline refs | Governance checklist, PR template checklist, state coverage artifact, CI gate | `docs/ux/design-governance-checklist.md`, `.github/PULL_REQUEST_TEMPLATE.md`, `docs/ux/baselines/state-coverage.json`, `scripts/ux-state-coverage-gate.js` |
| C2 accessibility baseline + keyboard/focus protocol + reduced motion + gate | WCAG/UI5 baseline doc, manual protocol, reduced-motion CSS, tooltip lint gate | `docs/ux/accessibility-baseline.md`, `css/style.css`, `view/fragment/DetailControlRail.fragment.xml`, `scripts/a11y-gate.js` |
| C3 SLO + telemetry + report + runbook | Telemetry utility + instrumentation in search/save/dialog operations, thresholds and report script, runbook | `util/UxTelemetry.js`, `controller/Search.controller.js`, `controller/Detail.controller.js`, `scripts/ux-slo-thresholds.json`, `scripts/ux-slo-report.js`, `docs/ux/ux-latency-slo.md`, `docs/ux/slo-breach-runbook.md` |
| C4 message taxonomy + normalization + CI lint | Taxonomy policy doc, metadata catalog, lint gate against i18n keys | `docs/ux/message-taxonomy.md`, `docs/ux/message-catalog-meta.json`, `scripts/message-taxonomy-gate.js` |
