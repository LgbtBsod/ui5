# QA report (latest)

## Gate summary

- Gate: **qa-all**
- Status: **FAIL**
- Error count: **1**

## Top offenders

- scripts/qa-all.js:1 :: ====================================
UI5 ENTERPRISE QA PIPELINE
====================================

[1/44] forbidden-patterns
PASS

[2/44] gateway-parity-validator
PASS

[3/44] sap-gateway-only-gate
PASS

[4/44] smart-odata-contract-gate
PASS

[5/44] architecture-gate
PASS

[6/44] style-scan
FAIL
[style:scan] WARN lint:css matched existing baseline debt and did not regress.
[style:scan] FAIL check:css-arch
> check:css-arch
> node scripts/check-css-architecture.mjs

CSS architecture check failed: css/modules/02_background.css contains !important outside 90_ui5_patches.css.

## Fix hints

- **QA_PIPELINE**: Run failing gate directly and address reported violation.
  - Good: npm run qa -> ALL ARCHITECTURE CHECKS PASSED
  - Bad: npm run qa -> FAIL <gate>

## Changed JS dependency scan

- No changed JS files detected.

## Documentation links

- `docs/architecture-map.md`
- `docs/dependency-graph.json`
- `docs/feature-dependency-map.md`