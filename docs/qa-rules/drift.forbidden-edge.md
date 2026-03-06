# drift.forbidden-edge

Detects newly introduced forbidden dependency edges compared to `docs/deps-graph-baseline.json`.

Forbidden:
- controller -> infra/backend
- domain usecase -> sap/ui/*

Initialize baseline manually:
`node scripts/ci/dependency-drift-gate.js --init-baseline`
