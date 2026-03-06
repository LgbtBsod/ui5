# UDOS Analysis Report

- Generated at: 2026-03-03T05:16:22.333Z
- Mode: report
- Decision: CLEAR
- Budget violated: false
- Queue size: 12
- Open missions: 4

## Scores
- ArchitectureScore: 96
- AIL: 90
- PMI: 90
- ADT: 90
- DomainCompleteness: 90

## Last Intake
- Change type: infra
- Risk: LOW
- Impacted workflows: -
- Files: docs/architecture-context.json, docs/architecture-health.md, docs/architecture-map.md, docs/dependency-graph.json, docs/domain-boundaries.md, docs/duplicate-logic.md, docs/feature-map.md, docs/feature-registry.json, docs/large-functions.md, docs/legacy-surface-map.md, docs/qa-history-latest.json, docs/qa-report-latest.json, docs/qa-report-latest.md, docs/refactor-history.md, docs/repository-map.md, docs/repository-memory.json, docs/workflow-map.md, docs/workflow-registry.json, scripts/architecture-governor.js

## Verification
- npm run qa: PASS
- npm run architect:audit: PASS
- npm run domain-model:verify: PASS
- npm run digital-twin:preflight: PASS
- npm run air-traffic-control: PASS

## Top Risk Modules
- digital-twin: 0.68
- package.json: 0.68
- history: 0.68
- kernel: 0.68

## Policy Proposals
- PP-relax-low-risk: Slightly relax review overhead for LOW-risk non-runtime-only patches.
