# Refactor change-log (Wave-by-wave)

## Wave D
- D.1: Controller slimming phase-2 initiated; diagnostics orchestration moved to usecases (`StartupCapabilityDiagnosticsUseCase`, `KpiSnapshotExportUseCase`).
- D.2: KPI snapshot/export contract introduced via `operationalKpiSnapshots` + export payload usecase.
- D.3: Startup capability matrix paths added to state model (`capabilityStatus`, `capabilityDegradedReason`, `capabilityMessageKey`, `capabilityDiagnostics`).

## Wave E
- E.1: Added deterministic release governance gates: `scripts/wave-d-regression-gate.js`, `scripts/wave-e-release-governance-gate.js`.
- E.2: ADR governance template added (`docs/adr/ADR-TEMPLATE.md`).
- E.3: Freeze v2 checklist + rollback playbook added.

- Wave Search-FullRefactor: Search controller full-pass extraction completed for route defaults/state-sync, execute-search lifecycle, and create/copy intent orchestration using `SearchStateSyncUseCase`, `SearchExecuteFlowUseCase`, `SearchCreateCopyFlowUseCase`; smoke and Wave D gates updated.

- Wave Detail-FullRefactor: Detail controller full-pass extraction completed for close-flow orchestration, toggle-edit lock flow, and save-flow envelope using `DetailCloseFlowOrchestrationUseCase`, `DetailToggleEditOrchestrationUseCase`, `DetailSaveFlowOrchestrationUseCase`; smoke and Wave D gates updated.

- Wave Refactor-Normalization: grouped Detail selection-meta sync extracted to `DetailSelectionMetaSyncUseCase`; extracted flow wrappers standardized fallback outcome to `no_result`; wave-d regression matrix extended.
