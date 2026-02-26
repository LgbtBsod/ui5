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

- Wave Search-FilterLifecycle: extracted search filter reset/toggle orchestration from `Search.controller.js` into `SearchFilterLifecycleUseCase` to advance thin-adapter profile and deterministic lifecycle ordering; smoke coverage extended.

- Wave Search-RetryLifecycle: extracted retry-load lifecycle envelope from `Search.controller.js` into `SearchRetryLifecycleUseCase` (latency start/finish, failure instrumentation, message presentation hook, post-apply hook) with deterministic fallback `missing_retry_flow`; smoke coverage extended.

- Wave Search-LifecycleSync+StatusStrip: extracted analytics/filter-hint lifecycle synchronization to `SearchLifecycleSyncUseCase` (SmartTable dataReceived + fallback search sync) and completed CSS unified status-strip hierarchy pass for filter-hint/degraded/load-error stack.

- Wave Search-ToolbarLifecycle+ToolbarCSS: extracted Search action-toolbar lifecycle envelope to `SearchToolbarLifecycleUseCase` (create/copy/delete/export intent) and completed toolbar hierarchy CSS pass for spacing/chips alignment/responsive wrap.

- Wave Search-AnalyticsDialogLifecycle+DialogCSS: extracted workflow analytics dialog lifecycle envelope to `SearchWorkflowAnalyticsLifecycleUseCase` (open/load/close + degraded handling) and completed analytics dialog hierarchy CSS pass.

- Wave Search-StatusFilterLifecycle+StatusChipCSS: extracted status-filter press lifecycle to `SearchStatusFilterLifecycleUseCase` and completed status-chip/action emphasis CSS consistency pass.

- Wave Search-TriggerPolicy+CompactCSS: extracted unified SmartFilter/search-mode/status-filter/reset trigger policy to `SearchTriggerPolicyUseCase` and completed compact breakpoint CSS consistency pass for SmartFilter+chips interactions.

- Wave Search-RouteLifecycle+RouteEntryCSS: extracted route-matched lifecycle bundle to `SearchRouteLifecycleUseCase` and completed route-entry visual stability CSS pass for first-frame consistency.

- Wave Search-RebindLifecycle+SmartTableCSS: extracted SmartTable rebind/dataReceived lifecycle to `SearchRebindLifecycleUseCase` and completed SmartTable header/actions compact consistency CSS pass.

- Wave Search-ConvergenceLifecycle+NoDataCSS: extracted search-result summary+empty-state convergence bundle to `SearchResultConvergenceLifecycleUseCase` and completed summary/no-data transition CSS consistency pass.

- Wave Search-SelectionLifecycle+SelectionCSS: extracted smart/fallback selection lifecycle to `SearchSelectionLifecycleUseCase` and completed selection affordance CSS pass for selected/focus/compact states.

- Wave Search-ExportLifecycle+ExportCSS: extracted export execution/intent presentation convergence to `SearchExportLifecycleUseCase` and completed export action emphasis CSS consistency pass.
