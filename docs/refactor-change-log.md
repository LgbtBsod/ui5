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

- Wave ContractSemver+A3-TriggerExtraction: completed B1 semver-compatibility enforcement via `ensureContractCompatibility` and fixture-driven contract drift gate checks; started A3 by extracting Search trigger execution orchestration into `SearchTriggerExecutionUseCase` and slimming `Search.controller` trigger handlers; smoke coverage extended.

- Wave A3-InlineAnalyticsRailExtraction: extracted inline analytics trigger/rail refresh orchestration from `Search.controller.js` into `SearchInlineAnalyticsRailUseCase`; controller switched to argument-builder + usecase delegation pattern; smoke coverage added for supported/unsupported trigger and rail refresh state transitions.

- Wave A3-InlineAnalyticsAutoRefreshExtraction: extracted analytics auto-refresh timer lifecycle from `Search.controller.js` into `SearchInlineAnalyticsAutoRefreshUseCase`; controller onExit/refresh-start now use deterministic usecase delegation; smoke coverage added for timer restart/stop semantics.

- Wave A3-WorkflowAnalyticsDialogExtraction: extracted workflow analytics open/close degraded orchestration from `Search.controller.js` into `SearchWorkflowAnalyticsDialogLifecycleOrchestrationUseCase`; controller now delegates dialog envelope through argument-builder pattern; smoke coverage added for deterministic open/close lifecycle behavior.

- Wave B1-SemverPolicyHardening+A3-AnalyticsLoadExtraction: completed production-grade B1 semver policy validation (invalid policy range detection + fail-fast `enforceContractCompatibility` + startup `init` enforcement + dedicated semver gate in pre-push) and extracted workflow analytics load envelope from `Search.controller.js` to `SearchWorkflowAnalyticsLoadOrchestrationUseCase`; smoke coverage extended.

- Wave A3-FilterInteractionPolicyExtraction+B1-ExactUpperBoundCoverage: extracted Search reset/toggle filter interaction policy orchestration into `SearchFilterInteractionOrchestrationUseCase` (controller keeps wiring-only callbacks) and extended B1 gates/tests with exact semver upper-bound compatibility scenarios to harden policy enforcement determinism.

- Wave A3-RetryLoadExtraction+B1-MalformedSemverCoverage: extracted Search retry-load orchestration envelope into `SearchRetryLoadOrchestrationUseCase` (controller reduced to callback wiring) and hardened B1 gates/tests with malformed semver metadata compatibility rejection checks (`invalid_semver_metadata`).

- Wave A3-ExportIntentExtraction+B1-SemverGateMalformedCoverage: extracted Search export intent lifecycle envelope into `SearchExportLifecycleUseCase.runExportIntentOrchestration` (default/menu/report handlers now use a shared args-builder + delegation path) and expanded `backend-semver-policy-gate.js` with malformed semver metadata rejection coverage.

- Wave A4-SaveErrorOutcomeLifecycleExtraction: started A4 by extracting Detail save-error outcome lifecycle envelope (presentation + KPI markers + conflict classification + latency finish) from `Detail.controller.js` into `DetailSaveErrorOutcomePresentationUseCase.runOutcomeLifecycle`, keeping controller as wiring shell for callback adapters.

- Wave A3-ExportExecutionEnvelope+A4-LocationValueHelpLifecycle: extracted Search export execution envelope into `SearchExportLifecycleUseCase.runExportExecutionOrchestration` and reduced `Search.controller.js` export execution path to adapter wiring; continued A4 by extracting Detail location value-help open/list/tree/combo lifecycle envelopes into `DetailLocationValueHelpUseCase` orchestration methods with controller callback-only adapters.

- Wave A3-SelectionOpenInteractionEnvelope+A4-DictionarySelectionLifecycle: extracted Search selection/open interaction lifecycle envelope into `SearchSelectionOpenFlowUseCase.runSelectionChange/runItemPress` and switched Search controller smart/fallback selection handlers to shared args-builder delegation; continued A4 by extracting dictionary selection lifecycle envelope into `DetailDictionarySelectionUseCase.runDictionarySelectionLifecycle` and delegating Detail LPC/profession handlers to callback-only controller wiring.

- Wave A3A4-LifecycleConsolidationPass: consolidated Search selection/open controller handlers through `SearchSelectionOpenFlowUseCase` interaction APIs and consolidated Detail LPC dictionary+warning sequencing via `DetailDictionarySelectionUseCase.runLpcSelectionLifecycle`; smoke coverage expanded for lifecycle passthrough guarantees.

- Wave A3A4-ExecutionPresentationAndProfessionLifecycle: moved Search export execution presentation callbacks (empty/success/error + file naming/download) into `SearchExportLifecycleUseCase.runExportExecutionPresentationOrchestration` and reduced controller `_runExport` to adapter wiring; continued A4 with profession dictionary delegation via `DetailDictionarySelectionUseCase.runProfessionSelectionLifecycle` and added smoke assertions for both orchestration paths.

- Wave A3A4-SelectionInteractionAndCancelEditLifecycle: consolidated Search smart/fallback selection+item orchestration via `SearchSelectionOpenFlowUseCase.runSelectionInteractionOrchestration` (controller handlers now route through a single interaction adapter) and continued A4 by extracting Detail cancel-edit lifecycle into `DetailToggleEditOrchestrationUseCase.runCancelEditFlow`; smoke tests extended for both orchestration branches.
