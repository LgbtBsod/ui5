# Owner Matrix

| Responsibility | Current owner file(s) | Duplicate owner? | Target single owner | Action |
| --- | --- | --- | --- | --- |
| Detail domain entry | `app/service/domain/detail/DetailFacade.js` | No, after recent merge | `app/service/domain/detail/DetailFacade.js` | Keep |
| Detail enter-edit orchestration | `EnterEditUseCase.js`, `DetailWorkflowRuntime.js`, controller call sites | Partial | `app/service/domain/detail/usecases/EnterEditUseCase.js` with result decoration in `DetailWorkflowRuntime.js` | Keep, no new split |
| Detail attachment open/download | `DetailAttachmentOpenRuntime.js`, fragment-triggered controller flow | No | `app/service/features/detail/runtime/DetailAttachmentOpenRuntime.js` | Keep |
| Attachment drop zone orchestration | `AttachmentDropZoneRuntime.js` | No, after latest merge | `app/controller/detail/AttachmentDropZoneRuntime.js` | Keep |
| Attachment state orchestration | `DetailAttachmentViewState.js`, `LoadAttachmentsUseCase.js`, controller attachment helpers | Yes | One canonical detail attachments runtime | Merge candidate |
| Detail validation orchestration | `DetailValidationSummaryRuntime.js`, `DetailValidationHelperRuntime.js` | Reduced | `app/controller/detail/DetailValidationSummaryRuntime.js` for runtime flow, `DetailValidationHelperRuntime.js` for shared path/value helpers | Keep after merge |
| Detail controller UI flow | `DetailControllerBehavior.js`, `DetailControllerRuntime.js`, `DetailPageFlow.js`, multiple helpers | Yes | `app/controller/detail/DetailControllerRuntime.js` or `DetailControllerBehavior.js` as single entry owner | Merge |
| Search controller UI flow | `SearchControllerBehavior.js`, `SearchLifecycleBehavior.js`, `SearchActionBehavior.js`, `SearchSmartTableBehavior.js` | Reduced further | `app/controller/search/SearchControllerBehavior.js` with focused secondary owners for action and smart-table flows | Keep after merge wave |
| Search selection state | `SearchActionBehavior.js`, `SearchSelectionRuntime.js`, `SearchSelectionStateRuntime.js`, `SearchReturnRediscoveryRuntime.js` | Reduced | `app/service/features/search/runtime/SearchSelectionRuntime.js` for runtime mechanics, `SearchSelectionStateRuntime.js` for shared state payloads, `SearchActionBehavior.js` for controller orchestration | Keep after merge |
| Search action orchestration | `SearchActionBehavior.js` | No, after latest merge | `app/controller/search/SearchActionBehavior.js` | Keep |
| Search viewport/sticky/scroll | `SearchViewportRuntime.js` | No, after latest merge | `app/service/features/search/runtime/SearchViewportRuntime.js` | Keep |
| App shell behavior | `AppLifecycleBehavior.js`, `AppOverlayBehavior.js`, `AppShellBehavior.js`, `AppStateBehavior.js`, `AppDomBehavior.js` | Yes | Single app controller runtime/behavior owner | Merge |
| Lock transport mapping | `app/infra/adapters/LockAdapter.js` | No | `app/infra/adapters/LockAdapter.js` | Keep |
| Technical identity alias normalization | `ODataChecklistPayloadMapper.js`, `DeltaFieldMappers.js`, `DeltaPayloadBuilder.js`, `ChecklistSnapshotMapper.js` | Previously yes, now reduced to shared field sets | `app/infra/adapters/shared/ODataEntityContracts.js` for repeated field-name sets, local mappers for direction-specific mapping | Keep single constants owner, no new layer |
| Runtime settings projection | `SettingsManager.js`, `ApplyRuntimeSettingsUseCase.js` | No | `ApplyRuntimeSettingsUseCase.js` for projection, `SettingsManager.js` for load/cache | Keep |
| Delta field mapping | `DeltaFieldMappers.js`, payload mapper helpers | Partial | `app/service/shared/delta/DeltaFieldMappers.js` | Keep and centralize callers |
| CSS style ownership | Many module CSS files patching SAP internals | Yes | App-owned wrapper classes with minimal central overrides | Reduce |
