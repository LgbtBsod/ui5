# Merge Delete Plan

## Already completed

### Detail domain wrappers
- Sources:
  - `app/service/domain/detail/DetailService.js`
  - `app/service/domain/detail/DetailUseCaseRegistry.js`
  - `app/service/domain/detail/DetailEntryAdapterRuntime.js`
- Canonical owner:
  - `app/service/domain/detail/DetailFacade.js`
- Reason:
  - all three were pass-through layers without unique boundary value
- Status:
  - merged and deleted

### Search action wrappers
- Sources:
  - `app/controller/search/SearchViewSelectionBehavior.js`
  - `app/controller/search/internal/SearchViewNavigationBehavior.js`
  - `app/controller/search/internal/SearchFlowBehavior.js`
- Canonical owner:
  - `app/controller/search/SearchActionBehavior.js`
- Reason:
  - all three were pass-through controller-level wrappers around existing selection, export, and busy/search flow semantics
- Status:
  - merged and deleted

### Detail attachment drop zone wrappers
- Sources:
  - `app/controller/detail/AttachmentDropZoneBindingRuntime.js`
  - `app/controller/detail/AttachmentDropZoneEventRuntime.js`
  - `app/controller/detail/AttachmentDropZoneVisualRuntime.js`
- Canonical owner:
  - `app/controller/detail/AttachmentDropZoneRuntime.js`
- Reason:
  - same detail drop-zone feature was split into binding/event/visual micro-files with no separate boundary value
- Status:
  - merged and deleted

### Search selection internal wrappers
- Sources:
  - `app/service/features/search/runtime/SearchSelectionFocusRuntime.js`
  - `app/service/features/search/runtime/SearchSelectionTableRuntime.js`
- Canonical owner:
  - `app/service/features/search/runtime/SearchSelectionRuntime.js`
- Reason:
  - both were internal helper owners used only by the main selection runtime
- Status:
  - merged and deleted

### Detail validation wrappers
- Sources:
  - `app/controller/detail/internal/DetailValidationFocusRuntime.js`
  - `app/controller/detail/internal/DetailValidationReactiveRuntime.js`
  - `app/controller/detail/internal/DetailValidationStateRuntime.js`
- Canonical owner:
  - `app/controller/detail/DetailValidationSummaryRuntime.js`
- Reason:
  - the public validation runtime was only a pass-through owner; compute/recompute/focus/reactive logic belonged to one validation flow
- Status:
  - merged and deleted

### Search viewport wrappers
- Sources:
  - `app/service/features/search/runtime/SearchViewportBindingRuntime.js`
  - `app/service/features/search/runtime/SearchStickyLayoutRuntime.js`
  - `app/service/features/search/runtime/SearchStickyOffsetRuntime.js`
  - `app/service/features/search/runtime/SearchScrollRuntime.js`
- Canonical owner:
  - `app/service/features/search/runtime/SearchViewportRuntime.js`
- Reason:
  - one viewport/layout concern was fragmented across binding, sticky, offset, and scroll helper modules
- Status:
  - merged and deleted

### Search toolbar formatting/settings wrappers
- Sources:
  - `app/controller/search/SearchFormatterBehavior.js`
  - `app/controller/search/SearchToolbarSettingsRuntime.js`
  - `app/controller/search/SearchToolbarDialogFactoryRuntime.js`
- Canonical owner:
  - `app/controller/search/SearchControllerBehavior.js`
  - `app/controller/search/SearchToolbarDialogRuntime.js`
- Reason:
  - controller-only formatters and dialog settings/factory logic were thin wrappers around the active search screen owner flow
- Status:
  - merged and deleted

### Search lifecycle internal wrappers
- Sources:
  - `app/controller/search/internal/SearchStartupBehavior.js`
  - `app/controller/search/internal/SearchViewLoadBehavior.js`
  - `app/controller/search/internal/SearchFilterLifecycleBehavior.js`
  - `app/controller/search/internal/SearchRequestRuntime.js`
- Canonical owner:
  - `app/controller/search/SearchLifecycleBehavior.js`
  - `app/controller/search/SearchSmartTableBehavior.js`
- Reason:
  - route/init/exit logic belonged to lifecycle owner, while smart filter/table load flow belonged to smart-table owner; the internal modules only fragmented one search screen flow
- Status:
  - merged and deleted

## Next merge candidates

### Search controller cluster
- Sources:
  - `app/controller/search/SearchLifecycleBehavior.js`
  - `app/controller/search/SearchActionBehavior.js`
  - `app/controller/search/SearchSmartTableBehavior.js`
- Canonical owner:
  - `app/controller/search/SearchControllerBehavior.js`
- Reason:
  - search screen still has multiple visible owners, but the internal hidden-framework layer is already removed

### Search selection runtime cluster
- Sources:
  - `app/service/features/search/runtime/SearchSelectionStateRuntime.js`
  - `app/service/features/search/runtime/SearchReturnRediscoveryRuntime.js`
- Canonical owner:
  - `app/service/features/search/runtime/SearchSelectionRuntime.js`
- Reason:
  - one UX responsibility still split between shared state payloads and return-rediscovery flow

### App shell behavior cluster
- Sources:
  - `app/controller/app/AppLifecycleBehavior.js`
  - `app/controller/app/AppOverlayBehavior.js`
  - `app/controller/app/AppShellBehavior.js`
  - `app/controller/app/AppStateBehavior.js`
  - `app/controller/app/AppDomBehavior.js`
  - `app/controller/app/AppShellActionRuntime.js`
- Canonical owner:
  - `app/controller/App.controller.js` plus one app controller runtime/behavior owner
- Reason:
  - app shell behavior currently fragmented without clear single owner
