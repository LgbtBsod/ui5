# Performance And Progressive Readiness Blueprint

Date: 2026-03-12
Scope: `UI5 1.71 frontend` for transition from test contour to real SAP Gateway
Objective: reduce `time to useful work` by segmenting startup, service loading, and screen readiness

## Target Principle

The application should not behave as if the whole screen becomes usable only after the whole application becomes ready.

Target behavior:
- shell becomes visible first
- search becomes interactive second
- detail becomes interactive independently
- analytics and secondary enrichments load later
- background capabilities never block core checklist work

## Readiness Model

Introduce explicit readiness states in the app state model:

- `appShellReady`
- `navigationReady`
- `searchReady`
- `detailReady`
- `analyticsReady`
- `attachmentsReady`
- `settingsReady`
- `backgroundReady`

Add matching busy and degraded states:

- `searchLoading`
- `detailLoading`
- `analyticsLoading`
- `attachmentsLoading`
- `backgroundSyncRunning`
- `partialDataMode`

These states should replace broad coarse-grained app busy behavior where possible.

## Loading Segments

### Segment 1: Critical startup

Purpose:
- make the shell render
- make routing stable
- allow user to start navigation

Should include:
- component bootstrap
- minimal i18n
- route resolution
- minimal current user context
- minimal settings required to avoid broken rendering
- base search shell model

Should not include:
- analytics preload
- export data preparation
- attachment capability preload
- optional theme enrichment
- large metadata-derived normalization work beyond critical route needs

Likely code owners:
- [Component.js](/C:/Users/lgbtb/Desktop/ui5/app/Component.js)
- [ComponentInitRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitRuntime.js)
- [ComponentCoordinatorRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentCoordinatorRuntime.js)
- [ComponentListenerRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentListenerRuntime.js)

### Segment 2: Search readiness

Purpose:
- get the main worklist/search view usable fast

Should include:
- filter model defaults
- minimal checklist search request
- list container rendering
- selection contract
- search-side status and readiness banners

Should defer:
- large analytics summaries
- detail-side enrichments
- heavy result decoration not needed for initial selection

Likely code owners:
- [SearchControllerBehavior.js](C:/Users/lgbtb/Desktop/ui5/app/controller/search/SearchControllerBehavior.js)
- [SearchViewportRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchViewportRuntime.js)
- [SearchSelectionRuntime.js](C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchSelectionRuntime.js)
- [SearchViewBehavior.js](C:/Users/lgbtb/Desktop/ui5/app/controller/search/SearchViewBehavior.js)
- [Search.view.xml](C:/Users/lgbtb/Desktop/ui5/app/views/Search.view.xml)

### Segment 3: Detail readiness

Purpose:
- let the user inspect and edit the selected checklist without waiting for analytics and secondary rails

Should include:
- detail header
- core checklist rows
- lock state
- edit mode state
- primary save and autosave affordances

Should defer:
- secondary cards
- optional information rails
- attachment previews
- non-critical decoration

Likely code owners:
- [DetailViewBehavior.js](C:/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailViewBehavior.js)
- [DetailChecklistBehavior.js](C:/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailChecklistBehavior.js)
- [DetailChecklistRowBehavior.js](C:/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailChecklistRowBehavior.js)
- [DetailActionViewportBehavior.js](C:/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailActionViewportBehavior.js)
- [Detail.view.xml](C:/Users/lgbtb/Desktop/ui5/app/views/Detail.view.xml)

### Segment 4: Attachments and secondary workflow capabilities

Purpose:
- load optional but business-relevant actions after primary checklist work is already available

Should include:
- upload policy retrieval
- attachment list retrieval
- drop zone activation
- file capability messaging

Should defer:
- preview enhancement
- background metadata enrichment

Likely code owners:
- [AttachmentUploadCore.js](C:/Users/lgbtb/Desktop/ui5/app/controller/detail/AttachmentUploadCore.js)
- [DetailControlRail.fragment.xml](C:/Users/lgbtb/Desktop/ui5/app/views/fragment/DetailControlRail.fragment.xml)
- [ODataChecklistRepoAdapter.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/ODataChecklistRepoAdapter.js)

### Segment 5: Analytics and export

Purpose:
- fully isolate non-critical analytical load from startup and core edit flow

Should include:
- analytics widget hydration
- breakdown fragments
- export preparation
- optional trend enrichment

Should always be deferred until:
- shell is ready
- search is ready
- current detail context is stable if analytics depends on it

Likely code owners:
- [AnalyticsControllerBehavior.js](C:/Users/lgbtb/Desktop/ui5/app/controller/analytics/AnalyticsControllerBehavior.js)
- [AnalyticsPayloadNormalizer.js](/C:/Users/lgbtb/Desktop/ui5/app/service/domain/analytics/AnalyticsPayloadNormalizer.js)
- [WorkflowAnalyticsBreakdowns.fragment.xml](C:/Users/lgbtb/Desktop/ui5/app/views/fragment/WorkflowAnalyticsBreakdowns.fragment.xml)

### Segment 6: Background and cosmetic enrichment

Purpose:
- perform non-blocking work after user can already operate the app

Should include:
- optional settings hydration
- prefetch for likely next detail item
- theme enrichment not required for correctness
- metadata cache refresh helpers
- low-priority compatibility requests

Likely code owners:
- [ThemeService.js](C:/Users/lgbtb/Desktop/ui5/app/service/framework/ThemeService.js)
- [SettingsManager.js](/C:/Users/lgbtb/Desktop/ui5/app/service/runtime/SettingsManager.js)
- component runtime modules

## Progressive Readiness By UI Block

### Block A: Shell

Make visible immediately:
- top shell frame
- app title
- route label
- base navigation layout

Do not wait for:
- analytics counters
- user menu enrichment
- theme prefetch completion

Current candidates:
- [App.view.xml](C:/Users/lgbtb/Desktop/ui5/app/views/App.view.xml)
- [AppShellHeader.js](C:/Users/lgbtb/Desktop/ui5/app/controls/AppShellHeader.js)

### Block B: Search pane

Make interactive as soon as:
- route is known
- minimal filters are bound
- first result request can execute

Use:
- skeleton list placeholders
- independent busy state for list only
- non-blocking filter enhancement

Current candidates:
- [Search.view.xml](C:/Users/lgbtb/Desktop/ui5/app/views/Search.view.xml)
- search controller support files

### Block C: Detail pane

Make interactive as soon as:
- selected item id is known
- lock/edit state is resolved
- core rows are available

Use:
- row-level placeholders
- section-by-section hydration
- delayed mounting of optional rail content

Current candidates:
- [Detail.view.xml](C:/Users/lgbtb/Desktop/ui5/app/views/Detail.view.xml)
- detail controller support files

### Block D: Analytics region

Never block search or detail.

Use:
- lazy fragment creation
- explicit `analyticsLoading`
- stale-while-refresh display if prior snapshot exists

Current candidates:
- [WorkflowAnalyticsBreakdowns.fragment.xml](C:/Users/lgbtb/Desktop/ui5/app/views/fragment/WorkflowAnalyticsBreakdowns.fragment.xml)
- analytics controller support files

### Block E: Dialogs and optional overlays

Load on demand only.

Use:
- fragment factory on first open
- local caching after first mount
- no startup creation for infrequent dialogs

Current candidates:
- dialog fragments and shell-related popovers

## Service Segmentation Recommendations

### Requests that should remain in the critical path

- current route context
- minimal checklist search bootstrap
- current selected checklist detail
- lock acquisition only when entering edit path

### Requests that should leave the critical path

- analytics summaries
- export preparation payloads
- attachment capability reads when the panel is not opened
- optional settings and personalization hydration
- non-essential reference data if it does not affect first interaction

### OData strategy

Recommended:
- keep requests small and aligned to screen responsibility
- batch only tightly related reads needed for one readiness band
- avoid one giant bootstrap batch for unrelated concerns
- add explicit timeout and error policy per segment

Not recommended:
- startup mega-request that mixes shell, search, detail, analytics, settings, and attachments

## Concrete Refactoring Candidates

### First-wave candidates

1. [ComponentInitRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ComponentInitRuntime.js)
Reason:
- startup orchestration is centralized here and can be split into priority bands

2. [SearchControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/search/SearchControllerBehavior.js)
Reason:
- likely mixes initial search readiness, actions, and post-load behavior

3. [DetailViewRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailViewBehavior.js)
Reason:
- detail readiness should be separated into core detail, secondary panels, and deferred enrichments

4. [AnalyticsControllerActions.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/analytics/AnalyticsControllerBehavior.js)
Reason:
- analytics should become a strictly deferred branch

5. [ODataChecklistRepoAdapter.js](/C:/Users/lgbtb/Desktop/ui5/app/infra/adapters/ODataChecklistRepoAdapter.js)
Reason:
- service contract methods should be grouped by readiness band and screen responsibility

### Second-wave candidates

1. [SearchViewportRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchViewportRuntime.js)
2. [SearchSelectionRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchSelectionRuntime.js)
3. [DetailChecklistRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailChecklistBehavior.js)
4. [AttachmentUploadCore.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/detail/AttachmentUploadCore.js)
5. [ThemeService.js](/C:/Users/lgbtb/Desktop/ui5/app/service/framework/ThemeService.js)

## Suggested Technical Design

### 1. Introduce a startup coordinator with priority bands

Example target bands:
- `bandCritical`
- `bandSearch`
- `bandDetail`
- `bandDeferred`
- `bandBackground`

Each band should:
- declare dependencies
- own its requests
- expose its readiness marker
- fail independently where possible

### 2. Separate global busy from local busy

Current anti-pattern to avoid:
- one global busy flag that blocks the whole screen for partial work

Target:
- shell busy
- search busy
- detail busy
- analytics busy
- attachments busy

### 3. Make fragments lazy by default

Apply to:
- analytics breakdown fragments
- optional detail rails
- settings and user menu overlays
- secondary dialogs

### 4. Add lightweight metrics

Track at minimum:
- shell first render
- search interactive
- detail interactive
- analytics complete
- total background completion

Store these as:
- browser performance marks in dev mode
- optional structured logs in test contour

## Delivery Order

### Wave 1

- segment startup in component runtime
- separate shell readiness from search readiness
- defer analytics completely

### Wave 2

- split detail readiness from secondary rail and attachment readiness
- lazy-load dialogs and fragments
- centralize readiness state contract

### Wave 3

- optimize service granularity and OData batching by band
- add measurements and thresholds
- remove leftover broad startup coupling

## Expected Outcome

If implemented correctly:
- application appears ready earlier
- primary checklist work starts faster
- analytics no longer slows down core work
- regressions become easier to isolate by readiness band
- migration to real Gateway becomes safer because contract usage is clearer and more intentional
