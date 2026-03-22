# Project Structure Map

## Top level app structure
- `app/controller`
  Role: UI event entrypoints and controller-side orchestration.
  Risk: fragmented into many small behavior/runtime files, especially in `detail`, `search`, `analytics`, `app`.
- `app/views`
  Role: XML views and fragments.
- `app/model`
  Role: JSONModel factories, schemas, state paths.
- `app/constants`
  Role: technical constants and contracts.
  Risk: already useful, but raw-string debt still exists outside this folder.
- `app/infra`
  Role: adapters, OData mapping, transport boundaries.
- `app/service`
  Role: backend, domain, features, framework, runtime, shared logic.
  Risk: high layering density with `domain / features / framework / runtime / behavior / usecases`.
- `app/styles/modules`
  Role: modular CSS.
  Risk: large amount of internal SAP control class patching.
- `app/localService`
  Role: local metadata/mock contract.
- `app/test`
  Role: unit and integration tests.

## Controller map
- `app/controller/detail`
  Role: detail page controller logic.
  Current shape: 21 top-level files plus `internal/*`.
  Overengineering signal: many small behavior/runtime helpers with overlapping UI orchestration.
- `app/controller/search`
  Role: search page controller logic.
  Current shape: 12 top-level files plus `internal/*`.
  Overengineering signal: many small behavior/runtime helpers around search flow and selection.
- `app/controller/app`
  Role: shell/app lifecycle behavior.
  Overengineering signal: app shell logic split across multiple behavior files.
- `app/controller/analytics`
  Role: analytics page logic.
  Overengineering signal: multiple runtime/behavior files around one screen.

## Service map
- `app/service/domain/detail/usecases`
  Role: detail business operations.
  Current count: 18 usecase files.
  Overengineering signal: potential thin wrappers and fragmented ownership.
- `app/service/domain/search/usecases`
  Role: search business operations.
  Current count: 10 usecase files.
  Overengineering signal: likely wrapper usecases around one search flow.
- `app/service/features/detail/runtime`
  Role: detail feature-side runtime helpers.
- `app/service/features/search/runtime`
  Role: search runtime helpers.
  Overengineering signal: duplicated viewport/selection/loading ownership across many files.
- `app/service/framework`
  Role: reusable UI/runtime execution primitives.
  Note: should stay stable unless a file is a proven thin wrapper.

## Hotspots checked
- `app/infra/adapters/LockAdapter.js` : found
- `app/infra/adapters/shared/ODataChecklistPayloadMapper.js` : found
- `app/service/shared/delta/DeltaFieldMappers.js` : found
- `app/infra/adapters/shared/ODataChecklistReadRuntime.js` : found
- `app/service/domain/cache/usecases/CacheValidationUseCase.js` : found
- `app/views/fragment/DetailAttachmentsBody.fragment.xml` : found
- `app/service/domain/detail/usecases/LoadAttachmentsUseCase.js` : found
- `app/service/features/detail/runtime/DetailAttachmentOpenRuntime.js` : found

## Primary overengineering zones
- `app/controller/detail/*`
- `app/controller/search/*`
- `app/controller/app/*`
- `app/controller/analytics/*`
- `app/service/domain/detail/*`
- `app/service/domain/search/*`
- `app/service/features/search/runtime/*`

## Primary CSS/DOM risk zones
- `app/styles/modules/23_dialogs.css`
- `app/styles/modules/40_page_search.css`
- `app/styles/modules/41_page_detail.css`
- `app/styles/modules/42_page_analytics.css`
- `app/styles/modules/90_ui5_overrides.css`
