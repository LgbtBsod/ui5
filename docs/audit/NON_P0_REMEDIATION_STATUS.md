# Non-P0 Remediation Status

## Implemented

- Release hygiene split into `validate:local` and `validate:release`.
- Release gate now requires confirmed productive UI5 runtime documentation before release validation can pass.
- `sap.ui.fl` removed from static dependencies and `flexEnabled` disabled because the app currently runs as a single-theme, non-flex application.
- Theme metadata inlined into `ThemeService`; dead `ThemePhilosophy.js` removed.
- UI5 override CSS consolidated into `90_ui5_overrides.css`.
- Control host/state/skin CSS consolidated into `26_controls.css`.
- Duplicate app feedback banners moved into reusable fragment host.
- `Detail.view.xml` layout wrappers reduced without changing visible structure.
- `zcl_zodata_dpc_ext` dependency lifecycle centralized and repeated non-P0 authority checks extracted into helpers.
- `zcl_zodata_rtti_cache` simplified to process-local singleton cache.
- Legacy lock wrapper methods explicitly marked as compatibility APIs.
- Thin override wrapper modules removed where they only proxied `OverrideHandlerFactory`.

## Deferred Until Contract Migration

- `ChecklistBasicInfoSet` is still active.
  This is an intentional separate CDS-backed entity contract, not a duplicate of `ChecklistRootSet`.
  Productive SAP reads basic info from its own table/CDS model alongside root, checks, barriers, and attachments.
  The local ABAP sample must not replace this with a hand-written read service.
  For CDS reference entities, read access should stay on the generated `SEGW/SADL` path.
  Local metadata was aligned accordingly: `ChecklistBasicInfoSet` remains filter-required and read-only.
  Frontend detail reads now use an explicit per-entity filter contract so `RootKey` vs `RootId` usage is centralized instead of duplicated as magic strings.

- `detailLayout` route migration is complete.
  Navigation now uses a single `detail` route with optional layout state in route arguments.

## Guardrails

- No P0 authorization/security behavior was changed.
- No create-permission logic was changed.
- All applied changes preserve current runtime behavior and were validated with local checks and mock Gateway tests.
