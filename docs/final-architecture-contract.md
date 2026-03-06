# Final Architecture Contract

This repository is frozen around a gateway-first UI5 architecture.

## Layer Direction

Allowed dependency direction:

`controller -> facade -> service/domain/usecases -> ports -> infra/adapters -> service/backend`

Controllers may also import pure helpers from `controller/support`, `util`, `model`, and `service/framework`.

Forbidden:

- controller -> infra/backend direct calls
- usecase -> controller/UI5 imports
- infra -> controller/usecase/backend reverse imports

## Canonical State Ownership

`state` is the only canonical workflow state model.

Canonical workflow paths live in `state`, including:

- `/mode`
- `/lockOperationState`
- `/autosaveEnabled`
- `/isDirty`
- `/activeObjectId`
- `/selectedId`
- `/splitLayoutMode`
- `/timers`

`selected` is the canonical editable detail snapshot for UI bindings.

`uiState` is ephemeral UI/runtime support only. It may mirror detail snapshots for bridge/runtime reasons, but must not become a second workflow source of truth.

## Detail Flow Contract

- `Detail.view.xml` binds editable fields to `selected`.
- `SaveDetailUseCase` and `AutosaveDetailUseCase` must prefer `selected` as current snapshot input.
- Create sentinel logic must be centralized in `util/CreateSentinel.js`.
- Detail controller formatters and UI-only helpers should live in `controller/support/*` where reasonable.

## Search Flow Contract

- `Search.controller.js` stays orchestration-light.
- Search smart-control/session helpers live in `controller/support/SearchControllerSupport.js`.
- Search rate rendering lives in `controller/support/SearchRateProgress.js`.
- Selecting a row and opening a row are separate interactions.

## Startup Contract

- `Component.js` remains bootstrap-only orchestration.
- Runtime bootstrap support lives in `service/framework/ComponentRuntimeSupport.js`.
- Runtime config order is fixed:
  1. defaults
  2. env
  3. backend runtime settings
  4. sanitize/apply
  5. start managers
- `RuntimeSettingsSet('GLOBAL')` is mandatory for runtime hydration; UI runtime must not downgrade into a local defaults fallback branch when Gateway settings load fails.

## Gateway-Only Contract

- Runtime transport is SAP Gateway OData only.
- `fetch`, `XMLHttpRequest`, `axios`, `GatewayDirectHttp`, manual REST fallback paths, and ad hoc jQuery transport are forbidden on the runtime path.
- `sap/ui/thirdparty/jquery` transport access is allowed only in `service/backend/GatewayODataTransport.js`.
- `ODataModel` construction stays centralized in `Component.js`.

## Smart OData Contract

- Search flow is anchored on `sap.ui.comp` Smart controls over `ChecklistSearchSet`.
- `Search.view.xml` must keep `SmartFilterBar` + `SmartTable` bound to the same OData entity set.
- Search orchestration must stay facade-driven; controllers must not call `mainService.read/create/remove/callFunction/submitChanges` directly.
- Search result rendering must not regress into local fallback tables outside `SmartTable`.

## Localization Contract

- Russian is the primary product locale.
- `manifest.json` must keep `fallbackLocale = "ru"`.
- `i18n_ru.properties` must be complete and key-aligned with `i18n.properties`.

## UI / UX Contract

- Design language is `glass + air + light`.
- Theme contract targets `sap_horizon` and `sap_horizon_dark`.
- Cupertino/macOS bridge styling is allowed only through the shared style token system.

## Architecture Freeze Thresholds

These are enforced by automated gates:

- `controller/Search.controller.js <= 250` lines
- `controller/Detail.controller.js <= 550` lines
- `Component.js <= 700` lines
- `css/style.css <= 2200` lines

## Governance

- Every new change must preserve this structure.
- New helpers should be extracted into the existing support layers rather than duplicating orchestration in controllers or `Component.js`.
- QA, architect audit, and UDOS must validate against this contract.
