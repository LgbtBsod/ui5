# ERROR REMEDIATION PLAN

## P1

- Lock release on leave now stays inside the OData/Gateway stack and forwards `async` mode down to the function import request path instead of introducing a parallel REST transport.
- Maintain product smoke checks against the current `app/` runtime surface.

## P2

- Reduce custom runtime indirection in `search`, `detail`, and `app shell` layers.
- Bind sticky, focus, and viewport behavior only to stable application wrapper ids/classes.
- Keep accessibility landmarks, skip-link behavior, and shell metrics aligned with SAP/Fiori semantics.
- Analytics controller graph flattened into a single controller runtime; obsolete year/drilldown/refresh wrappers removed from the production path.
- `DetailFacade` and `DetailService` now share one entry-adapter runtime for enter-edit/discard/use-case execution without changing their separate public entrypoints.
- Checks and barriers expanded-row dialogs now use one shared fragment template instead of duplicated dialog markup.
- OData binary root-key normalization is now centralized instead of being reimplemented in multiple adapter paths.

## P3

- Shrink private `.sap*` styling overrides over time in favor of wrapper-based styling.
- Keep governance and release evidence separate from product runtime smoke.
- Continue replacing dead compatibility helpers with direct controller to facade/use case flows.
- Shared table skin moved to `24_table_common.css`; remaining private table selectors are now concentrated in one module instead of being duplicated across `detail`, `dialogs`, and `search`.
- Background theme attributes are now written through the background runtime API, reducing overlapping shell/theme DOM side effects.
