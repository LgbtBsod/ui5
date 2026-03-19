# ERROR REMEDIATION PLAN

## P1

- Remove route and layout behavior that bypasses standard UI5 navigation contracts.
- Keep `FlexibleColumnLayout` under `layout` state control instead of private DOM/CSS patching.
- Maintain product smoke checks against the current `app/` runtime surface.

## P2

- Reduce custom runtime indirection in `search`, `detail`, and `app shell` layers.
- Bind sticky, focus, and viewport behavior only to stable application wrapper ids/classes.
- Keep accessibility landmarks, skip-link behavior, and shell metrics aligned with SAP/Fiori semantics.

## P3

- Shrink private `.sap*` styling overrides over time in favor of wrapper-based styling.
- Keep governance and release evidence separate from product runtime smoke.
- Continue replacing dead compatibility helpers with direct controller to facade/use case flows.
