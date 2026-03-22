# CSS DOM Violations

## CSS internal SAP selector debt

High-risk files
- `app/styles/modules/23_dialogs.css`
- `app/styles/modules/40_page_search.css`
- `app/styles/modules/41_page_detail.css`
- `app/styles/modules/42_page_analytics.css`
- `app/styles/modules/90_ui5_overrides.css`

Observed violation patterns
- direct styling of `.sapMBtnInner`
- direct styling of `.sapMInputBaseContentWrapper`
- direct styling of `.sapMSegBBtn`, `.sapMSegBBtnInner`
- direct styling of `.sapMITB*`
- direct styling of `.sapMDialog*`, `.sapMPopover*`
- direct styling of `.sapUxAP*`
- direct styling of `.sapMList*`, `.sapMListTbl*`
- direct styling of `.sapUiView`, `.sapUiTable*`, `.sapUiIcon`

Assessment
- This is a large SAP internal styling patch surface.
- Some selectors may be unavoidable in UI5 1.71, but the current quantity is too high to treat as standard practice.

Target strategy
- Replace where possible with app-owned wrapper classes on outer controls.
- Keep minimal SAP-internal overrides only where the control does not expose a supported styling hook.
- Document remaining unavoidable selectors as residual risk.

## DOM/runtime hacks

Observed patterns
- direct `document.body` manipulation
- `classList` mutations on raw DOM nodes
- `getDomRef()` + `offsetWidth` reflow tricks
- `scrollIntoView()` for UX navigation/focus

Files with notable DOM access
- `app/controller/app/AppDomBehavior.js`
- `app/controller/detail/AttachmentDropZoneBindingRuntime.js`
- `app/controller/detail/DetailActionPinnedRailRuntime.js`
- `app/controller/detail/internal/DetailValidationFocusRuntime.js`
- `app/service/features/detail/runtime/DetailStateActionRuntime.js`
- `app/service/features/search/runtime/SearchScrollRuntime.js`
- `app/service/features/search/runtime/SearchStickyLayoutRuntime.js`
- `app/service/features/search/runtime/SearchViewportRuntime.js`
- `app/service/framework/ControlStyleRuntime.js`
- `app/service/framework/ThemeDomRuntime.js`

Assessment
- Some DOM access is legitimate for scrolling/focus/layout in UI5 1.71.
- Reflow-driven style restarts and SAP-internal CSS coupling need targeted reduction.
