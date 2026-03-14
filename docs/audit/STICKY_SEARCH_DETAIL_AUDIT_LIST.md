# Sticky Search / Detail Audit List

Date: 2026-03-14

Purpose: classify the current sticky/search/detail implementation into `safe to keep`, `needs hardening`, and `rewrite first` with concrete file references and priorities.

## Safe To Keep

### P3

- `SearchStickyLayoutRuntime` offset orchestration in [SearchStickyLayoutRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchStickyLayoutRuntime.js)
  Reason: the overall pattern is correct. It coordinates sticky offsets through CSS variables instead of mutating internal DOM of UI5 controls.

- Search focus recovery in [SearchSelectionFocusRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchSelectionFocusRuntime.js)
  Reason: fallback focus restoration is useful and aligned with enterprise search-to-detail navigation.

- Detail section anchor rail in [DetailSectionAnchorRail.fragment.xml](/C:/Users/lgbtb/Desktop/ui5/app/views/fragment/DetailSectionAnchorRail.fragment.xml)
  Reason: thin navigation layer over `ObjectPage` is acceptable and improves large-form usability.

- Detail pinned rail de-escalation in [DetailActionPinnedRailRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailActionPinnedRailRuntime.js)
  Reason: risky viewport-pinning logic is already neutralized, which lowers regression risk.

## Needs Hardening

### P2

- Scoped DOM resolution in [SearchStickyDomRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchStickyDomRuntime.js)
  Reason: some fallback height lookups depend on global selectors and should stay scoped to the current view whenever possible.

- Search results toolbar targeting in [SearchViewportRuntime.js](/C:/Users/lgbtb/Desktop/ui5/app/service/features/search/runtime/SearchViewportRuntime.js)
  Reason: toolbar scrolling should target the actual toolbar first, not the whole results shell, to reduce focus/scroll ambiguity.

- Sticky fallback offsets in [44_search_sticky_layout.css](/C:/Users/lgbtb/Desktop/ui5/app/styles/modules/search/44_search_sticky_layout.css)
  Reason: several hardcoded fallback heights are too magic-number-heavy and are fragile under toolbar wrapping and density changes.

- Mobile detail rail wrapping in [49_detail_rail_responsive.css](/C:/Users/lgbtb/Desktop/ui5/app/styles/modules/detail/49_detail_rail_responsive.css)
  Reason: behavior is mostly acceptable, but still needs explicit validation against long labels, action overflow, and virtual keyboard overlap.

## Rewrite First

### P1

- None currently mandatory for repo-side sticky/search/detail code.

Reason:
- the previously most dangerous area, custom DOM mutation inside shell controls, has already been removed;
- the remaining problems are mostly hardening issues, not immediate rewrite blockers.
