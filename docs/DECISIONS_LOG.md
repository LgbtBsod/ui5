# Decisions Log

## 2026-03-04 Baseline Documentation

- What changed: created the canonical UX baseline docs for style context, design system, themes, inventory, audit, backlog, and QA.
- Consistency Check: follows `STYLE_CONTEXT.md` principles for shell-first structure, one interaction language, and shared token-based patterns.
- Self-review: current design language is strong enough to preserve, but shell coherence, responsive behavior, and feedback consistency are not yet at flagship quality.
- Next focus: fix responsive table/dialog behavior and establish a real shell header before deeper visual polish.

## 2026-03-04 Shell and Responsive Foundation

- What changed: introduced the shared `AppShellHeader`, added shell notifications/help/settings/user popovers, moved the theme toggle into the shell, aligned effect feedback to `glassDialog` and `glassToast`, and switched detail mobile fallbacks to `appView>/isPhoneViewport`.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for shell-first structure, one interaction language, shared token usage, and viewport-aware responsive behavior.
- Self-review: the first shell pass failed because the brand stack clipped inside toolbar height; revised the shell to a taller glass header with explicit title contrast and re-validated in Morning and Night screenshots.
- Validation: `check-xml-views`, `check-css-architecture`, `interaction-smoke.py`, and baseline screenshots all passed after the shell-aware smoke assertions were updated.

## 2026-03-04 Search and Detail UX Rebuild

- What changed: rebuilt `Search` into a clearer command-center layout, converted `Detail` hero chips into a summary-board composition, strengthened the sticky control rail, and exposed visible labels on key detail actions without changing field logic or controller flows.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for shell-aware sticky layout, search workbench composition, shared glass surfaces, and labeled workflow-critical actions.
- Self-review: the first detail hero revision left too much dead space on the left; revised the hero grid and stat-board density so the workspace reads as one operational panel.
- Validation: `check-xml-views`, `check-css-architecture`, `interaction-smoke.py`, and refreshed baseline screenshots all passed after the UX rebuild.

## 2026-03-04 Theme Density and Shell Semantics

- What changed: tokenized theme differences for shell height, workbench contrast, control density, table row height, and detail stat/rail scale; added visible desktop labels to shell utility actions.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for theme philosophy as behavior, token-first styling, shell-first composition, and no tooltip-only critical context.
- Self-review: refreshed Morning and Night screenshots now read as two clear philosophies; Morning regained actionable search contrast and Night feels denser and more instrument-like without changing IA.
- Validation: `check-xml-views`, `check-css-architecture`, `interaction-smoke.py`, and refreshed baseline screenshots all passed after the token pass.

## 2026-03-04 Search Segment Compactness

- What changed: tightened `resultSegmentControl` inside the search workbench so segment labels truncate cleanly, buttons stop overlapping, and the control stretches to full width on narrow viewports.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for compact search workbench behavior, shared segmented-control treatment, and no overlapping controls at responsive breakpoints.
- Self-review: the segmented filters now read as one compact control instead of three competing pills, and narrow layouts degrade cleanly instead of clipping.

## 2026-03-04 Checklist Delete Context and Search Scroll

- What changed: removed checklist delete from the search discovery toolbar, added it to the detail edit control rail, and restored Search page scrolling by re-enabling page-owned scrolling.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for context-aware destructive actions, shell/detail workflow ownership, and functional controls over decorative placement.
- Self-review: delete now appears where the user has full checklist context, and Search no longer traps the user at the top of the page.
- Validation: `check-xml-views`, `check-css-architecture`, `interaction-smoke.py`, and refreshed baseline screenshots all passed after the flow change.

## 2026-03-04 Row Actions, Segments, and Value Help Stability

- What changed: replaced tooltip-only row icon actions with labeled shared row-action buttons across checks, barriers, attachments, and expanded dialogs; rebuilt search segmented filters as one semantic contour; stabilized location value-help tree-table layout and debounced live search.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for one interaction language, explicit row-action semantics, shared segmented-control behavior, and stable dialog/value-help patterns.
- Self-review: yes, this now reads like one product instead of UI5 defaults leaking through dense tables; the first value-help attempt failed because `layoutData` was attached to `TreeTable` incorrectly, so the layout contract was refactored before re-running runtime QA.
- Validation: `check-xml-views`, `check-css-architecture`, `interaction-smoke.py`, `final-static-qa.js`, refreshed Morning/Night screenshot matrix, and targeted Playwright checks for segmented colors, row-action labels, and location value-help stability all passed.

## 2026-03-04 Header Actions and Keyboard Return Focus

- What changed: upgraded all `Actions` table headers to the shared badge treatment, made the shell user popover runtime-aware, and added explicit focus return for shell popovers, expanded dialogs, and the location value help.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for shell-first context, shared action-column hierarchy, and exact return-focus behavior.
- Self-review: the detail rail now keeps delete visible in edit context, action headers read as one pattern, keyboard traversal returns to the opening control instead of dropping focus, and the edit switch no longer depends on pointer input.
- Reference influence: informed by Apple HIG focus continuity and SAP Fiori guidance on preserving context and predictable object-action affordances.

## 2026-03-04 Relative Sizing and Overflow Cleanup

- What changed: moved shell/detail overflow fixes to relative layout contracts, collapsed user-shell controls earlier, removed tooltip dependency from visible row-action buttons, and made the detail control rail wrap instead of forcing horizontal overflow.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for relative sizing, no tooltip-only critical affordances, and adaptive shell/detail rails.
- Self-review: the previous layout was too monitor-sensitive; the shell and control rail now degrade by wrapping and density instead of turning into a clipped strip.
- Validation: custom responsive overflow scans for `1440/1180/980/820/680` widths returned no clipped shell or detail-rail containers; `check-xml-views`, `check-css-architecture`, `interaction-smoke.py`, and `final-static-qa.js` all passed after removing the stray gateway header.

## 2026-03-04 Context-Aware User Menu and Search Width Contract

- What changed: turned the user popover primary action into a runtime-aware shell action (`Refresh runtime context` vs `Change test user`), strengthened the shared `Actions` header badge treatment, and refactored Search rails/table wrappers to shrink and wrap before overflowing the viewport.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for shell-first context ownership, shared action-column hierarchy, and relative sizing over monitor-specific widths.
- Self-review: the shell user entry now behaves like a real product control instead of a passive label, and narrow Search layouts no longer spill toolbars/table chrome past the viewport edge.
- Validation: `check-xml-views`, `check-css-architecture`, `interaction-smoke.py`, `final-static-qa.js`, responsive overflow scans at `980/820/680/540`, and keyboard-only runtime checks for shell popover/dialog return-focus all passed.

## 2026-03-04 Sticky Rail Timing and Search Landing Cleanup

- What changed: raised the pinned detail control rail slightly so it starts sticking later, moved `WF Analytics` from the landing hero into the shared search action rail, and tightened split/narrow search hero contracts so the stage card and analytics deck stop crowding each other.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for shell-safe sticky offsets, hero-as-orientation instead of action clutter, and adaptive split-mode composition.
- Self-review: the detail rail no longer feels like it jumps onto the page too early, and the search landing reads as one aligned composition instead of competing cards.
- Validation: `check-xml-views`, `check-css-architecture`, `interaction-smoke.py`, and targeted runtime checks confirmed the analytics trigger now lives in the toolbar and the detail sticky pin sits lower in the scroll flow.

## 2026-03-04 ObjectPage Wrapper Surface Cleanup

- What changed: styled `ObjectPage` header/detail wrapper layers to stop leaking raw UI5 white surfaces, and reinforced the shared detail empty-state contract so attachment and other empty states inherit product surfaces instead of looking like collapsed system blocks.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for glass-card ownership of visible surfaces and transparent/full-width `ObjectPage` wrappers.
- Self-review: the attachments/detail area now belongs to the same surface family as the rest of the product instead of showing a stray system-looking block underneath.
- Validation: `check-xml-views`, `check-css-architecture`, and `interaction-smoke.py` all passed after the wrapper cleanup.

## 2026-03-04 Detail State Dedup and Tighter Sticky Rail

- What changed: removed the duplicate standalone workflow chip rail from the detail header, surfaced checklist status directly inside the sticky control rail, hid the placeholder autosave line when it has no value, and reduced the pinned rail gap under the shell.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for non-duplicated workflow state, sticky rail ownership of runtime context, and tighter pinned clearance.
- Self-review: the detail header now reads cleaner, the active checklist state is still visible while scrolling, and the control rail no longer hangs too far below the shell.
- Validation: `check-xml-views`, `check-css-architecture`, and `interaction-smoke.py` all passed after the control-rail cleanup.

## 2026-03-04 Unified Search Date Formatting

- What changed: routed search `Updated` timestamps and analytics refresh timestamps through the shared runtime human-date formatter instead of raw ISO strings or ad-hoc locale calls.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for one interaction language and shared patterns over one-off formatting.
- Self-review: the KPI rail, search meta rail, and analytics dialog now show the same operational date style as the rest of the product; `DateCheck` in base info stays on its existing field-specific contract.

## 2026-03-04 Theme Toggle State Sync and Shell Wrap

- What changed: fixed the shell theme toggle so its thumb position and `aria-checked` state update immediately with the active theme, held the theme-switch visual envelope until the real UI5 theme swap settles, and refactored the shell header to wrap into rows before controls start crushing each other.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for shell-first composition, wrapping before overlap, and immediate theme-toggle state continuity.
- Self-review: the theme switch now reads like a real control instead of a decorative icon pill, and the shell header behaves like one coherent surface on narrower widths instead of a clipped toolbar strip.

## 2026-03-04 Detail Controls and Form Recovery Pass

- What changed: removed the passive `Overview` control-rail anchor, moved workflow analytics into the checklist control rail, fixed status change commands to include the active root context, made row-delete resolve the real row binding path, converted attachment download to a real browser download action, reduced oversized detail info-card spans, and added visible required-field helper lines under invalid edit controls.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for actionable control rails, inline validation visibility, native overflow over clipping, and one interaction language across detail/search/table surfaces.
- Self-review: the checklist control rail now contains only things the user can actually act on, detail forms recover from validation failures more clearly, and the search/detail composition wastes less width on passive blocks.
- Validation: `check-xml-views`, `check-css-architecture`, and `interaction-smoke.py` all passed after moving analytics to the detail rail and tightening the detail/search contracts.

## 2026-03-04 Cache, Lock, and Feedback Runtime Integrity Pass

- What changed: fixed cache validation to read canonical snapshot stamps, switched `LastChangeSet` reads to the gateway entity contract with collection fallback, restored banner/dialog fallbacks so feedback cannot silently disappear, differentiated `LOCK_LOST`/`KILLED`/`EXPIRED` messaging, enabled expired-lock takeover prompts, and added best-effort lock release on tab close via `pagehide` + `sendBeacon`.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for one product truth, no silent states, and architecture-first fixes over one-off UI patches.
- Self-review: edit-mode concurrency now behaves predictably instead of failing quietly, and runtime feedback paths degrade visibly instead of no-oping.
- Validation: `check-xml-views`, `check-css-architecture`, `interaction-smoke.py`, and a live mock-gateway probe confirmed `LastChangeSet?RootId=...` returns a collection while canonical `LastChangeSet('<RootKey>')` returns the entity stamp the cache validator now uses.

## 2026-03-04 Detail Surface Cleanup and Overflow-First Rails

- What changed: tightened `ObjectPage` header/body wrappers to stop leaking white tails under the detail hero and attachments section, converted search/detail action rails to overflow-first behavior instead of wrap/clip, and polished the detail hero plus attachment workspace as one cleaner surface family.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for glass-card ownership of visible surfaces, no raw UI5 white containers, and native `...` overflow before clipping or monitor-dependent wrapping.
- Self-review: the detail page now reads more like one intentional workspace; the hero is calmer, attachment tools stay reachable under hard resize, and the remaining large white blocks should no longer be coming from the page-level layout contract.

## 2026-03-04 Screen-by-Screen Layout Polish Pass

- What changed: restored a proper search orientation stage card next to the KPI deck, tightened search workbench/results spacing, refined detail hero and section rhythm, and unified dialog/popover inner surfaces so overlays read like the same product shell instead of stock UI5 containers.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for command-center search composition, calm orientation surfaces, dense-but-clean work areas, and one dialog/popover language across overlays.
- Self-review: Search regained a useful top-level narrative without slipping back into filler, detail now breathes better between hero/control/content, and shell popovers plus analytics dialog feel noticeably less like a separate UI kit.
- Validation: `check-xml-views`, `check-css-architecture`, and `interaction-smoke.py` all passed after the screen-by-screen polish pass.

## 2026-03-04 Search Support Architecture Gate Cleanup

- What changed: removed the forbidden `controller/support -> service/framework/ComponentRuntimeSupport` dependency by moving the shared search datetime formatter into [SearchControllerSupport.js](/Users/lgbtb/Desktop/sap_ui5/controller/support/SearchControllerSupport.js) and keeping [SearchViewSupport.js](/Users/lgbtb/Desktop/sap_ui5/controller/support/SearchViewSupport.js) inside the allowlisted controller-support layer.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for architecture-first consistency and shared patterns over one-off controller imports.
- Self-review: behavior stayed identical, but the layering is now coherent and static QA no longer has a known red gate.
- Validation: `final-static-qa.js`, `check-xml-views`, and `interaction-smoke.py` all passed after the import cleanup.

## 2026-03-04 Detail White-Tail Cleanup and Borderless Segments

- What changed: forced `DynamicPage` header/content/spacer layers inside detail workspaces to stay transparent and removed the extra outline/shadow contour from search segmented filters so only the semantic fill remains visible.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for product-card ownership of visible surfaces and segmented filters without extra outline chrome.
- Self-review: the remaining white tails now trace to fewer framework layers, and the result filters read as one softer control instead of a pill nested inside another pill.

## 2026-03-04 Cross-Theme Table Typography and Density Pass

- What changed: introduced shared table typography/padding/radius tokens, aligned search/detail/dialog table headers and body rows to the same reading rhythm, restored card-like row surfaces for detail/attachment tables, and tightened split-width search results toward fixed-layout plus ellipsis instead of optical sprawl.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for one shared table language across surfaces and density differences by theme tokens rather than per-screen one-offs.
- Self-review: Morning now reads calmer and airier while Night stays tighter without becoming cramped; table headers finally feel lighter than data rows, and detail/dialog rows read as deliberate surfaces instead of leftover framework cells.
- Validation: tested Morning and Night at `1440/1180/980/820`, then re-ran `check-css-architecture`, `check-xml-views`, and `interaction-smoke.py`.

## 2026-03-04 Search Rail Simplification and Detail DnD/Table Pass

- What changed: removed the non-functional search stage block, kept analytics as a compact four-card rail with `2x2` fallback on narrower widths, added a separate backend `top` input beside the existing max-visible-results control, widened person suggestions, made attachment DnD prime and expand on file-drag intent, added autosave toast feedback, and hardened row-delete payload resolution for checks/barriers.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for non-functional blocks removal, one shared table language with softer row surfaces, and responsive rails that expose real controls instead of decorative filler.
- Self-review: Search now uses its top area for real data rather than decorative copy, detail cards feel less stretched, the attachment drop zone finally behaves like a modern upload target, and the search toolbar exposes backend windowing without overloading the existing max-visible field.
- Validation: `check-xml-views`, `check-css-architecture`, `final-static-qa.js`, `interaction-smoke.py`, plus live runtime checks confirming the analytics stage block is gone and the backend `top` input is present in the SmartTable toolbar.

## 2026-03-04 Night Surface Unification and Viewport-Pinned Checklist Rail

- What changed: unified dark-theme cards, rails, empty states, search workbench surfaces, and detail header wrappers to one calmer matte fill without glow or hotspot highlights, and switched the checklist control rail to a viewport-pinned inner card so it can keep traveling through the full detail scroll instead of stopping at the first subsection boundary.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for one calm Night surface family, no bright dark-theme highlights, and sticky checklist controls that pin against shell clearance through the full object-page scroll.
- Self-review: Night now reads closer to one controlled enterprise surface system instead of a mix of glowing and matte blocks, and the checklist rail behaves like a real persistent control cockpit instead of dying after the base-info block.
- Validation: `check-css-architecture`, `check-xml-views`, `final-static-qa.js`, and `interaction-smoke.py` all passed after the dark-surface and pinned-rail pass.

## 2026-03-04 Split Scroll Ownership and Resize Resync

- What changed: removed scroll ownership from outer splitter pane wrappers and XMLView hosts so the no-op right-side scrollbars disappear, and added a splitter resize resync on viewport changes so detail workspaces expand with the window instead of leaving stale empty space after resize.
- Consistency Check: follows `STYLE_CONTEXT.md` rules that only active workspace containers may own scrollbars and split/detail geometry must resync on viewport resize.
- Self-review: split mode now reads cleaner because the pane chrome stopped exposing decorative scrollbars, and the detail workspace no longer has to live inside an old host width when the window grows.
- Validation: `check-css-architecture`, `check-xml-views`, `final-static-qa.js`, and `interaction-smoke.py` all passed after the scroll-ownership and resize-sync pass.

## 2026-03-04 Search SmartTable Split Pop-in Strategy

- What changed: added an explicit column-width and `demandPopin` policy for the SmartTable inner `sap.m.Table`, keeping checklist id and core status fields in-row longest while moving secondary fields into pop-in earlier; also styled pop-in rows as part of the same table language instead of leaving stock UI5 fallback chrome.
- Consistency Check: follows `STYLE_CONTEXT.md` rules for fixed-layout search tables, explicit split-width column priority, and pop-in rows that continue the main table system rather than becoming a separate UI dialect.
- Self-review: split-mode search results now have a clearer hierarchy under compression, and narrow widths degrade into readable label/value pop-ins instead of feeling like the table is simply running out of room.
- Validation: `check-css-architecture`, `check-xml-views`, `final-static-qa.js`, and `interaction-smoke.py` all passed after the SmartTable column-priority and pop-in pass.
## 2026-03-06 Manifest Domain Model Contract Recovery

- What changed: restored the `data` and `mpl` JSON model registrations in `manifest.json` and component bootstrap so the runtime model contract matches `ModelFactory` and `domain-model-verify` again.
- Consistency Check: keeps model registration aligned across manifest, component init, and domain verification instead of letting legacy factories drift from runtime wiring.
- Self-review: this is intentionally small but important; the previous state let QA stay green while the explicit domain-model verifier was red, which is the wrong contract boundary.
- Validation: `npm.cmd run domain-model:verify`, `npm.cmd run qa`, and targeted model-contract inspection all pass after the recovery.