# Scenario Suite

Coverage model: Hybrid (automation + structured manual).
## Pass Cadence

- P0: must pass each commit
- P1: must pass each phase
- P2: must pass before release

## Locked Policies
- Pending-save navigation: Block Until Saved
- Invalid save behavior: Save remains available; validation runs on demand/status change
- Themes: Theme A (Morning) / Theme B (Night)
- Breakpoints: 1440 / 1080 / 720

## Matrix

| ID | Phase | Severity | Steps | Expected | Theme A/B | Breakpoints | Keyboard Notes | Automation | Status |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| A1 | P0 | Blocker | 1) Enable browser offline mode. 2) Trigger Save and Autosave. 3) Trigger Search while offline. 4) Retry after network restore. | Offline errors are clear, retry is available, busy state is not stuck, autosave remains non-blocking. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| A2 | P1 | Major | 1) Throttle network (Slow 3G). 2) Run Search/Save over 2 seconds. 3) Verify skeleton/busy policy and no visual flicker. | Skeleton/busy overlay policy is correct, no layout shift/double-toast/flicker, and working message appears over 2 seconds. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| A3 | P0 | Blocker | 1) Simulate timeout/5xx on Save/Search. 2) Verify retry CTA and preserved user input. | Timeout/5xx uses correct severity, retry is available, and user input is preserved. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| A4 | P1 | Major | 1) Expire the session in detail/edit state. 2) Verify a safe recovery path without redirect loops. | Expired session shows clear messaging and one safe recovery path without loops. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| A5 | P1 | Major | 1) Return unexpected payload/contract error. 2) Verify graceful fallback and copyable support id. | Unexpected payload does not crash UI; graceful degradation with copyable support id is present. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| B1 | P0 | Blocker | 1) Create lock conflict. 2) Trigger Edit. 3) Verify UI remains read-only with next-step guidance. | Lock acquire failure keeps UI in read-only mode with actionable next steps. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| B2 | P1 | Major | 1) Hold lock from another session. 2) Handle takeover prompt with accept/decline paths. | Takeover accept/decline paths are deterministic with no stuck edit visuals. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| B3 | P0 | Blocker | 1) Start editing. 2) Kill or expire lock. 3) Verify banner, forced read-only, and unsaved handling. | Lock loss forces read-only, shows persistent warning, and handles unsaved data explicitly. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| B4 | P1 | Major | 1) Open same root in two tabs. 2) Edit in one tab. 3) Verify deterministic conflict feedback in the other tab. | Second tab receives deterministic conflict/read-only feedback with no silent corruption. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| C1 | P0 | Blocker | 1) Make the form dirty. 2) Try route change/back/close/refresh. 3) Verify confirm guard and default focus. | Unsaved guard triggers on route/close/back with correct button behavior and default focus. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| C2 | P0 | Blocker | 1) Start save/autosave. 2) Immediately attempt route change. 3) Verify block-until-saved with auto-resume. | Route change during save/autosave is blocked and resumed automatically after completion. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| C3 | P1 | Major | 1) Open deep-link detail route. 2) Refresh during edit context. 3) Verify safe state restoration. | Refresh/deep-link restores safe detail state with clear status communication. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| D1 | P1 | Major | 1) Blur required fields. 2) Verify touched-only inline errors and required marker consistency. | Field errors appear after touch/blur, stay near field, and required markers are consistent. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| D2 | P1 | Major | 1) Submit with multiple invalid fields. 2) Verify summary and focus jump to selected field. | Cross-field summary appears for multiple errors and focuses target field on click. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| D3 | P0 | Blocker | 1) Create invalid state. 2) Verify single policy for Save/Validate behavior. | Invalid-state Save policy is consistent and documented. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| D4 | P2 | Minor | 1) Use extreme label/value lengths. 2) Verify no overlap/clipping and consistent ellipsis/tooltip behavior. | Long content does not break layout, clip controls, or violate ellipsis/tooltip rules. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| D5 | P2 | Minor | 1) Input non-latin and emoji content where allowed. 2) Verify stable rendering and validation. | Non-latin and emoji inputs render correctly without layout or validation breakage. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| E1 | P0 | Blocker | 1) Verify actions with no selection. 2) Verify multi-selection bulk actions where supported. | Actions are disabled with no selection; bulk action behavior is correct for multi-select. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| E2 | P1 | Major | 1) Apply sort/filter/variant. 2) Save/apply/reset variant. 3) Verify persistence. | Sort/filter/variant save/apply/reset works and persists correctly. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| E3 | P1 | Major | 1) Test top/max bounds and invalid values. 2) Verify result count and load-more coherence. | Top/max bounds validate correctly; result count and load-more are coherent. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| E4 | P1 | Major | 1) Verify Enter primary action behavior. 2) Verify ESC close behavior. 3) Verify stable tab order. | Enter/ESC behavior and tab order remain stable and logical. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| F1 | P0 | Blocker | 1) Open dialog/popover. 2) Verify initial focus and trap. 3) Close and verify focus return. | Focus trap, initial focus, and focus return to trigger work consistently. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| F2 | P1 | Major | 1) Verify popover layering in scrolled containers. 2) Verify in-dialog scrolling and fixed footer. | Overlays are not clipped and dialog scrolling stays inside dialog container. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| F3 | P1 | Major | 1) Compare dialog header/footer spacing and button order across dialogs. | Dialog headers/footers have consistent spacing, hierarchy, and action order. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| G1 | P1 | Major | 1) Trigger repeated autosave events. 2) Verify toast dedupe/throttle behavior. | One event maps to one toast, with dedupe/throttle preventing spam. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| G2 | P0 | Blocker | 1) Trigger success/info/system/high-risk events. 2) Verify toast/banner/dialog severity mapping. | Severity mapping follows policy for toast/banner/dialog usage. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| G3 | P2 | Minor | 1) Verify feedback coloring in both themes. 2) Confirm semantic token usage only. | Semantic token coloring is theme-aware and readable in both themes. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| H1 | P1 | Major | 1) Toggle theme rapidly 10 times. 2) Verify no leaks, artifacts, or background breakage. | Rapid theme toggles do not cause leaks, flash artifacts, or broken background rendering. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| H2 | P2 | Minor | 1) Compare Theme A/B for spacing, radius, elevation, typography, focus, and motion tempo. | Theme A/B has distinct visual feel while keeping shared layout structure. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| H3 | P1 | Major | 1) Enable prefers-reduced-motion. 2) Verify minimal motion without UX degradation. | Reduced motion minimizes transitions while preserving high-quality UX. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| I1 | P0 | Blocker | 1) Execute P0 flows with keyboard only. 2) Verify visible continuous focus ring. | P0 flows are keyboard-operable with visible continuous focus indication. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| I2 | P1 | Major | 1) Verify aria-label for icon-only controls. 2) Verify required/error announcements. | Screen-reader basics are covered: labels, names, required and error announcements. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| I3 | P2 | Minor | 1) Verify contrast for text and controls across states and themes. | Text/control contrast remains readable across themes and semantic states. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| J1 | P2 | Minor | 1) Traverse heavy screens. 2) Verify no scroll jank, layout thrash, or heavy blur usage. | No performance jank from animation, blur, or layout thrash. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| J2 | P1 | Major | 1) Run style:scan and related gates. 2) Verify no new dead/duplicate CSS regressions. | CSS hygiene gates pass with no new duplicate/dead/unsafe style regressions. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| J3 | P1 | Major | 1) Measure startup path. 2) Verify shell/skeleton appears immediately without blocking. | Startup remains responsive with immediate shell/skeleton rendering. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| K1 | P0 | Blocker | 1) Execute dangerous actions (delete/close/finalize). 2) Verify explicit confirmation with consequences. | Dangerous actions require explicit confirmation with clear consequences. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| K2 | P1 | Major | 1) Simulate permission denied. 2) Verify hide/disable states with next-step guidance. | Permission denied states are handled via hide/disable plus clear next steps. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| K3 | P1 | Major | 1) Trigger varied errors. 2) Verify user-friendly copy with no secret leakage. | Error copy is user-friendly with no sensitive data leakage. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| L1 | P1 | Major | 1) Inject long translation strings. 2) Verify layout resilience and no new hardcoded text. | Long translations do not break layout; text remains i18n-driven. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| L2 | P1 | Major | 1) Verify date/time/number locale formatting. 2) Verify consistent 'Saved at' style timestamps. | Date/time/number formatting and saved-at style timestamps follow locale. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |
| M1 | P0 | Blocker | 1) Run feature:scan. 2) Verify empty handlers/TODO triggers/unreachable routes/dialogs are blocked. | Feature scan blocks dead UI triggers and unreachable handlers/routes/dialogs. | Morning / Night | 1440, 1080, 720 | Primary action Enter, ESC for dismissals, stable tab order. | hybrid | PASS |

Blocker status: 0 open in P0.
Major status: 0 failed in P1.
Minor status: 0 failed in P2.

## N/A Rule

N/A is allowed only with both:
- Code-search proof (file + pattern evidence).
- UI confirmation evidence (route/screen capture in QA crawl).

Current run: no scenario marked N/A.
