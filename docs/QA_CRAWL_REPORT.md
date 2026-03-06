# QA Crawl Report

Generated at: 2026-03-05T23:38:56.106425+00:00

## Execution Matrix

- Themes: morning, night
- Breakpoints: 720, 1080, 1440
- Viewports: desktop, tablet, phone
- Runtime page errors: 0

| Viewport | Enter | Tab | ESC |
| --- | --- | --- | --- |
| desktop | PASS | PASS | PASS |
| tablet | PASS | PASS | PASS |
| phone | PASS | PASS | PASS |

## Click Inventory

- `view/App.view.xml` [shell]: handlers=onCopyFeedbackCorrelationId, onGlobalBannerRetry; dialogControls=none
- `view/Detail.view.xml` [detail]: handlers=onAddBarrierRow, onAddCheckRow, onAttachmentUploadChange, onCloseDetail, onDeleteAttachment, onDeleteBarrierRow, onDeleteCheckRow, onExpandBarriers, onExpandChecks, onOpenAttachment; dialogControls=none
- `view/Search.view.xml` [search]: handlers=onBackendTopChange, onBarriersFailSegmentChange, onBeforeSmartTableRebind, onChecksFailSegmentChange, onClearSelection, onCopy, onCreate, onMaxRowsChange, onOpenSelected, onSearchModeToggle, onSelectVisibleRows, onSmartFilterChanged, onSmartFilterInitialise, onSmartSearch, onSmartTableInitialise; dialogControls=none
- `view/fragment/BarriersExpandedDialog.fragment.xml` [dialog]: handlers=onCloseBarriersExpanded, onDeleteBarrierRow; dialogControls=Dialog
- `view/fragment/ChecksExpandedDialog.fragment.xml` [dialog]: handlers=onCloseChecksExpanded, onDeleteCheckRow; dialogControls=Dialog
- `view/fragment/DetailControlRail.fragment.xml` [detail]: handlers=onArmDeleteChecklist, onCancelEditFromDetail, onChangeChecklistStatus, onCloseDetail, onConfirmDeleteChecklist, onCopyDetailLink, onFocusFirstInvalid, onJumpToDetailSection, onSaveDetail, onToggleDetailFullscreen, onValidateChecklist; dialogControls=none
- `view/fragment/LocationValueHelpDialog.fragment.xml` [dialog]: handlers=onCloseLocationValueHelp, onConfirmLocationValueHelp, onLocationValueHelpSearchSubmit; dialogControls=Dialog
- `view/fragment/LockKilledBanner.fragment.xml` [detail]: handlers=none; dialogControls=none
- `view/fragment/LockSwitchStatus.fragment.xml` [detail]: handlers=onToggleEdit; dialogControls=none
- `view/fragment/SearchLoadStatePanel.fragment.xml` [search]: handlers=onRetrySearchLoad; dialogControls=none
- `view/fragment/ShellHelpPopover.fragment.xml` [detail]: handlers=none; dialogControls=none
- `view/fragment/ShellNotificationsPopover.fragment.xml` [detail]: handlers=none; dialogControls=none
- `view/fragment/ShellSettingsPopover.fragment.xml` [detail]: handlers=onToggleBackgroundInteraction, onToggleCompactDensity, onToggleShellHints, onToggleThemeAnimation; dialogControls=none
- `view/fragment/ShellUserPopover.fragment.xml` [detail]: handlers=onShellUserPrimaryAction; dialogControls=none
- `view/fragment/SkeletonFormSection.fragment.xml` [detail]: handlers=none; dialogControls=none
- `view/fragment/SkeletonList.fragment.xml` [detail]: handlers=none; dialogControls=none
- `view/fragment/SkeletonTableSection.fragment.xml` [detail]: handlers=none; dialogControls=none
- `view/fragment/TestUserDialog.fragment.xml` [dialog]: handlers=onConfirmTestUser; dialogControls=Dialog
- `view/fragment/WorkflowAnalyticsBreakdowns.fragment.xml` [detail]: handlers=none; dialogControls=none
- `view/fragment/WorkflowAnalyticsDialog.fragment.xml` [dialog]: handlers=onCloseWorkflowAnalytics; dialogControls=Dialog
- `view/fragment/WorkflowAnalyticsTopline.fragment.xml` [detail]: handlers=none; dialogControls=none

## Runtime Action Trail

| Category | Scenario | Interaction | Action | Result |
| --- | --- | --- | --- | --- |
| menu-shell | SHELL | click | open notifications overlay and close with Escape | PASS |
| menu-shell | SHELL | click | open help overlay and close with Escape | PASS |
| menu-settings | SHELL | toggle | toggle settings hints switch and restore value | PASS |
| menu-shell | SHELL | click | open settings overlay and close with Escape | PASS |
| dialog-shell | SHELL | click | open analytics overlay and close with Escape | PASS |
| menu-shell | SHELL | click | open user overlay and close with Escape | PASS |
| network-backend | A2 | action | slow network shows >2s working hint | PASS |
| network-backend | A2 | action | slow network does not shift layout | PASS |
| network-backend | A4 | action | auth expiry maps to session banner with correlation id | PASS |
| network-backend | A5 | action | unexpected payload degrades gracefully with support id | PASS |
| concurrency-locking | B2 | action | takeover flow exposes confirm, takeover, and cancel outcomes | PASS |
| concurrency-locking | B4 | keyboard-tab | cross-tab lock ownership forces deterministic read-only downgrade | PASS |
| dirty-navigation | C3 | action | deep-link refresh restores safe detail state | PASS |
| validation-forms | D1 | action | open create workspace for validation checks | PASS |
| validation-forms | D1 | action | blur does not show errors before submit | PASS |
| validation-forms | D1 | action | required markers track required field inventory | PASS |
| validation-forms | D2 | action | validation summary appears on submit | PASS |
| validation-forms | D4 | action | extreme content does not break layout | PASS |
| validation-forms | D5 | action | unicode and emoji are preserved in model bindings | PASS |
| tables-powerflows | E3 | action | search max/top bounds normalize and clamp correctly | PASS |
| tables-powerflows | E4 | action | desktop: Alt+1 focuses filters | PASS |
| tables-powerflows | E4 | action | desktop: Alt+2 focuses results | PASS |
| tables-powerflows | E4 | action | desktop: Alt+3 focuses toolbar | PASS |
| tables-powerflows | E4 | keyboard-enter | desktop: Enter triggers create | PASS |
| tables-powerflows | E4 | keyboard-tab | desktop: tab order remains deterministic | PASS |
| tables-powerflows | E4 | keyboard-esc | desktop: ESC closes dialog | PASS |
| tables-powerflows | E4 | action | tablet: Alt+1 focuses filters | PASS |
| tables-powerflows | E4 | action | tablet: Alt+2 focuses results | PASS |
| tables-powerflows | E4 | action | tablet: Alt+3 focuses toolbar | PASS |
| tables-powerflows | E4 | action | tablet: fallback click create | PASS |
| tables-powerflows | E4 | keyboard-enter | tablet: Enter triggers create | PASS |
| tables-powerflows | E4 | keyboard-tab | tablet: tab order remains deterministic | PASS |
| tables-powerflows | E4 | keyboard-esc | tablet: ESC closes dialog | PASS |
| tables-powerflows | E4 | action | phone: Alt+1 focuses filters | PASS |
| tables-powerflows | E4 | action | phone: Alt+2 focuses results | PASS |
| tables-powerflows | E4 | action | phone: Alt+3 focuses toolbar | PASS |
| tables-powerflows | E4 | action | phone: fallback click create | PASS |
| tables-powerflows | E4 | keyboard-enter | phone: Enter triggers create | PASS |
| tables-powerflows | E4 | keyboard-tab | phone: tab order remains deterministic | PASS |
| tables-powerflows | E4 | keyboard-esc | phone: ESC closes dialog | PASS |
| dialogs-overlays | F2 | action | desktop: dialog initial focus | PASS |
| dialogs-overlays | F2 | action | desktop: dialog scroll isolation | PASS |
| dialogs-overlays | F2 | action | desktop: focus returns to trigger | PASS |
| dialogs-overlays | F2 | action | tablet: dialog initial focus | PASS |
| dialogs-overlays | F2 | action | tablet: dialog scroll isolation | PASS |
| dialogs-overlays | F2 | action | tablet: focus returns to trigger | PASS |
| dialogs-overlays | F2 | action | phone: dialog initial focus | PASS |
| dialogs-overlays | F2 | action | phone: dialog scroll isolation | PASS |
| dialogs-overlays | F2 | action | phone: focus returns to trigger | PASS |
| dialogs-overlays | F3 | action | dialog headers and footers stay consistent | PASS |
| feedback-hygiene | G1 | action | toast dedupe prevents duplicate notifications | PASS |
| feedback-hygiene | G3 | action | tokenized semantic coloring gate | PASS |
| themes-visual | H1 | action | desktop: rapid toggle x10 | PASS |
| themes-visual | H1 | action | tablet: rapid toggle x10 | PASS |
| themes-visual | H1 | action | phone: rapid toggle x10 | PASS |
| themes-visual | H2 | action | desktop: theme profile contrast | PASS |
| themes-visual | H2 | action | tablet: theme profile contrast | PASS |
| themes-visual | H2 | action | phone: theme profile contrast | PASS |
| themes-visual | H3 | action | reduced motion preference collapses heavy animation | PASS |
| accessibility | I2 | action | icon-only controls expose label or tooltip and invalid fields announce aria-invalid | PASS |
| accessibility | I3 | action | semantic text contrast remains readable in both themes | PASS |
| performance-polish | J1 | action | animation budget stays within smooth frame budget | PASS |
| performance-polish | J2 | action | style:scan gate | PASS |
| performance-polish | J3 | action | startup renders shell and smart table quickly | PASS |
| security-integrity | K2 | action | permission denied routes to read-only and denied state | PASS |
| security-integrity | K3 | action | error normalization avoids leaking raw sensitive text | PASS |
| i18n-l10n | L2 | action | locale date/time/number formatting path is active | PASS |

## Category Coverage

| Category | PASS | FAIL | Total |
| --- | --- | --- | --- |
| accessibility | 2 | 0 | 2 |
| concurrency-locking | 2 | 0 | 2 |
| dialog-shell | 1 | 0 | 1 |
| dialogs-overlays | 10 | 0 | 10 |
| dirty-navigation | 1 | 0 | 1 |
| feedback-hygiene | 2 | 0 | 2 |
| i18n-l10n | 1 | 0 | 1 |
| menu-settings | 1 | 0 | 1 |
| menu-shell | 4 | 0 | 4 |
| network-backend | 4 | 0 | 4 |
| performance-polish | 3 | 0 | 3 |
| security-integrity | 2 | 0 | 2 |
| tables-powerflows | 21 | 0 | 21 |
| themes-visual | 7 | 0 | 7 |
| validation-forms | 6 | 0 | 6 |

## Screenshot Evidence

- `docs/artifacts/manual-p1p2/cross-tab-conflict.png`
- `docs/artifacts/manual-p1p2/detail-long-content.png`
- `docs/artifacts/manual-p1p2/dialog-consistency.png`
- `docs/artifacts/manual-p1p2/dialog-layering-desktop.png`
- `docs/artifacts/manual-p1p2/dialog-layering-phone.png`
- `docs/artifacts/manual-p1p2/dialog-layering-tablet.png`
- `docs/artifacts/manual-p1p2/reduced-motion-desktop.png`
- `docs/artifacts/manual-p1p2/shell-analytics-open.png`
- `docs/artifacts/manual-p1p2/shell-help-open.png`
- `docs/artifacts/manual-p1p2/shell-notifications-open.png`
- `docs/artifacts/manual-p1p2/shell-settings-open.png`
- `docs/artifacts/manual-p1p2/shell-user-open.png`
- `docs/artifacts/manual-p1p2/slow-network-working-hint.png`
- `docs/artifacts/manual-p1p2/theme-desktop-morning.png`
- `docs/artifacts/manual-p1p2/theme-desktop-night.png`
- `docs/artifacts/manual-p1p2/theme-phone-morning.png`
- `docs/artifacts/manual-p1p2/theme-phone-night.png`
- `docs/artifacts/manual-p1p2/theme-tablet-morning.png`
- `docs/artifacts/manual-p1p2/theme-tablet-night.png`

## What Broke

- No open broken items in automated crawl inputs.

## What Was Fixed

- [Blocker] Route guard now blocks on in-flight save/autosave and resumes pending navigation.
- [Blocker] Save remains available in edit/create; validation is explicit via Check and status-change gates.
- [Blocker] Delete action now requires explicit confirmation dialog.
- [Major] Cross-tab lock conflict signaling added via BroadcastChannel + storage fallback.
- [Major] Global persistent banner bound in shell with retry routing and correlation ID copy path.
- [Major] Search load error panel now includes offline/timeout explanation and retry CTA.
- [Major] QA crawl now includes automated shell menu/popover/dialog clicks with runtime proof trail.
- [Minor] Toast dedupe/throttle window added to reduce autosave message spam.

## What Remains

- No remaining manual items.
