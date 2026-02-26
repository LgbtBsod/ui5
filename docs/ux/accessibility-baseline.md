# Accessibility baseline (C2)

- Target: WCAG 2.2 AA.
- UI5 profile: keyboard-first, focus-visible always enabled, semantic labels required for interactive controls and dialogs.
- Reduced motion policy: `prefers-reduced-motion` must disable non-essential animations.

## Manual protocol
1. Keyboard flow: Search -> open detail -> edit -> save/cancel.
2. Keyboard flow: unsaved-close prompt -> stay/discard/save.
3. Verify focus order on toolbar, filters, table rows, control rail actions.
4. Verify message strips are readable in Morning/Night and at 200% zoom.
5. Verify all icon-only actions provide tooltip or aria text.

## CI gate behavior
`node scripts/a11y-gate.js` fails if required docs/protocol/reduced-motion policy/tooltip baseline rules are broken.
