# Message taxonomy (C4)

## Severity levels
- info: neutral guidance.
- success: completed expected action.
- warning: recoverable risk or degraded mode.
- error: failed action requiring user retry.
- critical: integrity/safety risk requiring immediate attention.

## Tone rules
- concise, action-oriented, non-blaming.
- one sentence primary text; optional follow-up action.
- consistent in Morning/Night themes.

## Ownership
- `search.*` -> Search team.
- `detail.*` -> Detail team.
- `system.*` -> Platform/UI foundation team.

Catalog completeness and naming are enforced by `node scripts/message-taxonomy-gate.js`.
