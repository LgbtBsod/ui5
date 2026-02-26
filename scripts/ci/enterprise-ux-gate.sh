#!/usr/bin/env bash
set -euo pipefail
node scripts/ux-state-coverage-gate.js
node scripts/a11y-gate.js
node scripts/message-taxonomy-gate.js
node scripts/semantic-contrast-gate.js
node scripts/css-accent-governance-gate.js
node scripts/theme-parity-gate.js
node scripts/pair-snapshots-gate.js
node scripts/control-token-mapping-gate.js
node scripts/theme-preferences-contract-gate.js
node scripts/ux-slo-report.js
