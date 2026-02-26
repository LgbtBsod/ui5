#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

node scripts/unit-smoke.js --json
node scripts/enterprise-readiness-gate.js
node scripts/backend-contract-drift-gate.js
node scripts/backend-semver-policy-gate.js
