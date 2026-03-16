#!/usr/bin/env node
const path = require("path");

const target = String(process.argv[2] || "").trim();

const ENTRYPOINTS = Object.freeze({
  "adapter-factory-boundary-gate": "./gates/adapter-factory-boundary-gate",
  "architecture-gate": "./internal/architecture-gate-core",
  "duplicate-responsibility-gate": "./gates/duplicate-responsibility-gate",
  "feature-token-drift-gate": "./gates/feature-token-drift-gate",
  "forbidden-literals-gate": "./gates/forbidden-literals-gate",
  "framework-alias-gate": "./gates/framework-alias-gate",
  "framework-token-drift-gate": "./gates/framework-token-drift-gate",
  "gateway-parity-validator": "./internal/gateway-parity-validator-core",
  "layer-map": "./internal/layer-map-core"
});

if (!target || !ENTRYPOINTS[target]) {
  console.error(
    "Usage: node scripts/run-gate.js <gate-name>\nSupported gates: " +
    Object.keys(ENTRYPOINTS).sort().join(", ")
  );
  process.exit(1);
}

require(path.resolve(__dirname, ENTRYPOINTS[target]));
