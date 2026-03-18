#!/usr/bin/env node
const fs = require("fs");
const path = require("path");

const root = process.cwd();

function read(relPath) {
  return fs.readFileSync(path.join(root, relPath), "utf8");
}

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function extractTableStatuses(markdown) {
  return markdown
    .split(/\r?\n/)
    .filter((line) => /^\|/.test(line) && !/---/.test(line))
    .map((line) => line.split("|").map((part) => part.trim()).filter(Boolean));
}

const bootstrapRuntime = read("app/ui5-bootstrap-runtime.js");
const evidenceMatrix = read("backend/sap_backend/EVIDENCE_ACCEPTANCE_MATRIX.md");
const signoffTracker = read("backend/sap_backend/OWNER_SIGNOFF_TRACKER.md");
const remediationPlan = read("docs/audit/ERROR_REMEDIATION_PLAN.md");

assert(
  bootstrapRuntime.includes("https://ui5.sap.com/1.71.70/resources/sap-ui-core.js") ||
  bootstrapRuntime.includes('"/resources/sap-ui-core.js"'),
  "Bootstrap runtime must declare an explicit UI5 source"
);

assert(
  remediationPlan.includes("## P1") && remediationPlan.includes("## P2") && remediationPlan.includes("## P3"),
  "Error remediation plan must keep P1/P2/P3 sections"
);

["EV-003", "EV-004", "EV-005", "EV-006"].forEach((evidenceId) => {
  assert(evidenceMatrix.includes(evidenceId), `Evidence matrix is missing ${evidenceId}`);
});

const signoffRows = extractTableStatuses(signoffTracker);
const requiredOwners = [
  "Solution architect",
  "ABAP developer",
  "Basis/Gateway owner",
  "Security / PFCG",
  "UX / QA",
  "Product owner",
  "Sponsor / release authority"
];

requiredOwners.forEach((owner) => {
  const row = signoffRows.find((parts) => parts[0] === owner);
  assert(row, `Owner sign-off tracker is missing row for ${owner}`);
  assert(["OPEN", "IN_PROGRESS", "ACCEPTED"].includes(row[3]), `Owner ${owner} has invalid status ${row[3]}`);
});

console.log("release-readiness-gate passed");
