#!/usr/bin/env node
const fs = require("fs");
const path = require("path");

const ROOT = process.cwd();
const bundles = [
  path.join(ROOT, "app", "i18n", "i18n.properties"),
  path.join(ROOT, "app", "i18n", "i18n_en.properties"),
  path.join(ROOT, "app", "i18n", "i18n_ru.properties")
];
const requiredKeys = [
  "persistenceIdle",
  "persistenceDirty",
  "persistenceSaving",
  "persistenceAutosaving",
  "persistenceSaved",
  "persistenceSavedAt",
  "persistenceNoChanges",
  "persistenceLockLost",
  "persistenceLockExpired",
  "persistenceLockNotOwned",
  "persistenceLockStolen",
  "persistenceLockMissing",
  "persistenceConflict",
  "persistencePermissionDenied",
  "persistenceValidationError",
  "persistenceNetworkError",
  "persistenceTechnicalError"
];
const filesToCheck = [
  path.join(ROOT, "app", "service", "domain", "detail", "DetailPersistenceRuntime.js"),
  path.join(ROOT, "app", "views", "fragment", "DetailControlStatusRow.fragment.xml")
];
const forbiddenLiterals = [
  "Saving...",
  "Autosaving...",
  "Not saved",
  "Saved"
];

function fail(message) {
  console.error(`Persistence i18n gate failed: ${message}`);
  process.exit(1);
}

bundles.forEach((filePath) => {
  const text = fs.readFileSync(filePath, "utf8");
  requiredKeys.forEach((key) => {
    if (!new RegExp(`^${key}=`, "m").test(text)) {
      fail(`${path.relative(ROOT, filePath)} is missing key ${key}`);
    }
  });
});

filesToCheck.forEach((filePath) => {
  const text = fs.readFileSync(filePath, "utf8");
  forbiddenLiterals.forEach((literal) => {
    if (text.includes(`"${literal}"`) || text.includes(`'${literal}'`)) {
      fail(`${path.relative(ROOT, filePath)} contains hardcoded UI literal ${literal}`);
    }
  });
});

console.log("Persistence i18n gate passed.");
