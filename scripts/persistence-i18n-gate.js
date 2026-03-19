#!/usr/bin/env node
const fs = require("fs");
const path = require("path");

const ROOT = process.cwd();
const bundles = [
  path.join(ROOT, "app", "i18n", "i18n.properties"),
  path.join(ROOT, "app", "i18n", "i18n_en.properties"),
  path.join(ROOT, "app", "i18n", "i18n_ru.properties")
];
const MOJIBAKE_PATTERN = /[ÐÑ][\u0080-\u00BF]/;
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

function parseProperties(text) {
  const result = new Map();
  text.split(/\r?\n/).forEach((line) => {
    const trimmed = line.trim();
    const index = trimmed.indexOf("=");
    if (!trimmed || trimmed.startsWith("#") || index <= 0) {
      return;
    }
    result.set(trimmed.slice(0, index), trimmed.slice(index + 1));
  });
  return result;
}

const parsedBundles = bundles.map((filePath) => {
  const text = fs.readFileSync(filePath, "utf8");
  if (MOJIBAKE_PATTERN.test(text)) {
    fail(`${path.relative(ROOT, filePath)} contains mojibake and must be normalized`);
  }
  return {
    filePath,
    text,
    entries: parseProperties(text)
  };
});

parsedBundles.forEach(({ filePath, text }) => {
  requiredKeys.forEach((key) => {
    if (!new RegExp(`^${key}=`, "m").test(text)) {
      fail(`${path.relative(ROOT, filePath)} is missing key ${key}`);
    }
  });
});

const canonicalBundle = parsedBundles[0];
parsedBundles.slice(1, 2).forEach(({ filePath, entries }) => {
  canonicalBundle.entries.forEach((_value, key) => {
    if (!entries.has(key)) {
      fail(`${path.relative(ROOT, filePath)} is missing parity key ${key}`);
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
