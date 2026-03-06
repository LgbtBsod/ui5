#!/usr/bin/env node
const fs = require('fs');

const cssPath = process.argv[2] || 'css/style.css';

function fail(message) {
  console.error(message);
  process.exit(1);
}

if (!fs.existsSync(cssPath)) {
  fail(`CSS accent governance gate: file not found: ${cssPath}`);
}

const lines = fs.readFileSync(cssPath, 'utf8').split(/\r?\n/);

const tokenSectionStart = lines.findIndex((line) => line.includes('Section: tokens-and-theme-modes'));
const bodySectionStart = lines.findIndex((line) => line.includes('Section: layout-shell-and-page-surfaces'));

if (tokenSectionStart < 0 || bodySectionStart < 0 || bodySectionStart <= tokenSectionStart) {
  fail('CSS accent governance gate: expected token/layout section markers were not found.');
}

const scopeStart = bodySectionStart + 1;
const scopedLines = lines.slice(scopeStart);

const hardcodedAccentCandidates = [];
const accentHex = /#(?:0070f2|91c8f6|4b7bff|5aa7ff|729cff|8bc2ff|00acee|37d2ff)\b/i;
const accentRgb = /rgba?\(\s*(?:0\s*,\s*112\s*,\s*242|145\s*,\s*200\s*,\s*246|0\s*,\s*172\s*,\s*238)\b/i;
const hasColorProp = /(color|background|border|shadow|stroke|fill)/i;

scopedLines.forEach((line, idx) => {
  const lineNo = scopeStart + idx + 1;
  const trimmed = line.trim();
  if (!trimmed || trimmed.startsWith('/*')) {
    return;
  }
  if (!hasColorProp.test(trimmed)) {
    return;
  }
  if (trimmed.includes('var(--')) {
    return;
  }
  if (accentHex.test(trimmed) || accentRgb.test(trimmed)) {
    hardcodedAccentCandidates.push({ lineNo, text: trimmed });
  }
});

if (hardcodedAccentCandidates.length > 0) {
  console.error('CSS accent governance gate failed: hardcoded accent-like colors found outside token section.');
  hardcodedAccentCandidates.forEach((item) => {
    console.error(`- line ${item.lineNo}: ${item.text}`);
  });
  process.exit(1);
}

console.log('CSS accent governance gate passed: no hardcoded accent-like colors outside token section.');
