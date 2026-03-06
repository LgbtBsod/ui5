#!/usr/bin/env node

const { missingPaths, readText, fail } = require('./lib/gateFs');

const requiredDocs = ['docs/DEVELOPMENT_PLAN.md', 'css/style.css', 'view/fragment/DetailControlRail.fragment.xml'];
const missing = missingPaths(requiredDocs);
if (missing.length) fail(`C2 gate failed: missing required artifacts: ${missing.join(', ')}`);

const plan = readText('docs/DEVELOPMENT_PLAN.md');
if (!plan.includes('reduced-motion') || !plan.includes('contrast-safe')) {
  fail('C2 gate failed: reduced-motion / contrast-safe requirements missing in DEVELOPMENT_PLAN.');
}

const css = readText('css/style.css');
if (!css.includes('prefers-reduced-motion: reduce')) fail('C2 gate failed: reduced-motion policy not found in css/style.css');

const rail = readText('view/fragment/DetailControlRail.fragment.xml');
const iconButtons = (rail.match(/<Button[^>]*icon="/g) || []).length;
const tooltipAttrs = (rail.match(/tooltip="/g) || []).length;
if (tooltipAttrs < iconButtons) {
  fail(`C2 gate failed: icon buttons (${iconButtons}) exceed tooltip attributes (${tooltipAttrs}) in DetailControlRail.`);
}

console.log('C2 gate passed: reduced-motion and tooltip accessibility baseline checks passed.');
