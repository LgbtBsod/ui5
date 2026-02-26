#!/usr/bin/env node
const fs = require('fs');

const requiredDocs = [
  'docs/ux/accessibility-baseline.md',
  'css/style.css',
  'view/fragment/DetailControlRail.fragment.xml'
];
const missing = requiredDocs.filter((p) => !fs.existsSync(p));
if (missing.length) {
  console.error(`C2 gate failed: missing required artifacts: ${missing.join(', ')}`);
  process.exit(1);
}

const css = fs.readFileSync('css/style.css', 'utf8');
if (!css.includes('prefers-reduced-motion: reduce')) {
  console.error('C2 gate failed: reduced-motion policy not found in css/style.css');
  process.exit(1);
}
const rail = fs.readFileSync('view/fragment/DetailControlRail.fragment.xml', 'utf8');
const iconButtons = (rail.match(/<Button[^>]*icon="/g) || []).length;
const tooltipAttrs = (rail.match(/tooltip="/g) || []).length;
if (tooltipAttrs < iconButtons) {
  console.error(`C2 gate failed: icon buttons (${iconButtons}) exceed tooltip attributes (${tooltipAttrs}) in DetailControlRail.`);
  process.exit(1);
}
console.log('C2 gate passed: reduced-motion and tooltip accessibility baseline checks passed.');
