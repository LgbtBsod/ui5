#!/usr/bin/env node
const fs = require('fs');

const cssPath = 'css/style.css';
if (!fs.existsSync(cssPath)) {
  console.error('Semantic contrast gate failed: css/style.css not found.');
  process.exit(1);
}

const css = fs.readFileSync(cssPath, 'utf8');
const requiredVars = ['--sapPositive', '--sapNegative', '--sapCritical', '--sapInformation'];
const missing = requiredVars.filter((v) => !css.includes(v));
if (missing.length) {
  console.error(`Semantic contrast gate failed: missing semantic vars: ${missing.join(', ')}`);
  process.exit(1);
}

if (!css.includes(':root.light-mode') || !css.includes('body.appDark')) {
  console.error('Semantic contrast gate failed: missing Morning/Night theme scopes.');
  process.exit(1);
}

console.log('Semantic contrast gate passed: semantic variables and dual theme scopes detected.');
