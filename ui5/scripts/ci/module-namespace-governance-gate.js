#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const APP_ROOT = path.join(ROOT, 'app');
const ALLOWED_PREFIXES = ['PRODUCTION_CONTROL_CHECKLIST/', 'sap/', './', '../'];

function walk(dir) {
  return fs.readdirSync(dir).flatMap((name) => {
    const p = path.join(dir, name);
    const st = fs.statSync(p);
    if (st.isDirectory()) return walk(p);
    return p.endsWith('.js') ? [p] : [];
  });
}

function main() {
  const files = walk(APP_ROOT);
  const issues = [];
  const depRe = /["']([^"']+)["']/g;

  files.forEach((file) => {
    const rel = path.relative(ROOT, file).replace(/\\/g, '/');
    const text = fs.readFileSync(file, 'utf8');
    const defineMatch = text.match(/sap\.ui\.define\s*\(\s*\[([\s\S]*?)\]\s*,/);
    if (!defineMatch) return;
    const block = defineMatch[1];
    let m;
    while ((m = depRe.exec(block)) !== null) {
      const dep = String(m[1] || '');
      if (!dep) continue;
      if (ALLOWED_PREFIXES.some((pfx) => dep.startsWith(pfx))) continue;
      issues.push(`${rel}: invalid dependency namespace '${dep}'`);
    }
  });

  if (issues.length) {
    console.error('module-namespace-governance-gate FAIL');
    issues.forEach((i) => console.error(i));
    process.exit(1);
  }

  console.log('module-namespace-governance-gate PASS');
}

main();
