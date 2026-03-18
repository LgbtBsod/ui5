#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const DOMAIN_ROOT = path.join(ROOT, 'app', 'service', 'domain');
const CRITICAL = [
  '/detailSkeletonBusy',
  '/attachmentsLoaded',
  '/sessionAttachments',
  '/validationShown',
  '/validationMissing',
  '/accessState'
];

function walk(dir) {
  return fs.readdirSync(dir).flatMap((name) => {
    const p = path.join(dir, name);
    const st = fs.statSync(p);
    if (st.isDirectory()) return walk(p);
    return p.endsWith('.js') ? [p] : [];
  });
}

function main() {
  const files = walk(DOMAIN_ROOT);
  const issues = [];

  files.forEach((file) => {
    const rel = path.relative(ROOT, file).replace(/\\/g, '/');
    if (rel.endsWith('/ViewPathContracts.js')) return;
    const text = fs.readFileSync(file, 'utf8');
    const usesViewContract = text.includes('ViewPathContracts');

    CRITICAL.forEach((literal) => {
      if (!text.includes(literal)) return;
      if (usesViewContract) {
        // allow transitional mixed files only if literal not used in modelPatch/get("view")
        const re = new RegExp(`(modelPatch\\(\\"view\\",\\s*\\"${literal.replace('/', '\\/')}\\"|get\\(\\"view\\",\\s*\\"${literal.replace('/', '\\/')}\\")`);
        if (!re.test(text)) return;
      }
      issues.push(`${rel} uses critical view literal path: ${literal}`);
    });
  });

  if (issues.length) {
    console.error('domain-viewpath-contract-gate FAIL');
    issues.forEach((i) => console.error(i));
    process.exit(1);
  }
  console.log('domain-viewpath-contract-gate PASS');
}

main();
