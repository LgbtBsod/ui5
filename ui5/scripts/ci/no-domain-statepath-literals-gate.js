#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const domainRoot = path.join(ROOT, 'app', 'service', 'domain');
const CONTRACT_MODULES = new Set([
  path.join(domainRoot, 'shared', 'ModelPathContracts.js'),
  path.join(domainRoot, 'shared', 'DomainStatePaths.js')
]);

function walk(dir) {
  return fs.readdirSync(dir).flatMap((name) => {
    const p = path.join(dir, name);
    const st = fs.statSync(p);
    if (st.isDirectory()) return walk(p);
    return p.endsWith('.js') ? [p] : [];
  });
}

function main() {
  const files = walk(domainRoot);
  const issues = [];
  const pattern = /(?:get\("state",\s*"([^"]+)"\)|modelPatch\("state",\s*"([^"]+)"\))/g;

  files.forEach((file) => {
    if (CONTRACT_MODULES.has(file)) return;
    const rel = path.relative(ROOT, file).replace(/\\/g, '/');
    const lines = fs.readFileSync(file, 'utf8').split(/\r?\n/);
    lines.forEach((line, idx) => {
      let match;
      while ((match = pattern.exec(line)) !== null) {
        const pathLiteral = (match[1] || match[2] || '').trim();
        if (!pathLiteral.startsWith('/')) continue;
        issues.push(`${rel}:${idx + 1} - literal state path is forbidden in domain usecases: ${pathLiteral}`);
      }
      pattern.lastIndex = 0;
    });
  });

  if (issues.length) {
    console.error('no-domain-statepath-literals-gate FAIL');
    issues.forEach((issue) => console.error(issue));
    process.exit(1);
  }

  console.log('no-domain-statepath-literals-gate PASS');
}

main();
