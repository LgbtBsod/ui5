#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const APP_ROOT = path.join(ROOT, 'app');
const TARGET_DIRS = ['controller', 'service', 'infra', 'util', 'model', 'Component.js'];
const FORBIDDEN = [
  { id: 'fetch', re: /\bfetch\s*\(/g, msg: 'fetch() bypasses OData gateway contract' },
  { id: 'xhr', re: /\bnew\s+XMLHttpRequest\s*\(/g, msg: 'XMLHttpRequest bypasses OData gateway contract' },
  { id: 'axios', re: /\baxios\b/g, msg: 'axios bypasses OData gateway contract' },
  { id: 'jqueryAjax', re: /\bjQuery\.ajax\s*\(/g, msg: 'jQuery.ajax bypasses OData gateway contract' }
];

function walk(dir) {
  return fs.readdirSync(dir).flatMap((name) => {
    const p = path.join(dir, name);
    const st = fs.statSync(p);
    if (st.isDirectory()) return walk(p);
    return p.endsWith('.js') ? [p] : [];
  });
}

function listFiles() {
  const files = [];
  TARGET_DIRS.forEach((entry) => {
    const p = path.join(APP_ROOT, entry);
    if (!fs.existsSync(p)) return;
    const st = fs.statSync(p);
    if (st.isDirectory()) files.push(...walk(p));
    else files.push(p);
  });
  return files;
}

function main() {
  const files = listFiles();
  const issues = [];

  files.forEach((file) => {
    const rel = path.relative(ROOT, file).replace(/\\/g, '/');
    const text = fs.readFileSync(file, 'utf8');
    FORBIDDEN.forEach((rule) => {
      let m;
      while ((m = rule.re.exec(text)) !== null) {
        const line = text.slice(0, m.index).split(/\r?\n/).length;
        issues.push(`${rel}:${line} ${rule.msg}`);
      }
      rule.re.lastIndex = 0;
    });
  });

  if (issues.length) {
    console.error('no-rest-bypass-gate FAIL');
    issues.forEach((i) => console.error(i));
    process.exit(1);
  }
  console.log('no-rest-bypass-gate PASS');
}

main();
