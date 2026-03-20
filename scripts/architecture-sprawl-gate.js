#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const SEARCH_DIR = path.join(ROOT, 'app/controller/search');
const DETAIL_DIR = path.join(ROOT, 'app/controller/detail');
const FRAMEWORK_DIR = path.join(ROOT, 'app/service/framework');
const SEARCH_SOFT = 16;
const DETAIL_SOFT = 24;
const FRAMEWORK_SOFT = 90;

function countJsFiles(dir) {
  if (!fs.existsSync(dir)) {
    return 0;
  }
  return fs.readdirSync(dir, { withFileTypes: true })
    .filter((entry) => entry.isFile() && entry.name.endsWith('.js'))
    .length;
}

function warnIfOver(issues, label, count, threshold) {
  if (count > threshold) {
    issues.push(`${label} module count ${count} > ${threshold}`);
  }
}

const searchCount = countJsFiles(SEARCH_DIR);
const detailCount = countJsFiles(DETAIL_DIR);
const frameworkCount = countJsFiles(FRAMEWORK_DIR);
const issues = [];

warnIfOver(issues, 'search controller/runtime', searchCount, SEARCH_SOFT);
warnIfOver(issues, 'detail controller/runtime', detailCount, DETAIL_SOFT);
warnIfOver(issues, 'framework runtime', frameworkCount, FRAMEWORK_SOFT);

if (issues.length) {
  console.log('WARN architecture-sprawl-gate');
  issues.forEach((issue) => console.log(`- ${issue}`));
  process.exit(0);
}

console.log('PASS architecture-sprawl-gate');
