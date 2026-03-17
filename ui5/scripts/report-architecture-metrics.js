#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { countFileLines } = require('./qa-shared');

const targets = [
  'controller/Search.controller.js',
  'controller/Detail.controller.js'
];

function countJsRecursive(dir) {
  const root = path.resolve(process.cwd(), dir);
  if (!fs.existsSync(root)) return 0;
  let count = 0;
  const stack = [root];
  while (stack.length) {
    const cur = stack.pop();
    const entries = fs.readdirSync(cur, { withFileTypes: true });
    entries.forEach((e) => {
      const full = path.join(cur, e.name);
      if (e.isDirectory()) stack.push(full);
      else if (e.isFile() && e.name.endsWith('.js')) count += 1;
    });
  }
  return count;
}

const report = {
  generatedAt: new Date().toISOString(),
  controllers: targets.map((p) => ({ path: p, lines: countFileLines(process.cwd(), p) })),
  domainUsecaseCount: countJsRecursive('service/domain'),
  utilCount: countJsRecursive('util')
};

console.log(JSON.stringify(report, null, 2));
