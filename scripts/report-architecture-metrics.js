#!/usr/bin/env node
const fs = require('fs');

const targets = [
  'controller/Search.controller.js',
  'controller/Detail.controller.js'
];

function lineCount(path) {
  const content = fs.readFileSync(path, 'utf8');
  return content.split(/\r?\n/).length;
}

const report = {
  generatedAt: new Date().toISOString(),
  controllers: targets.map((path) => ({ path, lines: lineCount(path) })),
  usecaseCount: fs.readdirSync('service/usecase').filter((f) => f.endsWith('.js')).length,
  utilCount: fs.readdirSync('util').filter((f) => f.endsWith('.js')).length
};

console.log(JSON.stringify(report, null, 2));
