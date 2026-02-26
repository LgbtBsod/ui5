#!/usr/bin/env node
const fs = require('fs');

const thresholdsPath = process.argv[2] || 'scripts/enterprise-readiness-thresholds.json';

function readJson(path) {
  return JSON.parse(fs.readFileSync(path, 'utf8'));
}

function lineCount(path) {
  const content = fs.readFileSync(path, 'utf8');
  return content.split(/\r?\n/).length;
}

function collectMetrics() {
  const controllerPaths = [
    'controller/Search.controller.js',
    'controller/Detail.controller.js'
  ];

  return {
    controllers: controllerPaths.map((path) => ({ path, lines: lineCount(path) })),
    usecaseCount: fs.readdirSync('service/usecase').filter((f) => f.endsWith('.js')).length,
    utilCount: fs.readdirSync('util').filter((f) => f.endsWith('.js')).length
  };
}

function main() {
  if (!fs.existsSync(thresholdsPath)) {
    console.error(`Enterprise thresholds file not found: ${thresholdsPath}`);
    process.exit(2);
  }

  const thresholds = readJson(thresholdsPath);
  const metrics = collectMetrics();
  const violations = [];

  metrics.controllers.forEach((controller) => {
    const limit = thresholds.maxControllerLines && thresholds.maxControllerLines[controller.path];
    if (typeof limit === 'number' && controller.lines > limit) {
      violations.push(`${controller.path}: ${controller.lines} lines > ${limit}`);
    }
  });

  if (typeof thresholds.maxUsecaseCount === 'number' && metrics.usecaseCount > thresholds.maxUsecaseCount) {
    violations.push(`service/usecase count: ${metrics.usecaseCount} > ${thresholds.maxUsecaseCount}`);
  }

  if (typeof thresholds.maxUtilCount === 'number' && metrics.utilCount > thresholds.maxUtilCount) {
    violations.push(`util count: ${metrics.utilCount} > ${thresholds.maxUtilCount}`);
  }

  if (violations.length > 0) {
    console.error('Enterprise readiness gate failed.');
    violations.forEach((message) => console.error(`- ${message}`));
    process.exit(1);
  }

  console.log('Enterprise readiness gate passed.');
  console.log(JSON.stringify(metrics, null, 2));
}

main();
