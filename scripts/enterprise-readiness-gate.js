#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { countFileLines, detectRuntimeRoot } = require('./qa-shared');
const { requireJsonReport } = require('./lib/reportGateRuntime');
const { exitWithMappedIssues } = require('./lib/gate-result');

const thresholdsPath = process.argv[2] || 'scripts/enterprise-readiness-thresholds.json';
const runtimeRoot = detectRuntimeRoot(process.cwd());

function collectMetrics() {
  const controllerPaths = [
    'controller/Search.controller.js',
    'controller/Detail.controller.js'
  ];
  const domainDir = path.join(process.cwd(), runtimeRoot, 'service/domain');
  const utilDir = path.join(process.cwd(), runtimeRoot, 'util');

  return {
    controllers: controllerPaths.map((path) => ({ path, lines: countFileLines(process.cwd(), path) })),
    usecaseCount: fs.existsSync(domainDir) ? fs.readdirSync(domainDir, { withFileTypes: true }).filter((f) => f.isDirectory()).length : 0,
    utilCount: fs.existsSync(utilDir) ? fs.readdirSync(utilDir).filter((f) => f.endsWith('.js')).length : 0
  };
}

function main() {
  const thresholds = requireJsonReport(thresholdsPath, {
    prefix: 'Enterprise thresholds gate failed',
    missingExitCode: 2,
    invalidExitCode: 2
  });
  const metrics = collectMetrics();
  const violations = [];

  metrics.controllers.forEach((controller) => {
    const limit = thresholds.maxControllerLines && thresholds.maxControllerLines[controller.path];
    if (typeof limit === 'number' && controller.lines > limit) {
      violations.push(`${controller.path}: ${controller.lines} lines > ${limit}`);
    }
  });

  if (typeof thresholds.maxUsecaseCount === 'number' && metrics.usecaseCount > thresholds.maxUsecaseCount) {
    violations.push(`service/domain feature count: ${metrics.usecaseCount} > ${thresholds.maxUsecaseCount}`);
  }

  if (typeof thresholds.maxUtilCount === 'number' && metrics.utilCount > thresholds.maxUtilCount) {
    violations.push(`util count: ${metrics.utilCount} > ${thresholds.maxUtilCount}`);
  }

  if (violations.length > 0) {
    exitWithMappedIssues(
      'enterprise-readiness-gate',
      violations,
      (line) => ({ file: String(line).split(':')[0], message: String(line) }),
      { controllers: metrics.controllers.length },
      { asJson: process.argv.includes('--json') }
    );
  }

  exitWithMappedIssues('enterprise-readiness-gate', [], function (item) { return item; }, metrics, { asJson: process.argv.includes('--json') });
}

main();
