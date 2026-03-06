#!/usr/bin/env node
const { spawnSync } = require('child_process');
const fs = require('fs');

const commands = [
  ['node', ['scripts/css-accent-governance-gate.js']],
  ['node', ['scripts/a11y-gate.js']],
  ['node', ['scripts/message-taxonomy-gate.js']],
  ['node', ['scripts/ux-state-coverage-gate.js']],
  ['node', ['scripts/theme-parity-gate.js']],
  ['node', ['scripts/pair-snapshots-gate.js']],
  ['node', ['scripts/control-token-mapping-gate.js']],
  ['node', ['scripts/theme-preferences-contract-gate.js']],
  ['node', ['scripts/semantic-contrast-gate.js']],
  ['node', ['scripts/unit-smoke.js']],
  ['node', ['scripts/ci-smoke-gate.js']],
  ['bash', ['scripts/ci/enterprise-ux-gate.sh']],
  ['node', ['scripts/full-style-migration-report.js']]
];

const results = [];
for (const [cmd, args] of commands) {
  const proc = spawnSync(cmd, args, { encoding: 'utf8' });
  const entry = {
    command: `${cmd} ${args.join(' ')}`,
    exitCode: proc.status,
    status: proc.status === 0 ? 'pass' : 'fail',
    stdout: (proc.stdout || '').trim(),
    stderr: (proc.stderr || '').trim()
  };
  results.push(entry);
  if (proc.status !== 0) {
    console.error(`FAILED: ${entry.command}`);
    if (entry.stderr) console.error(entry.stderr);
    break;
  }
}

const allPassed = results.every((r) => r.status === 'pass') && results.length === commands.length;
const cert = {
  generatedAt: new Date().toISOString(),
  ui5Baseline: '1.71.28',
  migrationStatus: allPassed ? 'completed' : 'incomplete',
  doneCriteria: {
    hardcodedAccent: allPassed,
    criticalA11yZero: allPassed,
    parity100: allPassed,
    smartFallbackStable: allPassed,
    allGatesGreen: allPassed,
    rollbackAttached: fs.existsSync('docs/artifacts/rollback-playbook-template.md')
  },
  results
};

fs.writeFileSync('docs/artifacts/final-style-migration-certificate.json', JSON.stringify(cert, null, 2) + '\n');
console.log(`Final certificate written: docs/artifacts/final-style-migration-certificate.json`);
if (!allPassed) {
  process.exit(1);
}
