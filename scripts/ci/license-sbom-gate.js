#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const REQUIREMENTS = path.join(ROOT, 'backend/mock_gateway/requirements.txt');
const CATALOG = path.join(ROOT, 'scripts/compliance/dependency-license-catalog.json');
const OUT_JSON = path.join(ROOT, 'docs/artifacts/dependency-sbom.json');
const OUT_MD = path.join(ROOT, 'docs/artifacts/dependency-sbom.md');
const ALLOWED = new Set(['MIT', 'BSD-3-Clause', 'BSD-2-Clause', 'Apache-2.0', 'ISC', 'MPL-2.0', 'PSF-2.0']);

function parseRequirements(text) {
  return String(text || '')
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter((line) => line && !line.startsWith('#'))
    .map((line) => {
      const clean = line.split(';')[0].trim();
      const match = clean.match(/^([A-Za-z0-9_.-]+)/);
      return match ? { name: match[1].toLowerCase(), spec: clean } : null;
    })
    .filter(Boolean);
}

function main() {
  if (!fs.existsSync(REQUIREMENTS)) {
    console.error('license-sbom-gate FAIL: requirements.txt not found');
    process.exit(1);
  }
  if (!fs.existsSync(CATALOG)) {
    console.error('license-sbom-gate FAIL: dependency-license-catalog.json not found');
    process.exit(1);
  }

  const requirements = parseRequirements(fs.readFileSync(REQUIREMENTS, 'utf8'));
  const catalog = JSON.parse(fs.readFileSync(CATALOG, 'utf8'));
  const pyCatalog = (catalog && catalog.python) || {};
  const issues = [];

  const components = requirements.map((dep) => {
    const license = pyCatalog[dep.name] || '';
    if (!license) {
      issues.push(`missing license mapping for python dependency: ${dep.name}`);
    } else if (!ALLOWED.has(license)) {
      issues.push(`disallowed license for ${dep.name}: ${license}`);
    }
    return {
      ecosystem: 'python',
      name: dep.name,
      spec: dep.spec,
      license: license || 'UNKNOWN'
    };
  });

  const sbom = {
    generatedAt: new Date().toISOString(),
    policy: {
      allowedLicenses: Array.from(ALLOWED)
    },
    components,
    issues
  };

  fs.mkdirSync(path.dirname(OUT_JSON), { recursive: true });
  fs.writeFileSync(OUT_JSON, JSON.stringify(sbom, null, 2));

  const md = [
    '# Dependency SBOM and License Policy Report',
    '',
    `- Generated at: ${sbom.generatedAt}`,
    `- Components: ${components.length}`,
    `- Issues: ${issues.length}`,
    '',
    '## Components',
    ...components.map((c) => `- ${c.ecosystem}:${c.name} (${c.spec}) — ${c.license}`),
    '',
    '## Policy',
    `- Allowed licenses: ${Array.from(ALLOWED).join(', ')}`,
    '',
    '## Issues',
    ...(issues.length ? issues.map((i) => `- ${i}`) : ['- none'])
  ].join('\n') + '\n';
  fs.writeFileSync(OUT_MD, md);

  if (issues.length) {
    console.error('license-sbom-gate FAIL');
    issues.forEach((i) => console.error(`- ${i}`));
    process.exit(1);
  }
  console.log('license-sbom-gate PASS');
}

main();
