#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

function readFile(file) {
  try {
    return fs.readFileSync(file, 'utf8');
  } catch (_e) {
    return '';
  }
}

function collectJsFiles(dir, out) {
  if (!fs.existsSync(dir)) return out;
  const entries = fs.readdirSync(dir, { withFileTypes: true });
  for (const entry of entries) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) collectJsFiles(full, out);
    else if (entry.isFile() && entry.name.endsWith('.js')) out.push(full);
  }
  return out;
}

function analyzePatterns(rootDir) {
  const files = collectJsFiles(path.join(rootDir, 'controller'), []);
  const approved = [
    'controller → facade → usecase',
    'usecase → port → adapter',
    'usecase → port → adapter → state mutation'
  ];
  const deprecated = ['controller → adapter'];
  const experimental = ['court precedent override with timed expiry'];

  let controllerFacadeCalls = 0;
  let directDomainCalls = 0;
  let adapterDirectCalls = 0;

  for (const file of files) {
    const src = readFile(file);
    if (/executeFacadeMethod\(/.test(src) || /Facade\./.test(src)) controllerFacadeCalls += 1;
    if (/service\/domain\//.test(src)) directDomainCalls += 1;
    if (/infra\/adapters\//.test(src) || /Adapter\./.test(src)) adapterDirectCalls += 1;
  }

  const stablePatterns = [
    {
      pattern: 'usecase → port → adapter → state mutation',
      stability: 98,
      evidence: ['domain modules use ports for infra access', 'state mutation centralized in usecase effects']
    },
    {
      pattern: 'controller → facade → usecase',
      stability: Math.max(70, Math.min(99, 80 + controllerFacadeCalls * 2)),
      evidence: [`controller facade usage hits: ${controllerFacadeCalls}`]
    }
  ];

  const antiPatterns = [
    {
      pattern: 'controller calling domain helper directly',
      occurrences: directDomainCalls,
      severity: directDomainCalls > 10 ? 'high' : directDomainCalls > 3 ? 'medium' : 'low'
    },
    {
      pattern: 'controller → adapter',
      occurrences: adapterDirectCalls,
      severity: adapterDirectCalls > 0 ? 'medium' : 'low'
    }
  ];

  const patternLibrary = {
    approved,
    deprecated,
    experimental,
    stablePatterns,
    antiPatterns,
    updatedAt: new Date().toISOString()
  };

  const libraryFile = path.join(rootDir, 'udos', 'evolution', 'models', 'pattern-library.json');
  fs.writeFileSync(libraryFile, JSON.stringify(patternLibrary, null, 2));

  return patternLibrary;
}

module.exports = { analyzePatterns };
