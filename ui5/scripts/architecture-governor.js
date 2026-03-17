#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { readJsonSafe } = require('./lib/auditInput');
const { writeTextFile } = require('./lib/reportFiles');
const { FEATURE_REGISTRY, featureOfFile } = require('./lib/domainFeatureRegistry');
const {
  DOCS,
  ROOT,
  listRuntimeJsFiles,
  parseImports,
  readAbsolute,
  writeDoc,
  writeDocJson
} = require('./lib/architectureArtifactsShared');

function read(file) {
  return readAbsolute(file);
}

function countLargeFunctions(text, maxLines = 50) {
  const lines = text.split('\n');
  const findings = [];
  let start = -1;
  let signature = '';
  for (let i = 0; i < lines.length; i++) {
    const l = lines[i];
    if (start < 0 && /(function\s+[A-Za-z0-9_]+\s*\(|=>\s*\{|^[ \t]*[A-Za-z0-9_]+\s*:\s*function\s*\()/.test(l)) {
      start = i;
      signature = l.trim().slice(0, 120);
      continue;
    }
    if (start >= 0 && l.includes('}')) {
      const span = i - start + 1;
      if (span > maxLines) findings.push({ line: start + 1, lines: span, signature });
      start = -1;
      signature = '';
    }
  }
  return findings;
}

function nowIso() {
  return new Date().toISOString();
}

function main() {
  const files = listRuntimeJsFiles();
  const controllers = files.filter((f) => f.startsWith('controller/'));
  const facades = files.filter((f) => f.endsWith('Facade.js'));
  const usecases = files.filter((f) => f.includes('/usecases/'));
  const ports = files.filter((f) => f.includes('/ports/'));
  const adapters = files.filter((f) => f.includes('/adapters/') || f.includes('service/backend/'));
  const managers = files.filter((f) => f.startsWith('service/runtime/'));
  const utilities = files.filter((f) => f.includes('/util/') || f.includes('service/framework/'));

  const importGraph = {};
  const layerViolations = [];
  const duplicateClusters = [];
  const fingerprintMap = new Map();
  const largeFunctions = [];
  const oversizedFiles = [];

  for (const file of files) {
    const text = read(path.join(ROOT, file));
    const imports = parseImports(text);
    importGraph[file] = imports;

    if (file.startsWith('controller/')) {
      const bad = imports.filter((i) => /service\/backend|adapters|infra\//.test(i));
      bad.forEach((imp) => layerViolations.push({ file, rule: 'controller-forbidden-import', import: imp }));
    }

    const lineCount = text.split('\n').length;
    if (lineCount > 400) oversizedFiles.push({ file, lines: lineCount });

    countLargeFunctions(text).forEach((f) => largeFunctions.push({ file, ...f }));

    const clean = text.split('\n').map((l) => l.trim()).filter((l) => l.length > 20);
    for (let i = 0; i + 2 < clean.length; i++) {
      const fp = `${clean[i]}|${clean[i + 1]}|${clean[i + 2]}`;
      if (fp.length < 120) continue;
      if (!fingerprintMap.has(fp)) fingerprintMap.set(fp, new Set());
      fingerprintMap.get(fp).add(file);
    }
  }

  [...fingerprintMap.entries()].forEach(([fp, set]) => {
    if (set.size > 1) duplicateClusters.push({ files: [...set], fingerprint: fp.slice(0, 180) });
  });

  const featureRegistry = FEATURE_REGISTRY;

  const workflowRegistry = {
    searchWorkflow: {
      steps: ['init search facade', 'execute search usecase', 'bind search results', 'select row state transition'],
      expectedStateTransitions: ['results:IDLE->LOADED', 'selection:NONE->ACTIVE']
    },
    detailOpenWorkflow: {
      steps: ['select search row', 'fetch detail via facade', 'validate cache LastChangeSet', 'hydrate detail model'],
      expectedStateTransitions: ['detail:CLOSED->OPEN', 'detailHydration:PENDING->READY']
    },
    editWorkflow: {
      steps: ['enter edit intent', 'tryAcquireLock', 'set editMode=EDIT on success', 'activate autosave when dirty'],
      expectedStateTransitions: ['editMode:READ->EDIT', 'lockState:IDLE->LOCKED']
    },
    lockWorkflow: {
      steps: ['acquire lock', 'monitor lock heartbeat', 'handle killed/lockLost to READ'],
      expectedStateTransitions: ['lockState:IDLE->LOCKED', 'lockLost:LOCKED->FAILED']
    },
    autosaveWorkflow: {
      steps: ['dirty=true in EDIT', 'autosave ACTIVE when LOCKED', 'stop autosave on lock lost'],
      expectedStateTransitions: ['autosave:IDLE->ACTIVE', 'lockLost:ACTIVE->IDLE']
    },
    cacheValidationWorkflow: {
      steps: ['read IndexedDB cache', 'compare AggChangedOn with server stamp', 'accept if abs(diff)<=5500ms'],
      expectedStateTransitions: ['cacheValidation:PENDING->VALID|INVALID']
    }
  };

  const featureOwners = {};
  Object.keys(featureRegistry).forEach((key) => { featureOwners[key] = []; });
  files.forEach((f) => featureOwners[featureOfFile(f)].push(f));

  const repositoryMapMd = [
    '# Repository Map',
    '',
    `- controllers: ${controllers.length}`,
    `- facades: ${facades.length}`,
    `- usecases: ${usecases.length}`,
    `- ports: ${ports.length}`,
    `- adapters: ${adapters.length}`,
    `- managers: ${managers.length}`,
    `- utilities: ${utilities.length}`,
    '',
    '## Key domain modules',
    ...Object.entries(featureRegistry).map(([k, v]) => `- ${k}: ${v}`)
  ].join('\n');

  const workflowMapMd = [
    '# Workflow Map',
    '',
    ...Object.entries(workflowRegistry).flatMap(([name, config]) => [
      `## ${name}`,
      ...config.steps.map((s, i) => `${i + 1}. ${s}`),
      '- expected transitions:',
      ...config.expectedStateTransitions.map((t) => `  - ${t}`),
      ''
    ])
  ].join('\n');

  const featureMapMd = [
    '# Feature Map',
    '',
    ...Object.keys(featureRegistry).flatMap((feature) => [
      `## ${feature}`,
      `- owner: ${featureRegistry[feature]}`,
      ...(featureOwners[feature].slice(0, 20).map((file) => `- ${file}`) || ['- (none)']),
      ''
    ])
  ].join('\n');

  const duplicateLogicMd = [
    '# Duplicate Logic Detection',
    '',
    `Detected duplicate clusters: ${duplicateClusters.length}`,
    '',
    ...duplicateClusters.slice(0, 30).map((c, i) => `${i + 1}. ${c.files.join(', ')}\n   - ${c.fingerprint}...`)
  ].join('\n');

  const largeFunctionsMd = [
    '# Large Functions',
    '',
    `Functions over 50 lines: ${largeFunctions.length}`,
    '',
    ...largeFunctions.slice(0, 50).map((f) => `- ${f.file}:${f.line} (${f.lines} lines) ${f.signature}`)
  ].join('\n');

  const domainBoundariesMd = [
    '# Domain Boundaries',
    '',
    `Layer violations found: ${layerViolations.length}`,
    '',
    ...layerViolations.map((v) => `- ${v.file} imports ${v.import} (${v.rule})`),
    '',
    'Expected direction: controllers -> facades -> usecases -> ports -> adapters -> backend.'
  ].join('\n');

  const legacySurfaceMd = [
    '# Legacy Surface Map',
    '',
    `Oversized files (>400 lines): ${oversizedFiles.length}`,
    ...oversizedFiles.slice(0, 50).map((f) => `- ${f.file}: ${f.lines}`)
  ].join('\n');

  const duplicatePenalty = Math.min(4, duplicateClusters.length * 0.05);
  const architectureHealthScore = Math.max(
    0,
    100 - layerViolations.length * 3 - duplicatePenalty - largeFunctions.length * 0.5 - oversizedFiles.length * 0.5
  );

  const architectureHealthMd = [
    '# Architecture Health',
    '',
    `- score: ${architectureHealthScore.toFixed(1)}`,
    `- layer violations: ${layerViolations.length}`,
    `- duplicate logic clusters: ${duplicateClusters.length}`,
    `- large functions (>50 lines): ${largeFunctions.length}`,
    `- oversized files (>400 lines): ${oversizedFiles.length}`,
    '',
    architectureHealthScore >= 92 ? 'Status: PASS (>=92).' : 'Status: NEEDS IMPROVEMENT (<92).'
  ].join('\n');

  const memory = {
    timestamp: nowIso(),
    layers: { controllers, facades, usecases, ports, adapters, managers, utilities },
    featureOwnership: featureRegistry,
    workflows: workflowRegistry,
    duplicateLogicClusters: duplicateClusters.slice(0, 100),
    domainBoundaries: { layerViolations },
    architectureHealth: {
      score: Number(architectureHealthScore.toFixed(1)),
      layerViolations: layerViolations.length,
      duplicateLogic: duplicateClusters.length,
      largeFunctions: largeFunctions.length,
      oversizedFiles: oversizedFiles.length
    }
  };

  const context = { timestamp: nowIso(), controllers, facades, usecases, ports, adapters, managers, utilities };

  const historyEntry = [
    `## ${nowIso()}`,
    '',
    '- files changed: docs/repository-map.md, docs/workflow-map.md, docs/workflow-registry.json, docs/duplicate-logic.md, docs/legacy-surface-map.md, docs/large-functions.md, docs/domain-boundaries.md, docs/architecture-health.md, docs/repository-memory.json, docs/architecture-context.json, docs/feature-registry.json',
    '- reason: Run autonomous architecture governor scan and refresh repository memory artifacts.',
    '- architecture impact: Updated dependency/layer/workflow observability and architecture health score reporting.',
    ''
  ].join('\n');

  const refactorHistoryPath = path.join(DOCS, 'refactor-history.md');
  const priorHistory = fs.existsSync(refactorHistoryPath) ? read(refactorHistoryPath) : '# Refactor History\n\n';

  writeDoc('repository-map.md', repositoryMapMd + '\n');
  writeDoc('workflow-map.md', workflowMapMd + '\n');
  writeDocJson('dependency-graph.json', importGraph);
  writeDoc('feature-map.md', featureMapMd + '\n');
  writeDocJson('workflow-registry.json', workflowRegistry);
  writeDoc('duplicate-logic.md', duplicateLogicMd + '\n');
  writeDoc('legacy-surface-map.md', legacySurfaceMd + '\n');
  writeDoc('large-functions.md', largeFunctionsMd + '\n');
  writeDoc('domain-boundaries.md', domainBoundariesMd + '\n');
  writeDoc('architecture-health.md', architectureHealthMd + '\n');
  writeDocJson('repository-memory.json', memory);
  writeDocJson('architecture-context.json', context);
  writeDocJson('feature-registry.json', featureRegistry);
  const qaLatest = readJsonSafe(path.join(DOCS, 'qa-report-latest.json'), {});
  const qaReportMd = [
    '# QA Report Latest',
    '',
    `- generatedAt: ${nowIso()}`,
    `- architectureHealthScore: ${architectureHealthScore.toFixed(1)}`,
    `- qaResultSource: docs/qa-report-latest.json`,
    '',
    '```json',
    JSON.stringify(qaLatest, null, 2),
    '```'
  ].join('\n');
  writeDoc('qa-report-latest.md', qaReportMd + '\n');
  writeTextFile(refactorHistoryPath, priorHistory + historyEntry);

  console.log(`Architecture governance artifacts generated. Score=${architectureHealthScore.toFixed(1)}`);
}

main();
