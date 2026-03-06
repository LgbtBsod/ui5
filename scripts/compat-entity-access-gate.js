#!/usr/bin/env node

const path = require('path');
const { collectFilesByExtensions } = require('./qa-shared');
const { readTextSafe } = require('./lib/auditInput');
const { exitWithColonIssues } = require('./lib/issueGateRuntime');

const root = process.cwd();
const compatSets = ['PersonVHSet', 'ChecklistFlatSet', 'CheckSet', 'BarrierSet', 'MplTreeSet'];
const files = collectFilesByExtensions(root, ['controller', 'service', 'infra', 'view'], ['.js', '.xml']);
const metadataFiles = [
  path.join(root, 'mock_gate_way/services/metadata_builder.py'),
  path.join(root, 'localService/metadata.xml'),
  path.join(root, 'sap/opu/odata/sap/Z_UI5_SRV/$metadata')
];

function buildMetadataText(sources) {
  return sources.map((file) => readTextSafe(file, '')).filter(Boolean).join('\n');
}

function isDeclaredInMetadata(metadataText, entitySet) {
  return metadataText.includes(`Name="${entitySet}"`) || metadataText.includes(`EntitySet Name="${entitySet}"`);
}

function fileCompatIssues(file, text, metadataText) {
  return compatSets.flatMap((entitySet) => {
    if (!text.includes(entitySet) || file.startsWith('infra/adapters/') || isDeclaredInMetadata(metadataText, entitySet)) {
      return [];
    }
    return [`${file}: compat entity ${entitySet} is used outside infra/adapters but is not declared in metadata`];
  });
}

const metadataText = buildMetadataText(metadataFiles);
const issues = files.flatMap((file) => fileCompatIssues(file, readTextSafe(path.join(root, file), ''), metadataText));
const stats = { filesScanned: files.length, metadataSources: metadataFiles.length };
exitWithColonIssues('compat-entity-access-gate', issues, stats, { asJson: process.argv.includes('--json') });
