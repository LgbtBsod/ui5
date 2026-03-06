#!/usr/bin/env node
const fs = require('fs');
const { execSync } = require('child_process');
const { getChangedFiles } = require('../../scripts/lib/git-changes');
const { readJsonSafe } = require('../../scripts/lib/auditInput');

function parseDiffText(raw) {
  const files = [];
  const addedImports = [];
  const touchedStatePaths = new Set();
  let current = null;

  for (const line of String(raw || '').split('\n')) {
    if (line.startsWith('diff --git')) {
      const m = line.match(/ b\/(.+)$/);
      current = m ? m[1] : null;
      if (current) files.push(current);
      continue;
    }
    if (line.startsWith('+') && !line.startsWith('+++')) {
      const l = line.slice(1);
      const imp = l.match(/"(sap_ui5\/[^"]+)"|'(sap_ui5\/[^']+)'/);
      if (imp && current) addedImports.push({ from: current, to: imp[1] || imp[2] });

      const runtimeStateOwner = /^(controller|service|infra|manager|util|model|view)\//.test(String(current || ""));
      if (runtimeStateOwner) {
        const statePathRegex = /\/(mode|lockOperationState|autosaveEnabled|activeObjectId|selectedId|search[^"']*)/g;
        let sm;
        while ((sm = statePathRegex.exec(l))) touchedStatePaths.add(`/${sm[1]}`);
      }
    }
  }

  return {
    files: [...new Set(files)],
    addedImports,
    touchedStatePaths: [...touchedStatePaths]
  };
}

function parseGitDiff() {
  try {
    const raw = execSync('git diff --unified=0 --no-color', {
      encoding: 'utf8',
      stdio: ['ignore', 'pipe', 'pipe']
    });
    return parseDiffText(raw);
  } catch (e) {
    return {
      files: getChangedFiles(),
      addedImports: [],
      touchedStatePaths: []
    };
  }
}

function parseProposedPatchFile(filePath) {
  const payload = readJsonSafe(filePath, {});
  if (payload && payload.diff) {
    const fromDiff = parseDiffText(payload.diff);
    return {
      files: payload.files && payload.files.length ? payload.files : fromDiff.files,
      addedImports: fromDiff.addedImports,
      touchedStatePaths: fromDiff.touchedStatePaths
    };
  }
  return {
    files: payload.files || [],
    addedImports: payload.addedImports || [],
    touchedStatePaths: payload.touchedStatePaths || []
  };
}

function buildVirtualGraph(twin, patch) {
  const baseEdges = (twin.dependencyEdges || []).slice();
  for (const e of patch.addedImports || []) {
    baseEdges.push({ from: e.from, to: e.to, kind: 'virtual-import' });
  }

  const touchedUseCases = (patch.files || []).filter((f) => /service\/domain\/.+\/usecases\/.+\.js$/i.test(f));
  const touchedWorkflows = (twin.workflowGraph || [])
    .filter((w) => touchedUseCases.some((f) => String(w.workflow).toLowerCase().includes(f.split('/')[2] || '')))
    .map((w) => w.workflow);

  return {
    touchedFiles: patch.files || [],
    touchedStatePaths: patch.touchedStatePaths || [],
    touchedUseCases,
    touchedWorkflows,
    virtualEdgesAdded: (patch.addedImports || []).length,
    metrics: {
      edgeCountBefore: (twin.dependencyEdges || []).length,
      edgeCountAfter: baseEdges.length,
      deltaEdges: baseEdges.length - (twin.dependencyEdges || []).length
    },
    dependencyEdges: baseEdges
  };
}

if (require.main === module) {
  const arg = process.argv[2];
  const patch = (arg && fs.existsSync(arg)) ? parseProposedPatchFile(arg) : parseGitDiff();
  console.log(JSON.stringify(patch, null, 2));
}

module.exports = { parseDiffText, parseGitDiff, parseProposedPatchFile, buildVirtualGraph };
