#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { readJsonSafe, readTextSafe } = require('./lib/auditInput');

const ROOT = process.cwd();
const configPath = process.argv[2] || 'scripts/duplicate-governance-config.json';
const verboseDetails = process.argv.includes('--details');

function normalize(filePath) {
  return filePath.replace(/\\/g, '/');
}

function collectFiles(relDir, extensions, out) {
  const absDir = path.join(ROOT, relDir);
  if (!fs.existsSync(absDir)) return;

  for (const entry of fs.readdirSync(absDir, { withFileTypes: true })) {
    if (entry.name === '.git' || entry.name === 'node_modules') continue;

    const relPath = normalize(path.join(relDir, entry.name));
    if (entry.isDirectory()) {
      collectFiles(relPath, extensions, out);
      continue;
    }

    if (extensions.some((ext) => relPath.toLowerCase().endsWith(String(ext).toLowerCase()))) {
      out.push(relPath);
    }
  }
}

function isBoilerplateBlock(block) {
  const lines = block.split('\n').map((line) => line.trim()).filter(Boolean);
  if (!lines.length) return true;

  return lines.every((line) => (
    line === '#!/usr/bin/env node'
    || line.startsWith('const ')
    || line.startsWith('let ')
    || line.startsWith('var ')
    || line.startsWith('} = require(')
    || line.startsWith('require(')
  ));
}

function normalizeBlock(block) {
  return block
    .replace(/\/\/.*$/gm, ' ')
    .replace(/\/\*[\s\S]*?\*\//g, ' ')
    .replace(/`(?:\\.|[^`])*`/g, ' STR ')
    .replace(/"(?:\\.|[^"])*"/g, ' STR ')
    .replace(/'(?:\\.|[^'])*'/g, ' STR ')
    .replace(/\b\d+(?:\.\d+)?\b/g, ' NUM ')
    .replace(/[A-Za-z_][A-Za-z0-9_]*/g, ' ID ')
    .replace(/\s+/g, ' ')
    .trim();
}

function buildClusters(files, opts) {
  const clusters = new Map();
  files.forEach((file) => {
    const lines = readTextSafe(path.join(ROOT, file), '').split(/\r?\n/);
    for (let i = 0; i <= lines.length - opts.blockLines; i += 1) {
      const rawBlock = lines.slice(i, i + opts.blockLines).join('\n').trim();
      if (rawBlock.length < opts.minBlockChars || isBoilerplateBlock(rawBlock)) continue;

      const key = opts.semanticMode ? normalizeBlock(rawBlock) : rawBlock;
      if (!key || key.length < opts.minNormalizedChars || isBoilerplateBlock(key)) continue;

      if (!clusters.has(key)) {
        clusters.set(key, {
          sampleRawBlock: rawBlock,
          places: new Map()
        });
      }

      const cluster = clusters.get(key);
      if (!cluster.places.has(file)) cluster.places.set(file, []);
      cluster.places.get(file).push(i + 1);
    }
  });

  return clusters;
}

function summarizeClusters(clusters, sampleLimit) {
  let sameRootClusters = 0;
  let crossRootClusters = 0;
  const sameRootSamples = [];
  const crossRootSamples = [];

  for (const cluster of clusters.values()) {
    if (cluster.places.size < 2) continue;

    const rootsInCluster = new Set([...cluster.places.keys()].map((f) => f.split('/')[0]));
    const sample = {
      locations: [...cluster.places.entries()].slice(0, sampleLimit).map(([file, lines]) => `${file}:${lines[0]}`),
      snippet: cluster.sampleRawBlock.split('\n').slice(0, 3).join(' | ')
    };

    if (rootsInCluster.size === 1) {
      sameRootClusters += 1;
      if (sameRootSamples.length < sampleLimit) sameRootSamples.push(sample);
    } else {
      crossRootClusters += 1;
      if (crossRootSamples.length < sampleLimit) crossRootSamples.push(sample);
    }
  }

  return { sameRootClusters, crossRootClusters, sameRootSamples, crossRootSamples };
}

function printSamples(label, samples) {
  samples.forEach((sample) => {
    console.log(`- ${label}: ${sample.locations.join(', ')}`);
    if (verboseDetails) {
      console.log(`  snippet: ${sample.snippet}`);
    }
  });
}

function main() {
  if (!fs.existsSync(path.join(ROOT, configPath))) {
    console.error(`Duplicate governance config not found: ${configPath}`);
    process.exit(2);
  }

  const cfg = readJsonSafe(path.join(ROOT, configPath), null);
  if (!cfg) {
    console.error(`Duplicate governance config is invalid JSON: ${configPath}`);
    process.exit(2);
  }
  const roots = Array.isArray(cfg.roots) ? cfg.roots : ['scripts'];
  const extensions = Array.isArray(cfg.extensions) ? cfg.extensions : ['.js'];
  const exclude = new Set((cfg.excludeFiles || []).map((f) => normalize(f)));
  const sampleLimit = Number(cfg.sampleLimit || 5);

  const opts = {
    blockLines: Number(cfg.blockLines || 6),
    minBlockChars: Number(cfg.minBlockChars || 100),
    minNormalizedChars: Number(cfg.minNormalizedChars || 40)
  };

  const files = [];
  roots.forEach((root) => collectFiles(root, extensions, files));
  const uniqueFiles = [...new Set(files.map((f) => normalize(f)))].filter((f) => !exclude.has(f));

  const exactSummary = summarizeClusters(buildClusters(uniqueFiles, { ...opts, semanticMode: false }), sampleLimit);
  const semanticSummary = summarizeClusters(buildClusters(uniqueFiles, { ...opts, semanticMode: true }), sampleLimit);

  const maxCrossRootClusters = Number(cfg.maxCrossRootClusters || 5);
  const forbidSameRootClusters = cfg.forbidSameRootClusters !== false;

  const violations = [];
  if (forbidSameRootClusters && exactSummary.sameRootClusters > 0) {
    violations.push(`same-root duplicate clusters: ${exactSummary.sameRootClusters} > 0`);
  }
  if (exactSummary.crossRootClusters > maxCrossRootClusters) {
    violations.push(`cross-root duplicate clusters: ${exactSummary.crossRootClusters} > ${maxCrossRootClusters}`);
  }

  if (violations.length) {
    console.log('FAIL duplicate-governance-gate');
    violations.forEach((v) => console.log(`- ${v}`));
    printSamples('same-root sample', exactSummary.sameRootSamples);
    printSamples('cross-root sample', exactSummary.crossRootSamples);
    console.log(`semantic same-root clusters (advisory): ${semanticSummary.sameRootClusters}`);
    console.log(`semantic cross-root clusters (advisory): ${semanticSummary.crossRootClusters}`);
    process.exit(1);
  }

  console.log(
    `PASS duplicate-governance-gate ` +
    `[exact same=${exactSummary.sameRootClusters}, exact cross=${exactSummary.crossRootClusters}, ` +
    `semantic same=${semanticSummary.sameRootClusters}, semantic cross=${semanticSummary.crossRootClusters}]`
  );
  printSamples('semantic same-root sample', semanticSummary.sameRootSamples);
}

main();
