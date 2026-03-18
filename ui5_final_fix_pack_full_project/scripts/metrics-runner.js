#!/usr/bin/env node

const path = require('path');
const { listFiles } = require('./lib/fileWalker');
const { readSafe } = require('./lib/textRead');
const { extractFunctions } = require('./lib/functionExtract');
const { buildGraph, fanStats, detectCycles } = require('./lib/importGraph');
const { sha256 } = require('./lib/hashUtils');
const { fingerprint } = require('./lib/fingerprint');
const { emitJson } = require('./lib/report');

const root = path.resolve(__dirname, '..');
const files = listFiles(root, { include: ['controller/**/*.js', 'service/**/*.js', 'infra/**/*.js', 'ports/**/*.js', 'model/**/*.js', 'util/**/*.js', 'scripts/**/*.js', 'Component.js'] });
const records = [];
const fileMap = {};
const fnDup = new Map();
const fileDup = new Map();

files.forEach((file) => {
  const read = readSafe(root, file);
  if (!read.ok) return;
  const funcs = extractFunctions(read.text);
  const longCount = funcs.filter((fn) => fn.length > 60).length;
  records.push({ file, lines: read.lines, funcs, longCount });
  fileMap[file] = read.text;
  fileDup.set(sha256(read.text), [...(fileDup.get(sha256(read.text)) || []), file]);
  funcs.forEach((fn) => fnDup.set(fingerprint(fn.text), [...(fnDup.get(fingerprint(fn.text)) || []), file]));
});

const graph = buildGraph(fileMap);
const stats = fanStats(graph);
const cycles = detectCycles(graph);
const topFiles = records.sort((a, b) => b.lines - a.lines).slice(0, 50);
const topFns = records.flatMap((r) => r.funcs.map((fn) => ({ file: r.file, ...fn }))).sort((a, b) => b.length - a.length).slice(0, 50);
const dupFnGroups = [...fnDup.values()].filter((g) => new Set(g).size > 1);
const dupFileGroups = [...fileDup.values()].filter((g) => g.length > 1);
const hotspot = records.map((r) => {
  const fanOut = stats.fanOut.get(r.file) || 0;
  const dupScore = dupFnGroups.filter((g) => g.includes(r.file)).length;
  return { file: r.file, score: (r.lines * (1 + fanOut)) + (r.longCount * 50) + (dupScore * 30) };
}).sort((a, b) => b.score - a.score).slice(0, 50);

const compliance = {
  feedback: files.filter((f) => /MessageBox|MessageToast/.test(fileMap[f] || '') && f !== 'service/framework/EffectApplier.js').length,
  style: files.filter((f) => /addStyleClass\(|removeStyleClass\(/.test(fileMap[f] || '') && f !== 'infra/adapters/Ui5StyleAdapter.js').length,
  manager: files.filter((f) => f.startsWith('service/runtime/') && /setProperty\(|setData\(/.test(fileMap[f] || '')).length,
  usecaseUi5: files.filter((f) => f.includes('/usecases/') && f.startsWith('service/domain/') && /['"]sap\//.test(fileMap[f] || '')).length
};

console.log('=== METRICS REPORT ===');
console.log('Top 50 largest JS files:', topFiles.length);
console.log('Top 50 longest functions:', topFns.length);
console.log('Top hotspots:', hotspot.slice(0, 10).map((h) => `${h.file} (${h.score})`).join(', '));
console.log('Coupling summary:');
console.log(' - top fan-out:', [...stats.fanOut.entries()].sort((a, b) => b[1] - a[1]).slice(0, 10).map(([f, n]) => `${f}:${n}`).join(', '));
console.log(' - top fan-in:', [...stats.fanIn.entries()].sort((a, b) => b[1] - a[1]).slice(0, 10).map(([f, n]) => `${f}:${n}`).join(', '));
console.log(` - cycles: ${cycles.length}`);
console.log(`Reuse: file duplicate groups=${dupFileGroups.length}, function duplicate groups=${dupFnGroups.length}`);
console.log('Architecture compliance summary:', compliance);

emitJson({ topFiles, topFunctions: topFns, hotspots: hotspot, fanOut: [...stats.fanOut.entries()], fanIn: [...stats.fanIn.entries()], cycles, reuse: { dupFileGroups, dupFnGroups }, compliance });
