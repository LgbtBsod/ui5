const fs = require('fs');
const path = require('path');

function toPosix(p) { return p.split(path.sep).join('/'); }

function parseStdImports(source) {
  const deps = [];
  const re = /(?:import\s+[^'"\n]+from\s+|require\()\s*['"]([^'"]+)['"]/g;
  let m = re.exec(source);
  while (m) { deps.push({ dep: m[1], kind: 'import' }); m = re.exec(source); }
  return deps;
}

function parseUi5ArrayCalls(source, callName, kind) {
  const deps = [];
  const re = new RegExp(`${callName}\\s*\\(\\s*(?:['\"][^'\"]+['\"]\\s*,\\s*)?\\[([\\s\\S]*?)\\]`, 'g');
  let m = re.exec(source);
  while (m) {
    const arr = m[1];
    const im = arr.matchAll(/["']([^"']+)["']/g);
    for (const hit of im) deps.push({ dep: hit[1], kind });
    m = re.exec(source);
  }
  return deps;
}

function parseRequireSync(source) {
  const deps = [];
  const re = /sap\.ui\.requireSync\s*\(\s*['"]([^'"]+)['"]\s*\)/g;
  let m = re.exec(source);
  while (m) { deps.push({ dep: m[1], kind: 'dynamic' }); m = re.exec(source); }
  return deps;
}

function parseDeps(source) {
  return [
    ...parseStdImports(source),
    ...parseUi5ArrayCalls(source, 'sap\\.ui\\.define', 'ui5-define'),
    ...parseUi5ArrayCalls(source, 'sap\\.ui\\.require', 'ui5-require'),
    ...parseRequireSync(source)
  ];
}

function resolveDep(file, dep, rootDir) {
  const dir = path.dirname(path.resolve(rootDir, file));
  if (dep.startsWith('sap_ui5/')) {
    const mapped = dep.replace(/^sap_ui5\//, '');
    return toPosix(mapped.endsWith('.js') ? mapped : `${mapped}.js`);
  }
  if (dep.startsWith('.')) {
    let p = path.resolve(dir, dep);
    if (fs.existsSync(p) && fs.statSync(p).isDirectory()) p = path.join(p, 'index.js');
    else if (!p.endsWith('.js')) p += '.js';
    return toPosix(path.relative(rootDir, p));
  }
  return null;
}

function scanFile(file, opts = {}) {
  const rootDir = opts.rootDir || process.cwd();
  const source = fs.readFileSync(path.resolve(rootDir, file), 'utf8');
  const parsed = parseDeps(source);
  return parsed.map((d) => ({
    dep: d.dep,
    kind: d.kind,
    resolved: resolveDep(file, d.dep, rootDir)
  }));
}

module.exports = { scanFile, parseDeps, resolveDep };
