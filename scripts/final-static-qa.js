#!/usr/bin/env node
const {
  fileExists,
  readText,
  collectFilesByExtensions,
  runNodeScriptAsCheck,
  createCheckAggregator
} = require('./qa-shared');

const root = process.cwd();

function read(file) { return readText(root, file); }
function exists(file) { return fileExists(root, file); }

function getFiles(dirs, extensions) {
  return collectFilesByExtensions(root, dirs, extensions);
}

function runValidatorScript(scriptName) {
  return runNodeScriptAsCheck(root, `scripts/${scriptName}`, scriptName);
}

const { check, printReport } = createCheckAggregator();

function extractDefineDeps(file) {
  const src = read(file);
  const m = src.match(/sap\.ui\.define\s*\(\s*\[([\s\S]*?)\]/);
  if (!m) return [];
  return [...m[1].matchAll(/["']([^"']+)["']/g)].map((x) => x[1]);
}

function collectControllerSupportFiles(ctrlPath) {
  const visited = new Set();
  const queue = [ctrlPath];
  while (queue.length) {
    const file = queue.shift();
    if (!file || visited.has(file) || !exists(file)) continue;
    visited.add(file);
    const src = read(file);
    extractDefineDeps(file).forEach((dep) => {
      if (!dep.startsWith('checklist/app/controller/support/')) return;
      const depFile = dep.replace(/^checklist\/app\//, '') + '.js';
      if (exists(depFile) && !visited.has(depFile)) queue.push(depFile);
    });
  }
  return [...visited];
}

function collectControllerMethodSet(ctrlPath) {
  const methods = new Set();
  collectControllerSupportFiles(ctrlPath).forEach((file) => {
    const src = read(file);
    [...src.matchAll(/([A-Za-z0-9_]+)\s*:\s*function\s*\(/g)].forEach((m) => methods.add(m[1]));
  });
  return methods;
}

check('XML handlers exist in owning controllers', () => {
  const xmlFiles = getFiles(['view'], ['.xml']);
  const eventAttrs = ['press','change','liveChange','search','selectionChange','itemPress','confirm','cancel','close','initialise','filterChange','beforeRebindTable','dataReceived'];
  const missing = [];
  for (const file of xmlFiles) {
    const xml = read(file);
    const mCtrl = xml.match(/controllerName\s*=\s*"([^"]+)"/);
    if (!mCtrl) continue;
    const ctrlPath = mCtrl[1].replace(/^checklist\.app\./,'').replace(/\./g,'/') + '.controller.js';
    if (!exists(ctrlPath)) { missing.push(`${file}: missing controller ${ctrlPath}`); continue; }
    const methods = collectControllerMethodSet(ctrlPath);
    const rx = new RegExp(`\\s(?:${eventAttrs.join('|')})\\s*=\\s*"\\.?([A-Za-z0-9_]+)"`, 'g');
    let mm;
    while ((mm = rx.exec(xml))) {
      const fn = mm[1];
      if (!methods.has(fn)) missing.push(`${file} -> ${ctrlPath} missing ${fn}`);
    }
  }
  return { ok: missing.length===0, detail: missing.join('\n') };
});

check('Routing integrity (manifest routes + used route names)', () => {
  const manifest = JSON.parse(read('manifest.json'));
  const routes = (((manifest['sap.ui5']||{}).routing||{}).routes)||[];
  const routeNames = new Set(routes.map(r=>r.name));
  const jsFiles = [...getFiles(['controller','util','infra'], ['.js']), 'Component.js'];
  const used = new Set();
  const re = /(navTo|getRoute|attachPatternMatched)\s*\(\s*["']([^"']+)["']/g;
  for (const f of jsFiles) {
    const s = read(f);
    let m; while ((m = re.exec(s))) used.add(m[2]);
  }
  const unknown = [...used].filter(n => !routeNames.has(n));
  return { ok: unknown.length===0, detail: unknown.join(', ') };
});

check('Module resolution for local checklist.app dependencies', () => {
  const jsFiles = [...getFiles(['controller','util','infra','service'], ['.js']), 'Component.js'];
  const missing = [];
  for (const f of jsFiles) {
    const s = read(f);
    const mm = s.match(/sap\.ui\.define\s*\(\s*\[([\s\S]*?)\]/);
    if (!mm) continue;
    const deps = [...mm[1].matchAll(/["']([^"']+)["']/g)].map(x=>x[1]);
    for (const d of deps) {
      if (!d.startsWith('checklist/app/')) continue;
      const rel = d.replace(/^checklist\/app\//,'') + '.js';
      if (!exists(rel)) missing.push(`${f}: ${d}`);
    }
  }
  return { ok: missing.length===0, detail: missing.join('\n') };
});

check('No fallback REST/dataset anti-patterns on UI runtime path', () => {
  const files = getFiles(['controller','util','service'], ['.js']).concat(getFiles(['view'], ['.xml']));
  let hasChecklistRest = false;
  let hasDatasetFallback = false;
  for (const f of files) {
    const s = read(f);
    if (/\/[Cc]hecklist(\b|\?|'|"|`)/.test(s) && !/pattern\s*:\s*"checklist\//.test(s)) hasChecklistRest = true;
    if (/setModel\s*\(\s*new\s+sap\.ui\.model\.json\.JSONModel\s*\(\s*\[/.test(s)) hasDatasetFallback = true;
  }
  return { ok: !hasChecklistRest && !hasDatasetFallback, detail: `checklistRest=${hasChecklistRest}, datasetFallback=${hasDatasetFallback}` };
});

check('FCL integrity (App.view contains FlexibleColumnLayout + layout css hooks)', () => {
  const app = read('view/App.view.xml');
  const cssFiles = [
    'css/claude-hyper.css',
    'css/modules/10_base.css',
    'css/modules/20_surface.css',
    'css/modules/21_controls.css',
    'css/modules/41_page_detail.css'
  ];
  const cssJoined = cssFiles.filter(exists).map(read).join('\n');
  const ok = app.includes('f:FlexibleColumnLayout')
    && app.includes('id="mainFcl"')
    && app.includes('detailPaneHost')
    && cssJoined.includes('.appLayoutSingle')
    && cssJoined.includes('.appLayoutSplit')
    && cssJoined.includes('.appLayoutDetailOnly');
  return { ok, detail: 'missing FCL host or app layout mode hooks' };
});

check('OData + SmartTable integrity', () => {
  const manifest = JSON.parse(read('manifest.json'));
  const modelType = manifest?.['sap.ui5']?.models?.mainService?.type;
  const searchView = read('view/Search.view.xml');
  const hasEntity = /smartTable:SmartTable[\s\S]*entitySet="ChecklistSearchSet"/.test(searchView);
  const searchFiles = collectControllerSupportFiles('controller/Search.controller.js');
  const hasBeforeRebind = collectControllerMethodSet('controller/Search.controller.js').has('onBeforeSmartTableRebind');
  const hasFailSegmentBuilder = searchFiles.some((file) => /SearchFilterBuilder\.buildFailSegmentFilter/.test(read(file)));
  const hasContract = hasBeforeRebind && hasFailSegmentBuilder;
  return { ok: modelType === 'sap.ui.model.odata.v2.ODataModel' && hasEntity && hasContract, detail: `modelType=${modelType}, entity=${hasEntity}, beforeRebind=${hasBeforeRebind}, failSegmentBuilder=${hasFailSegmentBuilder}` };
});

[
  ['Gateway parity validator', 'gateway-parity-validator.js'],
  ['SAP Gateway only gate', 'sap-gateway-only-gate.js'],
  ['Smart OData contract gate', 'smart-odata-contract-gate.js'],
  ['Architecture gate', 'architecture-gate.js'],
  ['Runtime settings gate', 'runtime-settings-gate.js'],
  ['Forbidden pattern scanner', 'forbidden-patterns.js'],
  ['Layer map generator', 'layer-map.js']
].forEach(([title, script]) => {
  check(title, () => runValidatorScript(script));
});

const failed = printReport('Static QA Report');
process.exit(failed.length ? 1 : 0);
