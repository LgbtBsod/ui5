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

function fragmentNameToFile(fragmentName) {
  if (!fragmentName) return '';
  return fragmentName.replace(/^PRODUCTION_CONTROL_CHECKLIST\./, '').replace(/\./g, '/') + '.fragment.xml';
}

function collectCompositeXml(entryFile, visited) {
  const seen = visited || new Set();
  if (!entryFile || seen.has(entryFile) || !exists(entryFile)) return [];
  seen.add(entryFile);
  const xml = read(entryFile);
  const out = [{ file: entryFile, xml }];
  [...xml.matchAll(/fragmentName\s*=\s*"([^"]+)"/g)].forEach((match) => {
    collectCompositeXml(fragmentNameToFile(match[1]), seen).forEach((entry) => out.push(entry));
  });
  return out;
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
      if (!dep.startsWith('PRODUCTION_CONTROL_CHECKLIST/controller/')) return;
      const depFile = dep.replace(/^PRODUCTION_CONTROL_CHECKLIST\//, '') + '.js';
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
  const xmlFiles = getFiles(['views'], ['.xml']);
  const eventAttrs = ['press','change','liveChange','search','selectionChange','itemPress','confirm','cancel','close','initialise','filterChange','beforeRebindTable','dataReceived'];
  const missing = [];
  for (const file of xmlFiles) {
    const xml = read(file);
    const mCtrl = xml.match(/controllerName\s*=\s*"([^"]+)"/);
    if (!mCtrl) continue;
    const ctrlPath = mCtrl[1].replace(/^PRODUCTION_CONTROL_CHECKLIST\./,'').replace(/\./g,'/') + '.controller.js';
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

check('Module resolution for local PRODUCTION_CONTROL_CHECKLIST dependencies', () => {
  const jsFiles = [...getFiles(['controller','util','infra','service'], ['.js']), 'Component.js'];
  const missing = [];
  for (const f of jsFiles) {
    const s = read(f);
    const mm = s.match(/sap\.ui\.define\s*\(\s*\[([\s\S]*?)\]/);
    if (!mm) continue;
    const deps = [...mm[1].matchAll(/["']([^"']+)["']/g)].map(x=>x[1]);
    for (const d of deps) {
      if (!d.startsWith('PRODUCTION_CONTROL_CHECKLIST/')) continue;
      const rel = d.replace(/^PRODUCTION_CONTROL_CHECKLIST\//,'') + '.js';
      if (!exists(rel)) missing.push(`${f}: ${d}`);
    }
  }
  return { ok: missing.length===0, detail: missing.join('\n') };
});

check('No fallback REST/dataset anti-patterns on UI runtime path', () => {
  const files = getFiles(['controller','util','service'], ['.js']).concat(getFiles(['views'], ['.xml']));
  let hasChecklistRest = false;
  let hasDatasetFallback = false;
  for (const f of files) {
    const s = read(f);
    if (/(^|[^#])\/checklist(?:\/|\?|["'`]|$)/i.test(s) && !/pattern\s*:\s*"checklist\//.test(s)) hasChecklistRest = true;
    if (/setModel\s*\(\s*new\s+sap\.ui\.model\.json\.JSONModel\s*\(\s*\[/.test(s)) hasDatasetFallback = true;
  }
  return { ok: !hasChecklistRest && !hasDatasetFallback, detail: `checklistRest=${hasChecklistRest}, datasetFallback=${hasDatasetFallback}` };
});

check('FCL integrity (App.view contains FlexibleColumnLayout + layout css hooks)', () => {
  const app = read('views/App.view.xml');
  const cssFiles = [
    'styles/app-styles.css',
    'styles/modules/10_base.css',
    'styles/modules/20_surface.css',
    'styles/modules/21_controls.css',
    'styles/modules/41_page_detail.css'
  ];
  const cssJoined = cssFiles.filter(exists).map(read).join('\n');
  const ok = app.includes('f:FlexibleColumnLayout')
    && app.includes('id="mainFcl"')
    && app.includes('appRootFclTransparent')
    && cssJoined.includes('.appLayoutSingle')
    && cssJoined.includes('.appLayoutSplit')
    && cssJoined.includes('.appLayoutDetailOnly');
  return { ok, detail: 'missing FCL host or app layout mode hooks' };
});

check('OData + SmartTable integrity', () => {
  const manifest = JSON.parse(read('manifest.json'));
  const modelType = manifest?.['sap.ui5']?.models?.mainService?.type;
  const searchView = collectCompositeXml('views/Search.view.xml').map((entry) => entry.xml).join('\n');
  const hasEntity = /smartTable:SmartTable[\s\S]*entitySet="ChecklistSearchSet"/.test(searchView);
  const searchFiles = collectControllerSupportFiles('controller/Search.controller.js');
  const hasBeforeRebind = collectControllerMethodSet('controller/Search.controller.js').has('onBeforeSmartTableRebind');
  const hasFailSegmentBuilder = searchFiles.some((file) => /SearchFilterBuilder\.(buildFailSegmentFilter|buildChecksFailSegmentFilter|buildBarriersFailSegmentFilter)/.test(read(file)))
    || exists('service/features/search/contracts/SearchFilterBuilder.js');
  const hasContract = hasBeforeRebind && hasFailSegmentBuilder;
  return { ok: modelType === 'sap.ui.model.odata.v2.ODataModel' && hasEntity && hasContract, detail: `modelType=${modelType}, entity=${hasEntity}, beforeRebind=${hasBeforeRebind}, failSegmentBuilder=${hasFailSegmentBuilder}` };
});

[
  ['Gateway parity validator', 'run-gate.js gateway-parity-validator'],
  ['SAP Gateway only gate', 'sap-gateway-only-gate.js'],
  ['Smart OData contract gate', 'smart-odata-contract-gate.js'],
  ['Architecture gate', 'run-gate.js architecture-gate'],
  ['Runtime settings gate', 'runtime-settings-gate.js'],
  ['Forbidden pattern scanner', 'forbidden-patterns.js'],
].forEach(([title, script]) => {
  check(title, () => runValidatorScript(script));
});

const failed = printReport('Static QA Report');
process.exit(failed.length ? 1 : 0);
