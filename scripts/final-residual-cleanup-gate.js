#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { collectFilesByExtensions } = require('./qa-shared');

const ROOT = process.cwd();
const jsFiles = collectFilesByExtensions(ROOT, ['app', 'backend'], ['.js', '.xml', '.abap']);

function read(file) {
  return fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : '';
}

function rel(file) {
  return path.relative(ROOT, file).replace(/\\/g, '/');
}

const issues = [];

for (const file of jsFiles) {
  const text = read(path.join(ROOT, file));
  if (!text) continue;

  if (/Attachment\.Value/.test(text) && !/compat/i.test(file)) {
    issues.push(`${file}: Attachment.Value must stay out of productive attachment flow`);
  }

  if (/\bRootId\b/.test(text) && /app\/(model|constants|view|controls)\//.test(file) && !/compat/i.test(file)) {
    issues.push(`${file}: RootId alias must stay in compatibility boundary only`);
  }

  if (/sapM|sapUi|sapF|sapMITB/.test(text) && /app\/styles\//.test(file)) {
    issues.push(`${file}: private UI5 selector usage requires whitelist review`);
  }

  if (/return\s+Object\.freeze\(\s*\{\s*\}\s*\)/.test(text) || /return\s+\w+;\s*$/.test(text)) {
    issues.push(`${file}: suspicious trivial export; review for wrapper sprawl`);
  }
}

for (const file of jsFiles.filter((f) => f.endsWith('.js') && /constants/i.test(f))) {
  const text = read(path.join(ROOT, file));
  if (/DetailContracts/.test(text) && /Message(Key|Code)Constants/.test(file)) {
    issues.push(`${file}: proxy dependency on DetailContracts should be removed`);
  }
}

const backendText = read(path.join(ROOT, 'backend/sap_backend/src/zif_zodata_contract_constants.intf.abap'));
if (/c_code_|c_msg_/.test(backendText)) {
  issues.push('backend/sap_backend/src/zif_zodata_contract_constants.intf.abap: technical contract must not contain machine-readable codes or human-readable texts');
}

const outputs = issues.length ? ['FAIL final-residual-cleanup-gate', ...issues.map((i) => `- ${i}`)] : ['PASS final-residual-cleanup-gate'];
console.log(outputs.join('\n'));
process.exit(issues.length ? 1 : 0);
