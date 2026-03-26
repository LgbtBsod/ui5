#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { collectFilesByExtensions } = require('./qa-shared');

const ROOT = process.cwd();
const scanFiles = collectFilesByExtensions(ROOT, ['app', 'backend'], ['.js', '.xml', '.abap', '.py', '.css']);

function read(file) {
  return fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : '';
}

function rel(file) {
  return path.relative(ROOT, file).replace(/\\/g, '/');
}

const issues = [];
const ROOT_ALIAS_ALLOWLIST = [
  'backend/mock_gateway/api/gateway_canonical_api.py',
  'backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap',
  'backend/mock_gateway/tests/test_lock_gateway_api_contract.py',
  'backend/mock_gateway/tests/test_lock_session_save_contract.py'
];
const ROOTIDRUNTIME_ALLOWLIST = [
  'backend/mock_gateway/tests/test_closeout_invariants.py'
];

for (const file of scanFiles) {
  const text = read(path.join(ROOT, file));
  if (!text) continue;

  if (/Attachment\.Value/.test(text) && !/compat/i.test(file)) {
    issues.push(`${file}: Attachment.Value must stay out of productive attachment flow`);
  }

  if (/RootIdRuntime/.test(text) && ROOTIDRUNTIME_ALLOWLIST.indexOf(file) < 0) {
    issues.push(`${file}: RootIdRuntime import/reference must not remain after canonical root-key migration`);
  }

  if (/DetailContracts\.CODES/.test(text)) {
    issues.push(`${file}: DetailContracts.CODES drift must be removed`);
  }

  if (/["'](?:RootKey|RootId)["']|\.(?:RootKey|RootId)\b/.test(text) && ROOT_ALIAS_ALLOWLIST.indexOf(file) < 0) {
    issues.push(`${file}: RootKey/RootId must not remain outside explicit compatibility boundary`);
  }

  if (/return\s+Object\.freeze\(\s*\{\s*\}\s*\)/.test(text) || /return\s+\w+;\s*$/.test(text)) {
    issues.push(`${file}: suspicious trivial export; review for wrapper sprawl`);
  }
}

const localMetadata = read(path.join(ROOT, 'app/localService/metadata.xml'));
if (/Property Name="RootKey"/.test(localMetadata)) {
  issues.push('app/localService/metadata.xml: canonical metadata must not expose RootKey');
}
const checklistRootEntity = localMetadata.match(/<EntityType Name="ChecklistRoot"[\s\S]*?<\/EntityType>/);
if (checklistRootEntity && /Property Name="PARENT_KEY"/.test(checklistRootEntity[0])) {
  issues.push('app/localService/metadata.xml: ChecklistRoot must not define PARENT_KEY');
}
['ChecklistCheck', 'ChecklistBarrier', 'Attachment'].forEach((entityName) => {
  const entityMatch = localMetadata.match(new RegExp(`<EntityType Name="${entityName}"[\\s\\S]*?<\\/EntityType>`));
  if (entityMatch && !/Property Name="PARENT_KEY"/.test(entityMatch[0])) {
    issues.push(`app/localService/metadata.xml: ${entityName} must expose PARENT_KEY`);
  }
});

const mockMetadataSource = read(path.join(ROOT, 'backend/mock_gateway/services/metadata_builder.py'));
const mockMetadata = /read_text/.test(mockMetadataSource) ? localMetadata : mockMetadataSource;
if (/Property Name="RootKey"/.test(mockMetadata)) {
  issues.push('backend/mock_gateway/services/metadata_builder.py: mock metadata must not expose RootKey');
}
if (!/DownloadUrl/.test(mockMetadata) || !/DocumentHandle/.test(mockMetadata)) {
  issues.push('backend/mock_gateway/services/metadata_builder.py: attachment metadata must expose DownloadUrl and DocumentHandle');
}

for (const file of scanFiles.filter((f) => f.endsWith('.js') && /constants/i.test(f))) {
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
