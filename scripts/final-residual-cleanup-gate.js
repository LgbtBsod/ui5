#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { collectFilesByExtensions } = require('./qa-shared');

const ROOT = process.cwd();
const scanFiles = collectFilesByExtensions(ROOT, ['app', 'backend'], ['.js', '.xml', '.abap', '.py', '.css']);
const sapInternalCssAllowlist = JSON.parse(
  fs.readFileSync(path.join(ROOT, 'scripts', 'sap-internal-css-allowlist.json'), 'utf8')
);

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
const OBJECTUUID_ALLOWLIST = [
  'backend/mock_gateway/api/gateway_canonical_api.py',
  'backend/mock_gateway/tests/test_gateway_contract_frontend_aliases.py',
  'backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap'
];
ROOT_ALIAS_ALLOWLIST.push('backend/mock_gateway/tests/test_closeout_invariants.py');
const CSS_PRIVATE_SELECTOR_ALLOWLIST = new Set(Object.keys(sapInternalCssAllowlist || {}));

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

  if (/\bObjectUuid\b|\bobjectUuid\b/.test(text) && OBJECTUUID_ALLOWLIST.indexOf(file) < 0) {
    issues.push(`${file}: ObjectUuid/objectUuid must not remain outside explicit compatibility boundary`);
  }

  if (/return\s+Object\.freeze\(\s*\{\s*\}\s*\)/.test(text) || /return\s+\w+;\s*$/.test(text)) {
    issues.push(`${file}: suspicious trivial export; review for wrapper sprawl`);
  }

  if (
    file.endsWith('.css') &&
    !CSS_PRIVATE_SELECTOR_ALLOWLIST.has(file) &&
    /\.(sapUiCompFilterBarToolbar|sapUxAPObjectPage[A-Za-z-]*|sapMSwtCont|sapUiTable(?:Cnt|Ctrl|CtrlScr|CCnt)?|sapMDialogScrollCont)\b/.test(text)
  ) {
    issues.push(`${file}: private UI5 selectors must be quarantined to the compatibility allowlist`);
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

const analyticsController = read(path.join(ROOT, 'app/controller/Analytics.controller.js'));
if (/AnalyticsContracts\.MESSAGES\.(ANALYTICS_LOAD_FAILED|ANALYTICS_REFRESH_FAILED|ANALYTICS_UNAVAILABLE)/.test(analyticsController) && !/_resolveAnalyticsMessage/.test(analyticsController)) {
  issues.push('app/controller/Analytics.controller.js: analytics UI error path must translate message keys through i18n before writing view>/error');
}
const analyticsExportRuntime = read(path.join(ROOT, 'app/controller/analytics/AnalyticsExportRuntime.js'));
['nothingToExport', 'searchExportSuccess', 'exportFailed'].forEach((key) => {
  if (analyticsExportRuntime.indexOf(`"${key}"`) >= 0) {
    issues.push(`app/controller/analytics/AnalyticsExportRuntime.js: raw i18n key literal ${key} must be routed through MessageKeyConstants`);
  }
});
const appController = read(path.join(ROOT, 'app/controller/App.controller.js'));
if (appController.indexOf('security_token_refresh_unavailable') >= 0) {
  issues.push('app/controller/App.controller.js: raw technical diagnostics must use a dedicated code owner');
}
const attachmentReadme = read(path.join(ROOT, 'backend/mock_gateway/README_ODATA.md'));
if (!/media upload to `AttachmentSet`/i.test(attachmentReadme) || !/no parallel repository upload boundary/i.test(attachmentReadme)) {
  issues.push('backend/mock_gateway/README_ODATA.md: attachment contract docs must state the final media-only architecture');
}
const smokePack = read(path.join(ROOT, 'scripts/gateway-only-smoke-pack.py'));
if (smokePack.indexOf('ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN') < 0) {
  issues.push('scripts/gateway-only-smoke-pack.py: smoke coverage must prove the base64 save path stays forbidden');
}
if (fs.existsSync(path.join(ROOT, 'dist/infra/adapters/shared/AttachmentRepoRuntime.js'))) {
  issues.push('dist/infra/adapters/shared/AttachmentRepoRuntime.js: stale attachment upload seam must stay deleted from dist output');
}
if (fs.existsSync(path.join(ROOT, 'dist/service/framework/WorkflowCoordinator.js'))) {
  issues.push('dist/service/framework/WorkflowCoordinator.js: stale workflow wrapper must stay deleted from dist output');
}

const outputs = issues.length ? ['FAIL final-residual-cleanup-gate', ...issues.map((i) => `- ${i}`)] : ['PASS final-residual-cleanup-gate'];
console.log(outputs.join('\n'));
process.exit(issues.length ? 1 : 0);
