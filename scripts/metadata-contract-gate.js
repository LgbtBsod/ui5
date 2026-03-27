#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const childProcess = require('child_process');

const ROOT = process.cwd();
const metadataPath = path.join(ROOT, 'app', 'localService', 'metadata.xml');
const builderPath = path.join(ROOT, 'backend', 'mock_gateway', 'services', 'metadata_builder.py');
const abapPath = path.join(ROOT, 'backend', 'sap_backend', 'src', 'zcl_zodata_dpc_ext.clas.abap');
const metadata = fs.readFileSync(metadataPath, 'utf8');
const builder = fs.readFileSync(builderPath, 'utf8');
const abap = fs.readFileSync(abapPath, 'utf8');
const issues = [];

if (!/LOCAL_METADATA_PATH/.test(builder) || !/metadata\.xml/.test(builder)) {
  issues.push('metadata_builder.py no longer anchored to app/localService/metadata.xml');
}

try {
  const builtMetadata = childProcess.execFileSync('python', ['-c', 'from backend.mock_gateway.services.metadata_builder import build_metadata; print(build_metadata(), end="")'], {
    cwd: ROOT,
    encoding: 'utf8'
  });
  if (String(builtMetadata || '').replace(/\r\n/g, '\n') !== metadata.replace(/\r\n/g, '\n')) {
    issues.push('mock metadata builder output drifted from app/localService/metadata.xml');
  }
} catch (error) {
  issues.push(`unable to verify metadata builder parity: ${error.message}`);
}

['FunctionImport Name="SaveChanges"', 'FunctionImport Name="AutoSave"', 'EntitySet Name="AttachmentSet"', 'EntitySet Name="ChecklistRootSet"'].forEach((token) => {
  if (!metadata.includes(token)) {
    issues.push(`metadata missing token ${token}`);
  }
});

if (/\bRootId\b/.test(abap)) {
  issues.push('ABAP DPC still reads RootId instead of canonical DB_KEY boundary');
}

if (issues.length) {
  console.log(['FAIL metadata-contract-gate', ...issues.map((issue) => `- ${issue}`)].join('\n'));
  process.exit(1);
}

console.log('PASS metadata-contract-gate');
