#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const metadata = fs.readFileSync(path.join(ROOT, 'app', 'localService', 'metadata.xml'), 'utf8');
const mockApi = fs.readFileSync(path.join(ROOT, 'backend', 'mock_gateway', 'api', 'gateway_canonical_api.py'), 'utf8');
const mockGatewayReadme = fs.readFileSync(path.join(ROOT, 'backend', 'mock_gateway', 'README_ODATA.md'), 'utf8');
const appFiles = [];
const issues = [];

function walk(dir) {
  if (!fs.existsSync(dir)) {
    return;
  }
  fs.readdirSync(dir, { withFileTypes: true }).forEach((entry) => {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      walk(full);
      return;
    }
    if (/\.(js|xml|py|md|txt)$/i.test(entry.name)) {
      appFiles.push(full);
    }
  });
}

function rel(file) {
  return path.relative(ROOT, file).replace(/\\/g, '/');
}

const attachmentBlock = (metadata.match(/<EntityType Name="Attachment"[\s\S]*?<\/EntityType>/) || [''])[0];
const attachmentFieldNames = [...attachmentBlock.matchAll(/Property Name="([^"]+)"/g)].map((match) => match[1]);
['DownloadUrl', 'DocumentHandle', 'PARENT_KEY', 'DB_KEY'].forEach((field) => {
  if (!new RegExp(`Name="${field}"`).test(attachmentBlock)) {
    issues.push(`metadata attachment missing ${field}`);
  }
});
if (!/EntityType Name="Attachment"[^>]*HasStream="true"/.test(attachmentBlock)) {
  issues.push('metadata attachment must be stream-capable (HasStream="true")');
}
if (/Name="Value"/.test(attachmentBlock)) {
  issues.push('metadata attachment still exposes Value');
}

if (!/DownloadUrl/.test(mockApi) || !/DocumentHandle/.test(mockApi)) {
  issues.push('mock attachment API missing DownloadUrl/DocumentHandle');
}
if (!/persisted attachment upload uses only media upload to `AttachmentSet`/i.test(mockGatewayReadme)) {
  issues.push('backend/mock_gateway/README_ODATA.md must document AttachmentSet as the only productive upload boundary');
}
if (!/no parallel repository upload boundary is allowed/i.test(mockGatewayReadme)) {
  issues.push('backend/mock_gateway/README_ODATA.md must explicitly forbid parallel repository upload boundaries');
}
if (!/SaveChanges` and `CreateChecklist` must not carry productive base64 attachment payloads/i.test(mockGatewayReadme)) {
  issues.push('backend/mock_gateway/README_ODATA.md must reject productive base64 attachment persistence in save payloads');
}
if (!/DownloadUrl` \/ `DocumentHandle` are the productive binary access seam/i.test(mockGatewayReadme)) {
  issues.push('backend/mock_gateway/README_ODATA.md must document DownloadUrl/DocumentHandle as the productive binary access seam');
}
attachmentFieldNames.forEach((field) => {
  if (!new RegExp(`"${field}"\\s*:`).test(mockApi) && !['Description'].includes(field)) {
    issues.push(`mock attachment serialization missing metadata field ${field}`);
  }
});
if (/_boundary_parent_key\(item\)\s+or\s+root_hex/.test(mockApi) === false) {
  issues.push('mock attachment payload normalization no longer anchors child parent key to canonical PARENT_KEY/root fallback');
}

const attachmentRepoRuntimePath = path.join(ROOT, 'app', 'infra', 'adapters', 'shared', 'AttachmentRepoRuntime.js');
if (fs.existsSync(attachmentRepoRuntimePath)) {
  issues.push('AttachmentRepoRuntime must not exist next to the canonical repo-owned attachment boundary');
}
const distAttachmentRepoRuntimePath = path.join(ROOT, 'dist', 'infra', 'adapters', 'shared', 'AttachmentRepoRuntime.js');
if (fs.existsSync(distAttachmentRepoRuntimePath)) {
  issues.push('dist/infra/adapters/shared/AttachmentRepoRuntime.js must stay deleted so the bundle cannot resurrect the stale upload seam');
}
const attachmentGatewayRuntime = fs.readFileSync(path.join(ROOT, 'app', 'service', 'features', 'detail', 'runtime', 'AttachmentGatewayRuntime.js'), 'utf8');
if (!/X-DB-Key/.test(attachmentGatewayRuntime) || !/X-Parent-Key/.test(attachmentGatewayRuntime) || !/ENTITY_SETS\.ATTACHMENT/.test(attachmentGatewayRuntime)) {
  issues.push('AttachmentGatewayRuntime must remain the canonical media upload owner for AttachmentSet');
}
const attachmentUploadRuntime = fs.readFileSync(path.join(ROOT, 'app', 'service', 'features', 'detail', 'runtime', 'AttachmentUploadRuntime.js'), 'utf8');
if (!/uploadPendingAttachments/.test(attachmentUploadRuntime) || !/attachmentLoad/.test(attachmentUploadRuntime)) {
  issues.push('AttachmentUploadRuntime must orchestrate media upload through AttachmentGatewayRuntime and then reload attachment metadata');
}
if (/AttachmentRepoRuntime/.test(attachmentUploadRuntime) || /AttachmentRepoRuntime/.test(attachmentGatewayRuntime)) {
  issues.push('attachment runtime must not reintroduce a repository upload seam next to the canonical media boundary');
}
if (/ContentBase64|Value/.test(attachmentUploadRuntime)) {
  issues.push('AttachmentUploadRuntime must not reintroduce productive base64/value attachment persistence');
}
if (!/parentKey:\s*sDbKey/.test(attachmentUploadRuntime.replace(/\r?\n/g, " "))) {
  issues.push('AttachmentUploadRuntime must keep persisted attachment ownership on canonical parentKey/dbKey semantics');
}
if (!/ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN/.test(mockApi)) {
  issues.push('mock gateway must explicitly reject base64 attachment payloads on SaveChanges');
}
if (!/allow_media_content=True/.test(mockApi)) {
  issues.push('mock gateway attachment media endpoint must be the only allowed binary upload path');
}
if (!/SaveChanges[\s\S]*ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN/.test(mockApi) || !/CreateChecklist[\s\S]*ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN/.test(mockApi)) {
  issues.push('mock gateway must reject attachment base64 persistence on both SaveChanges and CreateChecklist');
}
[
  ['scripts/curl-regression-pack.sh', fs.readFileSync(path.join(ROOT, 'scripts', 'curl-regression-pack.sh'), 'utf8')],
  ['scripts/testing/curl_regression.sh', fs.readFileSync(path.join(ROOT, 'scripts', 'testing', 'curl_regression.sh'), 'utf8')]
].forEach(([name, text]) => {
  if (!/AttachmentSet/.test(text) || !/X-Parent-Key/.test(text)) {
    issues.push(`${name} must exercise AttachmentSet media upload with canonical attachment headers`);
  }
  if (/RootKeys/.test(text) || /AttachmentSet\(Key=/.test(text) || /\/\$value/.test(text)) {
    issues.push(`${name} still describes stale attachment or export transport semantics`);
  }
  if (!/DownloadUrl|DocumentHandle/.test(text)) {
    issues.push(`${name} must acknowledge DownloadUrl / DocumentHandle as the persisted binary access seam`);
  }
});

walk(path.join(ROOT, 'app'));
walk(path.join(ROOT, 'backend'));

appFiles.forEach((file) => {
  const relative = rel(file);
  if (relative.includes('__pycache__')) {
    return;
  }
  const text = fs.readFileSync(file, 'utf8');
  const lines = text.split(/\r?\n/);
  lines.forEach((lineText, index) => {
    if (
      /\bValue\b/.test(lineText) &&
      /Attachment/.test(lineText) &&
      !/ContentBase64|_fileBase64|Value"\)|Value", "value"|ContentBase64", "content_base64"/.test(lineText)
    ) {
      issues.push(`${relative}:${index + 1} attachment Value leaked into active path`);
    }
  });
});

appFiles.forEach((file) => {
  const relative = rel(file);
  if (relative.includes('__pycache__')) {
    return;
  }
  const text = fs.readFileSync(file, 'utf8');
  const allowedContentBase64Owner =
    /backend\/mock_gateway\/api\/gateway_canonical_api\.py$/.test(relative) ||
    /backend\/mock_gateway\/tests\/test_attachment_upload_policy\.py$/.test(relative);
  if (/ContentBase64/.test(text) && !allowedContentBase64Owner) {
    issues.push(`${relative} uses ContentBase64 outside the gateway attachment upload boundary`);
  }
});

if (issues.length) {
  console.log(['FAIL attachment-contract-gate', ...issues.map((issue) => `- ${issue}`)].join('\n'));
  process.exit(1);
}

console.log('PASS attachment-contract-gate');
