#!/usr/bin/env node
const fs = require('fs');

function read(p) { return fs.readFileSync(p, 'utf8'); }
function fail(msg) { console.error('ux-behavior-sla-gate FAIL'); console.error(msg); process.exit(1); }

const detailRuntime = read('app/controller/support/DetailChecklistRuntime.js');
const attachmentDelete = read('app/service/domain/detail/usecases/AttachmentDeleteUseCase.js');
const attachmentIdentity = read('app/service/domain/detail/AttachmentIdentity.js');
const componentInit = read('app/service/framework/ComponentInitRuntime.js');

const detailPolicyDefaults = read('app/service/framework/behavior/DetailRuntimeDefaultHandlers.js');

const hasLegacyRetryHardening = /iAttempts\s*>=\s*3/.test(detailRuntime) && /setTimeout\s*\([\s\S]*220\)/.test(detailRuntime);
const hasPolicyRetryHardening = /analyticsEditRestorePlan/.test(detailRuntime)
  && /oRestorePlan\.maxAttempts/.test(detailRuntime)
  && /oRestorePlan\.retryDelayMs/.test(detailRuntime)
  && /maxAttempts:\s*3/.test(detailPolicyDefaults)
  && /retryDelayMs:\s*220/.test(detailPolicyDefaults);

if (!hasLegacyRetryHardening && !hasPolicyRetryHardening) {
  fail('analytics return edit-restore retry SLA is not hardened (expected >=3 attempts and delayed retry)');
}
if (!/removeByAttachment\(/.test(attachmentDelete) || !/ViewPathContracts\.SESSION_ATTACHMENTS/.test(attachmentDelete)) {
  fail('attachment delete SLA missing robust session/all attachment synchronization');
}
if (!/resolveDeletionId/.test(attachmentIdentity) || !/removeByAttachment/.test(attachmentIdentity)) {
  fail('attachment identity SLA missing robust delete identity resolution');
}
if (!/setGlobalBanner/.test(componentInit) || !/clearGlobalBanner/.test(componentInit)) {
  fail('component init SLA missing unified banner policy usage');
}

console.log('ux-behavior-sla-gate PASS');
