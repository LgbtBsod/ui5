const fs = require('fs');
const path = require('path');
const { signature, isBusinessRequest } = require('../test-pack/network-signature');
const { readJsonSafe } = require('./auditInput');

const RUNTIME_TRACE_FILE_NAMES = [
  'network-trace.runtime.json',
  'network-trace-real.json',
  'network-trace.json'
];

function parseRuntimeTraceFile(filePath) {
  const raw = readJsonSafe(filePath, null);
  if (!raw) {
    return [];
  }
  if (Array.isArray(raw)) {
    return raw;
  }
  if (Array.isArray(raw.trace)) {
    return raw.trace;
  }
  if (Array.isArray(raw.requests)) {
    return raw.requests;
  }
  if (raw && Array.isArray(raw.events)) {
    return raw.events;
  }
  return [];
}

function detectTraceSource(docsDir) {
  const candidates = RUNTIME_TRACE_FILE_NAMES.map((name) => path.join(docsDir, name));
  for (const candidate of candidates) {
    if (!fs.existsSync(candidate)) {
      continue;
    }
    try {
      const parsed = parseRuntimeTraceFile(candidate);
      const hasRuntimeFlag = /runtime|browser/i.test(path.basename(candidate));
      const hasRequestLike = parsed.some((e) => e && (e.url || e.path));
      if (hasRuntimeFlag || hasRequestLike) {
        return { path: candidate, trace: parsed };
      }
    } catch (_e) {
      // ignore and continue
    }
  }
  return null;
}

function normalizeTraceEvent(event, index) {
  const method = String(event.method || event.httpMethod || 'GET').toUpperCase();
  const url = String(event.url || event.path || '');
  const body = String(event.body || event.postData || event.requestBody || '');
  const ts = Number(event.ts || event.time || event.timestamp || index * 10);
  const phase = String(event.phase || '').trim();
  return { method, url, postData: body, ts, phase, raw: event };
}

function inferPhase(event) {
  const blob = `${event.url} ${event.postData || ''}`;
  if (/HasFailedChecks\s+eq\s+true/i.test(blob)) return 'segment-checks-failed';
  if (/HasFailedChecks\s+eq\s+false/i.test(blob)) return 'segment-checks-success';
  if (/HasFailedBarriers\s+eq\s+true/i.test(blob)) return 'segment-barriers-failed';
  if (/HasFailedBarriers\s+eq\s+false/i.test(blob)) return 'segment-barriers-success';
  if (/__create/i.test(blob)) return 'create-open';
  if (/ChecklistRootSet|ChecklistBasicInfoSet|ChecklistCheckSet|ChecklistBarrierSet|AttachmentSet/i.test(blob)) return 'detail-open';
  if (/\$metadata/i.test(blob)) return 'boot';
  return 'search-initial';
}

function isMetadataRequest(evt) {
  return /\/\$metadata(\?|$)/i.test(evt.url || '');
}

function isTokenRequest(evt) {
  const blob = `${evt.url} ${evt.postData || ''}`;
  return /csrf|x-csrf-token|refreshsecuritytoken/i.test(blob);
}

function detectDuplicates(events) {
  const warnings = [];
  const violations = [];
  const seenByPhase = new Map();

  events.forEach((evt) => {
    const phase = evt.phase || inferPhase(evt);
    const sig = signature({ method: evt.method, url: evt.url, body: evt.postData });
    if (!isBusinessRequest(evt.url) && !/\$batch|\$metadata/i.test(evt.url || '')) {
      return;
    }

    if (!seenByPhase.has(phase)) {
      seenByPhase.set(phase, new Map());
    }
    const phaseMap = seenByPhase.get(phase);

    if (isMetadataRequest(evt)) {
      const prevMeta = phaseMap.get(sig) || [];
      prevMeta.push(evt);
      phaseMap.set(sig, prevMeta);
      if (prevMeta.length > 2) {
        warnings.push({ phase, signature: sig, deltaMs: evt.ts - prevMeta[prevMeta.length - 2].ts, sampleUrl: evt.url, reason: 'metadata_repeated_more_than_twice' });
      }
      return;
    }

    if (isTokenRequest(evt)) {
      return;
    }

    const previous = phaseMap.get(sig) || [];
    if (previous.length) {
      const prev = previous[previous.length - 1];
      violations.push({ phase, signature: sig, deltaMs: evt.ts - prev.ts, sampleUrl: evt.url });
    }
    previous.push(evt);
    phaseMap.set(sig, previous);
  });

  return { warnings, violations };
}

module.exports = {
  detectDuplicates,
  detectTraceSource,
  inferPhase,
  normalizeTraceEvent
};
