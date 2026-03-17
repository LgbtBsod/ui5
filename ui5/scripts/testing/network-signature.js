#!/usr/bin/env node

const { URL } = require('url');

const NOISY_QUERY_KEYS = new Set([
  '_', 'ts', 'timestamp', 'cachebuster', 'cb', 'sap-cache-id', 'sap-client', 'sap-language',
  'sap-ui-language', 'sap-ui-debug', 'sap-ui-theme', 'sap-ui-appcachebuster', 'sap-ui-xx-viewcache',
  'sap-statistics', 'sap-contextid'
]);

function collapseWhitespace(sValue) {
  return String(sValue || '').replace(/\s+/g, ' ').trim();
}

function normalizeQuery(params) {
  const entries = [];
  Object.keys(params || {}).forEach((key) => {
    const value = params[key];
    if (value === undefined || value === null || value === '') {
      return;
    }
    const lowerKey = String(key).toLowerCase();
    if (NOISY_QUERY_KEYS.has(lowerKey) || lowerKey.startsWith('sap-') || lowerKey.startsWith('sap-ui-')) {
      return;
    }
    let normalizedValue = Array.isArray(value) ? value.join(',') : String(value);
    if (lowerKey === '$filter') {
      normalizedValue = collapseWhitespace(normalizedValue);
    }
    entries.push([key, normalizedValue]);
  });

  entries.sort((a, b) => {
    if (a[0] < b[0]) return -1;
    if (a[0] > b[0]) return 1;
    if (a[1] < b[1]) return -1;
    if (a[1] > b[1]) return 1;
    return 0;
  });

  return entries;
}

function parseQueryString(rawUrl) {
  const out = {};
  const query = String(rawUrl || '').split('?')[1] || '';
  if (!query) {
    return out;
  }
  query.split('&').forEach((chunk) => {
    if (!chunk) return;
    const [k, v] = chunk.split('=');
    const key = decodeURIComponent(k || '');
    const value = decodeURIComponent(v || '');
    if (Object.prototype.hasOwnProperty.call(out, key)) {
      if (Array.isArray(out[key])) {
        out[key].push(value);
      } else {
        out[key] = [out[key], value];
      }
      return;
    }
    out[key] = value;
  });
  return out;
}

function canonicalizeUrl(rawUrl) {
  const input = String(rawUrl || '');
  const hasProtocol = /^https?:\/\//i.test(input);
  let pathname = input;
  let queryObj = parseQueryString(input);

  if (hasProtocol) {
    const parsed = new URL(input);
    pathname = parsed.pathname || '/';
    queryObj = {};
    for (const [k, v] of parsed.searchParams.entries()) {
      if (Object.prototype.hasOwnProperty.call(queryObj, k)) {
        if (Array.isArray(queryObj[k])) {
          queryObj[k].push(v);
        } else {
          queryObj[k] = [queryObj[k], v];
        }
      } else {
        queryObj[k] = v;
      }
    }
  } else {
    pathname = input.split('?')[0] || '/';
  }

  const normalizedPath = pathname.startsWith('/') ? pathname : `/${pathname}`;
  const normalizedQuery = normalizeQuery(queryObj);
  if (!normalizedQuery.length) {
    return normalizedPath;
  }
  const query = normalizedQuery
    .map(([k, v]) => `${encodeURIComponent(k)}=${encodeURIComponent(v)}`)
    .join('&');
  return `${normalizedPath}?${query}`;
}

function normalizeBody(body) {
  if (!body) {
    return '';
  }
  return collapseWhitespace(String(body || ''));
}

function signature(request) {
  const method = String((request && request.method) || 'GET').toUpperCase();
  const url = canonicalizeUrl((request && request.url) || '');
  const body = normalizeBody((request && request.body) || (request && request.postData) || '');
  return `${method} ${url} ${body}`.trim();
}

function isBusinessRequest(pathOrUrl) {
  const s = String(pathOrUrl || '');
  if (!s) {
    return false;
  }
  if (/\/$metadata(\?|$)/i.test(s)) {
    return true;
  }
  if (/\/$batch(\?|$)/i.test(s)) {
    return true;
  }
  return /ChecklistSearchSet|ChecklistRootSet|ChecklistBasicInfoSet|ChecklistCheckSet|ChecklistBarrierSet|AttachmentSet/i.test(s);
}

module.exports = {
  canonicalizeUrl,
  signature,
  isBusinessRequest,
  normalizeQuery
};
