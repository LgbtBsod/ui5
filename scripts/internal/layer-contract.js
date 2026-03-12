#!/usr/bin/env node

const path = require('path');
const { normalizePath, fileExists } = require('../qa-shared');

const LAYER_NAMES = {
  controller: 'Controllers',
  facade: 'Facades',
  usecase: 'Usecases',
  backend: 'Backend',
  infra: 'Infra',
  util: 'Util',
  other: 'Other'
};
const PREFIX_LAYER_TABLE = 'controller/:controller,facades/:facade,service/backend/:backend,service/domain/:usecase,service/framework/:util,service/:usecase,infra/:infra,util/:util,model/:util';
const PREFIX_ORDER = PREFIX_LAYER_TABLE.split(',').map((entry) => entry.split(':')[0]);
const LAYER_BY_PREFIX = PREFIX_LAYER_TABLE.split(',').reduce((acc, entry) => {
  const parts = entry.split(':');
  acc[parts[0]] = parts[1];
  return acc;
}, {});

function canonicalLayer(modulePath) {
  const p = normalizePath(String(modulePath || ''));
  const prefix = PREFIX_ORDER.find((item) => p.startsWith(item));
  if (prefix) return LAYER_BY_PREFIX[prefix];
  return 'other';
}

function toLayerName(layerTag) {
  return LAYER_NAMES[layerTag] || LAYER_NAMES.other;
}

function normalizeRuntimeRelative(runtimeRoot, relPath) {
  const p = normalizePath(String(relPath || ''));
  const root = normalizePath(String(runtimeRoot || '.'));
  if (root === '.') return p;
  const prefix = `${root}/`;
  return p.startsWith(prefix) ? p.slice(prefix.length) : p;
}

function stripKnownPrefix(dep) {
  const value = String(dep || '').trim().replace(/^\//, '');
  if (value.startsWith('PRODUCTION_CONTROL_CHECKLIST/')) return value.slice('PRODUCTION_CONTROL_CHECKLIST/'.length);
  if (value.startsWith('app/')) return value.slice('app/'.length);
  if (value.startsWith('webapp/')) return value.slice('webapp/'.length);
  return value;
}

function resolveLocalModulePath(fromFile, dep) {
  if (dep.startsWith('./') || dep.startsWith('../')) {
    return normalizePath(path.join(path.dirname(fromFile), dep));
  }
  return normalizePath(stripKnownPrefix(dep));
}

function resolveDependency(rootDir, fromFile, dep, runtimeRoot) {
  const from = normalizePath(String(fromFile || ''));
  const value = String(dep || '');
  const runtime = runtimeRoot || '.';

  if (!value) return { kind: 'external', modulePath: value, filePath: null, layerTag: 'other', layerName: 'External' };
  if (value.startsWith('sap/')) return { kind: 'ui5', modulePath: value, filePath: null, layerTag: 'other', layerName: 'UI5' };

  const modulePath = resolveLocalModulePath(from, value).replace(/\.js$/, '');
  const relFile = `${modulePath}.js`;
  if (!fileExists(rootDir, relFile)) {
    return { kind: 'external', modulePath, filePath: null, layerTag: 'other', layerName: 'External' };
  }

  const normalizedModule = normalizeRuntimeRelative(runtime, modulePath);
  const layerTag = canonicalLayer(normalizedModule);
  return {
    kind: 'local',
    modulePath: normalizedModule,
    filePath: relFile,
    layerTag,
    layerName: toLayerName(layerTag)
  };
}

module.exports = {
  canonicalLayer,
  toLayerName,
  normalizeRuntimeRelative,
  resolveDependency
};
