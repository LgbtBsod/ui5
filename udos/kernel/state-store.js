#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const cfg = require('./config');
const { readJsonSafe } = require('../lib/io-runtime');

function ensureDir(file) {
  const dir = path.dirname(file);
  if (!fs.existsSync(dir)) fs.mkdirSync(dir, { recursive: true });
}

function loadState() {
  ensureDir(cfg.stateFile);
  if (!fs.existsSync(cfg.stateFile)) {
    fs.writeFileSync(cfg.stateFile, JSON.stringify(cfg.defaults, null, 2) + '\n');
    return JSON.parse(JSON.stringify(cfg.defaults));
  }
  return readJsonSafe(cfg.stateFile, JSON.parse(JSON.stringify(cfg.defaults)));
}

function saveState(state) {
  ensureDir(cfg.stateFile);
  fs.writeFileSync(cfg.stateFile, JSON.stringify(state, null, 2) + '\n');
}

module.exports = { loadState, saveState };
