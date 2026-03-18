const cp = require('child_process');
const fs = require('fs');
const path = require('path');
const crypto = require('crypto');
const { readJsonSafe } = require('./auditInput');

const ROOT = path.resolve(__dirname, '..', '..');
const TRACKER_DIR = path.join(ROOT, 'docs', 'artifacts');
const TRACKER_FILE = path.join(TRACKER_DIR, 'gitless-change-tracker.json');
const EXCLUDE_DIRS = new Set([
  '.git',
  'node_modules',
  'dist',
  'coverage',
  '__pycache__',
  '.pytest_cache'
]);
const EXCLUDE_EXTS = new Set([
  '.png', '.jpg', '.jpeg', '.gif', '.svg', '.woff', '.woff2', '.ttf', '.ico', '.db', '.sqlite', '.pyc'
]);

function runGit(args) {
  const result = cp.spawnSync('git', args, { encoding: 'utf8', cwd: ROOT });
  if (result.status !== 0) return [];
  return String(result.stdout || '').split(/\r?\n/).map((v) => v.trim()).filter(Boolean);
}

function hasGitRepo() {
  const result = cp.spawnSync('git', ['rev-parse', '--is-inside-work-tree'], { encoding: 'utf8', cwd: ROOT });
  return result.status === 0 && String(result.stdout || '').trim() === 'true';
}

function shouldSkip(relPath) {
  const normalized = String(relPath).replace(/\\/g, '/');
  if (!normalized || normalized.startsWith('docs/artifacts/')) return true;
  if (normalized.split('/').some((part) => EXCLUDE_DIRS.has(part))) return true;
  return EXCLUDE_EXTS.has(path.extname(normalized).toLowerCase());
}

function walkFiles(dir, acc) {
  const entries = fs.readdirSync(dir, { withFileTypes: true });
  for (const entry of entries) {
    const full = path.join(dir, entry.name);
    const rel = path.relative(ROOT, full).replace(/\\/g, '/');
    if (entry.isDirectory()) {
      if (!shouldSkip(rel)) walkFiles(full, acc);
      continue;
    }
    if (!shouldSkip(rel)) acc.push(rel);
  }
}

function sha1File(file) {
  const hash = crypto.createHash('sha1');
  hash.update(fs.readFileSync(path.join(ROOT, file)));
  return hash.digest('hex');
}

function loadTracker() {
  if (!fs.existsSync(TRACKER_FILE)) return { files: {}, generatedAt: null, source: 'gitless' };
  const parsed = readJsonSafe(TRACKER_FILE, null);
  return parsed && parsed.files ? parsed : { files: {}, generatedAt: null, source: 'gitless' };
}

function saveTracker(snapshot) {
  fs.mkdirSync(TRACKER_DIR, { recursive: true });
  fs.writeFileSync(TRACKER_FILE, JSON.stringify(snapshot, null, 2) + '\n', 'utf8');
}

function getChangedFilesWithoutGit() {
  const previous = loadTracker();
  const allFiles = [];
  walkFiles(ROOT, allFiles);
  const nextFiles = {};
  const changed = [];

  for (const file of allFiles.sort()) {
    const stat = fs.statSync(path.join(ROOT, file));
    const fingerprint = {
      size: stat.size,
      mtimeMs: Math.trunc(stat.mtimeMs),
      sha1: sha1File(file)
    };
    nextFiles[file] = fingerprint;
    const prev = previous.files[file];
    if (!prev || prev.size !== fingerprint.size || prev.mtimeMs !== fingerprint.mtimeMs || prev.sha1 !== fingerprint.sha1) {
      changed.push(file);
    }
  }

  for (const oldFile of Object.keys(previous.files || {})) {
    if (!nextFiles[oldFile]) changed.push(oldFile);
  }

  saveTracker({
    generatedAt: new Date().toISOString(),
    source: 'gitless',
    files: nextFiles
  });

  return [...new Set(changed)].sort();
}

function getChangedFiles() {
  if (hasGitRepo()) {
    const staged = runGit(['diff', '--name-only', '--cached']);
    const unstaged = runGit(['diff', '--name-only']);
    return [...new Set([...staged, ...unstaged])].sort();
  }
  return getChangedFilesWithoutGit();
}

module.exports = { getChangedFiles, getChangedFilesWithoutGit, hasGitRepo, TRACKER_FILE };
