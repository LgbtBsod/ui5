const fs = require('fs');
const path = require('path');

function detectRuntimeRoot(root) {
  for (const candidate of ['app', 'webapp']) {
    const abs = path.join(root, candidate);
    if (fs.existsSync(path.join(abs, 'manifest.json')) || fs.existsSync(path.join(abs, 'Component.js'))) {
      return candidate;
    }
  }
  return '.';
}

const DEFAULT_EXCLUDES = [
  'backend/sap_backend/**',
  'sap_backend/**',
  'node_modules/**',
  'dist/**',
  'coverage/**',
  '.git/**',
  '.venv/**',
  '**/.venv/**',
  'backend/mock_gateway/.venv/**',
  'backend/mock_gateway/uploads/**',
  'mock_gate_way/.venv/**',
  'mock_gate_way/uploads/**'
];

function normalize(value) {
  return value.replace(/\\/g, '/').replace(/^\//, '');
}

function patternToRegex(pattern) {
  const normalized = normalize(pattern);
  const tokens = normalized.split('**').map((segment) => segment.replace(/[.+^${}()|[\]\\]/g, '\\$&').replace(/\*/g, '[^/]*'));
  return new RegExp(`^${tokens.join('.*')}$`);
}

function compilePatterns(patterns) {
  return (patterns || []).map((pattern) => ({ regex: patternToRegex(pattern) }));
}

function matchAny(value, compiled) {
  return compiled.some((entry) => entry.regex.test(value));
}

function listFiles(root, options = {}) {
  const runtimeRoot = detectRuntimeRoot(root);
  const requestedInclude = options.include && options.include.length ? options.include : ['**/*.js'];
  const includePatterns = [];
  requestedInclude.forEach((pattern) => {
    includePatterns.push(pattern);
    if (
      runtimeRoot !== '.'
      && !pattern.startsWith(`${runtimeRoot}/`)
      && !pattern.startsWith('scripts/')
      && !pattern.startsWith('docs/')
      && !pattern.startsWith('backend/')
      && !pattern.startsWith('architecture/')
      && !pattern.startsWith('udos/')
    ) {
      includePatterns.push(`${runtimeRoot}/${pattern}`);
    }
  });
  const include = compilePatterns(includePatterns);
  const exclude = compilePatterns([...(options.exclude || []), ...DEFAULT_EXCLUDES]);
  const out = [];

  function walk(relDir) {
    const absDir = path.join(root, relDir);
    if (!fs.existsSync(absDir)) return;
    fs.readdirSync(absDir, { withFileTypes: true }).forEach((entry) => {
      const relPath = normalize(path.join(relDir, entry.name));
      if (matchAny(relPath, exclude) || matchAny(`${relPath}/`, exclude)) return;
      if (entry.isDirectory()) return walk(relPath);
      if (entry.isFile() && matchAny(relPath, include)) out.push(relPath);
    });
  }

  walk('');
  return out.sort();
}

module.exports = { listFiles, normalize };
