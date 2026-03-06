#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

function normalizePath(filePath) {
  return filePath.replace(/\\/g, '/');
}


function detectRuntimeRoot(rootDir) {
  const webappPath = resolveFromRoot(rootDir, 'webapp');
  const webappManifest = resolveFromRoot(rootDir, 'webapp/manifest.json');
  const webappComponent = resolveFromRoot(rootDir, 'webapp/Component.js');
  if (fs.existsSync(webappPath) && fs.statSync(webappPath).isDirectory() && (fs.existsSync(webappManifest) || fs.existsSync(webappComponent))) {
    return 'webapp';
  }
  return '.';
}

function countFileLines(rootDir, relPath) {
  const content = readText(rootDir, relPath);
  return content.split(/\r?\n/).length;
}

function resolveFromRoot(rootDir, relPath) {
  return path.join(rootDir, relPath);
}

function fileExists(rootDir, relPath) {
  return fs.existsSync(resolveFromRoot(rootDir, relPath));
}

function readText(rootDir, relPath) {
  return fs.readFileSync(resolveFromRoot(rootDir, relPath), 'utf8');
}

function collectFilesRecursively(rootDir, relDir, matcher, out) {
  if (!fileExists(rootDir, relDir)) {
    return;
  }

  const absDir = resolveFromRoot(rootDir, relDir);
  for (const entry of fs.readdirSync(absDir, { withFileTypes: true })) {
    if (entry.name === '.git' || entry.name === 'node_modules' || entry.name === 'scripts') {
      continue;
    }

    const relPath = normalizePath(path.join(relDir, entry.name));
    if (entry.isDirectory()) {
      collectFilesRecursively(rootDir, relPath, matcher, out);
      continue;
    }

    if (!matcher || matcher(relPath, entry)) {
      out.push(relPath);
    }
  }
}


function collectFilesByExtensions(rootDir, relDirs, extensions) {
  const dirs = Array.isArray(relDirs) ? relDirs : [];
  const exts = new Set((Array.isArray(extensions) ? extensions : []).map((ext) => String(ext || '').toLowerCase()));
  const out = [];

  for (const relDir of dirs) {
    collectFilesRecursively(rootDir, relDir, (relPath) => {
      const lower = String(relPath || '').toLowerCase();
      for (const ext of exts) {
        if (lower.endsWith(ext)) {
          return true;
        }
      }
      return false;
    }, out);
  }

  return [...new Set(out)].sort();
}

function runNodeScriptAsCheck(rootDir, scriptRelPath, fallbackName) {
  const result = runNodeScript(rootDir, scriptRelPath);
  if (result.error || result.status !== 0) {
    const fallback = fallbackName || scriptRelPath;
    return { ok: false, detail: result.output || (result.error && result.error.message) || `${fallback} failed` };
  }
  return { ok: true, detail: (result.output || 'PASS').trim() };
}

function lineFromIndex(source, index) {
  if (index == null || index < 0) return null;
  return source.slice(0, index).split('\n').length;
}

function extractUi5Dependencies(source) {
  const deps = [];
  const blocks = [];

  const defineRe = /sap\.ui\.define\s*\(\s*\[([\s\S]*?)\]\s*,/g;
  let defineMatch;
  while ((defineMatch = defineRe.exec(source))) {
    blocks.push({ body: defineMatch[1], start: defineMatch.index });
  }

  const requireRe = /require\s*\(\s*\[([\s\S]*?)\]\s*[,\)]/g;
  let requireMatch;
  while ((requireMatch = requireRe.exec(source))) {
    blocks.push({ body: requireMatch[1], start: requireMatch.index });
  }

  for (const block of blocks) {
    const depRe = /["']([^"']+)["']/g;
    let depMatch;
    while ((depMatch = depRe.exec(block.body))) {
      deps.push({ dep: depMatch[1], index: block.start + depMatch.index });
    }
  }

  return deps;
}


function runNodeScript(rootDir, scriptRelPath, args) {
  const cp = require('child_process');
  const scriptArgs = Array.isArray(args) ? args : [];
  const result = cp.spawnSync(process.execPath, [scriptRelPath, ...scriptArgs], {
    cwd: rootDir,
    encoding: 'utf8'
  });

  const stdout = result.stdout || '';
  const stderr = result.stderr || '';
  const output = `${stdout}${stderr}`.trim();

  return {
    status: result.status == null ? 1 : result.status,
    output,
    error: result.error || null
  };
}


function createCheckAggregator() {
  const results = [];

  function check(name, fn) {
    try {
      const result = fn() || {};
      results.push({ name, ok: !!result.ok, detail: result.detail || '' });
    } catch (error) {
      results.push({ name, ok: false, detail: error.message });
    }
  }

  function printReport(title) {
    if (title) {
      console.log(title);
    }
    for (const result of results) {
      console.log(`- ${result.ok ? 'PASS' : 'FAIL'}: ${result.name}${result.detail ? ` :: ${result.detail}` : ''}`);
    }
    return results.filter((result) => !result.ok);
  }

  return { check, results, printReport };
}

function scanRegexInFiles(rootDir, files, regex, onMatch) {
  for (const file of files) {
    const source = readText(rootDir, file);
    const flags = regex.flags.includes('g') ? regex.flags : `${regex.flags}g`;
    const re = new RegExp(regex.source, flags);

    let match = re.exec(source);
    while (match) {
      onMatch(file, source, match, lineFromIndex(source, match.index));
      match = re.exec(source);
    }
  }
}

module.exports = {
  normalizePath,
  detectRuntimeRoot,
  countFileLines,
  resolveFromRoot,
  fileExists,
  readText,
  collectFilesRecursively,
  collectFilesByExtensions,
  lineFromIndex,
  extractUi5Dependencies,
  scanRegexInFiles,
  runNodeScript,
  runNodeScriptAsCheck,
  createCheckAggregator
};
