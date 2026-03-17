#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { listFiles } = require('./lib/fileWalker');

const ROOT = process.cwd();
const STYLES_DIR = path.join(ROOT, 'app', 'styles');

function readUtf8(filePath) {
  return fs.readFileSync(filePath, 'utf8');
}

function stripComments(text) {
  return text.replace(/\/\*[\s\S]*?\*\//g, '');
}

function shouldTrackClass(name) {
  return Boolean(name) && name.length > 1 && !name.startsWith('sap');
}

function extractSelectorClasses(cssText) {
  const classes = new Set();
  const cleaned = stripComments(cssText);
  const blockPreludeRegex = /([^{}]+)\{/g;
  let match;
  while ((match = blockPreludeRegex.exec(cleaned))) {
    const prelude = (match[1] || '').trim();
    if (!prelude || prelude.startsWith('@') || !prelude.includes('.')) {
      continue;
    }
    const classRegex = /\.([_a-zA-Z]+[\w-]*)/g;
    let cls;
    while ((cls = classRegex.exec(prelude))) {
      if (shouldTrackClass(cls[1])) {
        classes.add(cls[1]);
      }
    }
  }
  return classes;
}

function extractUsedClassesFromText(text, classes) {
  const classAttrRegex = /\bclass\s*=\s*["']([^"']+)["']/g;
  let attr;
  while ((attr = classAttrRegex.exec(text))) {
    String(attr[1] || '')
      .split(/\s+/)
      .map((entry) => entry.trim())
      .filter(Boolean)
      .forEach((entry) => {
        if (shouldTrackClass(entry)) {
          classes.add(entry);
        }
      });
  }

  const styleClassRegex = /\b(?:addStyleClass|removeStyleClass|toggleStyleClass)\s*\(\s*["']([^"']+)["']/g;
  let styleClass;
  while ((styleClass = styleClassRegex.exec(text))) {
    const value = String(styleClass[1] || '').trim();
    if (shouldTrackClass(value)) classes.add(value);
  }

  const classListRegex = /\bclassList\.(?:add|remove|toggle)\s*\(([^)]*)\)/g;
  let listCall;
  while ((listCall = classListRegex.exec(text))) {
    const args = String(listCall[1] || '');
    const argRegex = /["']([^"']+)["']/g;
    let arg;
    while ((arg = argRegex.exec(args))) {
      const value = String(arg[1] || '').trim();
      if (!value) continue;
      value
        .split(/\s+/)
        .map((entry) => entry.trim())
        .filter(Boolean)
        .forEach((entry) => {
          if (shouldTrackClass(entry)) {
            classes.add(entry);
          }
        });
    }
  }
}

function extractUsedClasses(rootDir) {
  const classes = new Set();
  const files = listFiles(rootDir, {
    include: ['app/**/*.xml', 'app/**/*.js', 'app/**/*.json', 'app/**/*.html']
  });
  files.forEach((relPath) => {
    const absPath = path.join(rootDir, relPath);
    const text = readUtf8(absPath);
    extractUsedClassesFromText(text, classes);
  });
  return classes;
}

function writeList(fileName, list) {
  const outPath = path.join(ROOT, fileName);
  const body = list.length ? `${list.join('\n')}\n` : '';
  fs.writeFileSync(outPath, body, 'utf8');
}

function main() {
  if (!fs.existsSync(STYLES_DIR)) {
    console.error(`Missing styles directory: ${STYLES_DIR}`);
    process.exit(1);
  }
  const moduleClasses = new Set();
  listFiles(ROOT, {
    include: ['app/styles/**/*.css']
  }).forEach((relPath) => {
    const cssPath = path.join(ROOT, relPath);
    const classes = extractSelectorClasses(readUtf8(cssPath));
    classes.forEach((cls) => moduleClasses.add(cls));
  });

  const usedClasses = extractUsedClasses(ROOT);

  const moduleList = [...moduleClasses].sort();
  const usedList = [...usedClasses].sort();

  writeList('tmp-module-classes.txt', moduleList);
  writeList('tmp-used-classes.txt', usedList);

  const report = {
    generatedAt: new Date().toISOString(),
    counts: {
      modules: moduleList.length,
      used: usedList.length
    }
  };

  process.stdout.write(`${JSON.stringify(report, null, 2)}\n`);
}

main();
