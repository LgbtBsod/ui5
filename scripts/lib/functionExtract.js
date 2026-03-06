function createBlock(name, kind, start, end, lines) {
  return { name, kind, startLine: start + 1, endLine: end + 1, length: end - start + 1, text: lines.slice(start, end + 1).join('\n') };
}

function findFunctionStart(line) {
  const checks = [
    { kind: 'function', regex: /function\s+([\w$]+)?\s*\(/ },
    { kind: 'arrow', regex: /(const|let|var)\s+([\w$]+)\s*=\s*(async\s*)?(\([^)]*\)|[\w$]+)\s*=>\s*\{/ },
    { kind: 'method', regex: /^\s*(async\s+)?([\w$]+)\s*\([^)]*\)\s*\{/ }
  ];
  for (const check of checks) {
    const match = line.match(check.regex);
    if (match) {
      const name = match[2] || match[1] || 'anonymous';
      return { kind: check.kind, name };
    }
  }
  return null;
}


function isUi5ModuleCallback(lines, start) {
  for (let i = start; i >= Math.max(0, start - 12); i -= 1) {
    if (/sap\.ui\.define\s*\(/.test(lines[i] || '')) return true;
  }
  return false;
}

function extractFunctions(source) {
  const lines = source.split('\n');
  const blocks = [];
  let active = null;
  let depth = 0;

  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    const clean = line.replace(/"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*'/g, '');
    if (!active) {
      const found = findFunctionStart(clean);
      if (!found) continue;
      active = { ...found, start: i };
      depth = (clean.match(/\{/g) || []).length - (clean.match(/\}/g) || []).length;
      if (depth <= 0) {
        blocks.push(createBlock(active.name, active.kind, i, i, lines));
        active = null;
      }
      continue;
    }

    depth += (clean.match(/\{/g) || []).length - (clean.match(/\}/g) || []).length;
    if (depth <= 0) {
      if (!(active.name === 'anonymous' && isUi5ModuleCallback(lines, active.start))) {
        blocks.push(createBlock(active.name, active.kind, active.start, i, lines));
      }
      active = null;
      depth = 0;
    }
  }
  return blocks;
}

module.exports = { extractFunctions };
