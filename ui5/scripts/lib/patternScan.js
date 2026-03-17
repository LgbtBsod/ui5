function scanPatterns(file, text, specs, options = {}) {
  const violations = [];
  const lines = text.split('\n');
  const allow = options.allowlist || [];

  function isAllowed(line, spec) {
    return allow.some((entry) => {
      if (typeof entry === 'function') return entry({ file, line, spec });
      return entry.file === file && entry.line === line;
    });
  }

  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    for (const spec of specs) {
      const regex = spec.regex instanceof RegExp ? spec.regex : new RegExp(spec.regex);
      if (!regex.test(line)) continue;
      const lineNo = i + 1;
      if (isAllowed(lineNo, spec.id)) continue;
      violations.push({ file, line: lineNo, rule: spec.id, message: spec.message, snippet: line.trim() });
    }
  }

  return violations;
}

module.exports = { scanPatterns };
