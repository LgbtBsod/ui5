#!/usr/bin/env node

const qa = require('./qa-shared');
const { exitWithColonIssues } = require('./lib/issueGateRuntime');

const root = process.cwd();
const files = qa.collectFilesByExtensions(root, ['controller'], ['.js']);

const ENTRY_FORBIDDEN = ['onInit', '_onMatched', '_onRouteMatched', 'onAfterRendering'];
const INTERACTION_HINTS = ['suggest', 'liveChange', 'focusin', 'valueHelpRequest'];

function findFunctionBlocks(source) {
  const blocks = [];
  const fnRegex = /([A-Za-z0-9_]+)\s*:\s*function\s*\([^)]*\)\s*\{/g;
  let match = fnRegex.exec(source);
  while (match) {
    const name = match[1];
    const start = match.index;
    let i = fnRegex.lastIndex;
    let depth = 1;
    while (i < source.length && depth > 0) {
      const ch = source[i];
      if (ch === '{') depth += 1;
      if (ch === '}') depth -= 1;
      i += 1;
    }
    blocks.push({ name, body: source.slice(match.index, i) });
    match = fnRegex.exec(source);
  }
  return blocks;
}

const violations = [];

for (const file of files) {
  const src = qa.readText(root, file);
  const blocks = findFunctionBlocks(src);

  blocks.forEach((block) => {
    const hasSuggestCall = /\.(suggestPerson|suggest)\s*\(/.test(block.body) || /SuggestPersonUseCase/.test(block.body);
    if (!hasSuggestCall) return;

    if (ENTRY_FORBIDDEN.includes(block.name)) {
      violations.push(`${file}: ${block.name} must not trigger suggest usecases/facades`);
      return;
    }

    const lowerName = block.name.toLowerCase();
    const interactionAllowed = INTERACTION_HINTS.some((hint) => lowerName.includes(hint.toLowerCase()));
    if (!interactionAllowed) {
      violations.push(`${file}: ${block.name} suggest call is outside interaction handlers`);
    }
  });
}

exitWithColonIssues(
  'suggest-on-interaction-only-gate',
  violations,
  {
    filesScanned: files.length,
    policy: 'Suggest usecases/facades may run only from interaction handlers (suggest/liveChange/focusin/valueHelpRequest).'
  },
  { asJson: process.argv.includes('--json') }
);
