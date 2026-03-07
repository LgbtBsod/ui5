#!/usr/bin/env node

const { runGate } = require('./lib/gateRunner');
const { extractFunctions } = require('./lib/functionExtract');

const SOFT = 60;
const HARD = 80;

function isModuleWrapper(fn, source) {
  if (!fn || fn.name !== 'anonymous') return false;
  if (Number(fn.startLine || 0) > 30) return false;
  return /sap\.ui\.define\s*\(/.test(source) || /module\.exports\s*=/.test(source);
}

runGate({
  name: 'function-length-gate',
  advisory: true,
  include: [
    'controller/**/*.js', 'service/**/*.js', 'infra/**/*.js', 'ports/**/*.js',
    'manager/**/*.js', 'model/**/*.js', 'util/**/*.js', 'scripts/**/*.js'
  ],
  check: ({ file, text }) => extractFunctions(text)
    .filter((fn) => !isModuleWrapper(fn, text))
    .filter((fn) => fn.length > SOFT)
    .map((fn) => ({
      file,
      line: fn.startLine,
      message: `${fn.name} length ${fn.length} > ${SOFT}${fn.length > HARD ? ' (HARD FAIL >80)' : ''}`
    }))
});
