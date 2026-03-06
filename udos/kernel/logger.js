#!/usr/bin/env node
function log(level, msg, data) {
  const payload = data ? ` ${JSON.stringify(data)}` : '';
  console.log(`[UDOS][${level}] ${msg}${payload}`);
}

module.exports = {
  info: (m, d) => log('INFO', m, d),
  warn: (m, d) => log('WARN', m, d),
  error: (m, d) => log('ERROR', m, d)
};
