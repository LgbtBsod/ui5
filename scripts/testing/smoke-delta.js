const fs = require('fs');

async function runDeltaSmoke() {
  const file = 'app/service/shared/DeltaPayloadBuilder.js';
  return [{
    name: 'delta:payload-builder-exists',
    ok: fs.existsSync(file),
    detail: 'canonical delta payload builder module exists'
  }];
}

module.exports = { runDeltaSmoke };
