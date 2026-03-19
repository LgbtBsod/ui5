const fs = require('fs');

async function runFeedbackSmoke() {
  const files = [
    'app/service/framework/FeedbackPolicy.js',
    'app/service/framework/EffectApplier.js',
    'app/service/framework/EffectFeedbackRuntime.js',
    'app/service/framework/EffectActionRouting.js'
  ];
  return files.map((file) => ({ name: `feedback:${file}`, ok: fs.existsSync(file), detail: 'feedback/effect runtime contract file exists' }));
}

module.exports = { runFeedbackSmoke };
