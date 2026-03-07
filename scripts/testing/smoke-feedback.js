const fs = require('fs');

async function runFeedbackSmoke() {
  const files = [
    'service/framework/FeedbackPolicy.js',
    'service/framework/EffectApplier.js',
    'service/framework/EffectUiHandlers.js',
    'service/framework/EffectActionRouting.js'
  ];
  return files.map((file) => ({ name: `feedback:${file}`, ok: fs.existsSync(file), detail: 'feedback/effect runtime contract file exists' }));
}

module.exports = { runFeedbackSmoke };
