const fs = require('fs');

function resolveExistingPath(candidates) {
  return candidates.find((file) => fs.existsSync(file)) || candidates[0];
}

async function runFeedbackSmoke() {
  const contractFiles = [
    ['app/service/framework/FeedbackPolicy.js'],
    ['app/service/framework/execution/EffectApplier.js', 'app/service/framework/EffectApplier.js'],
    ['app/service/framework/execution/EffectFeedbackRuntime.js', 'app/service/framework/EffectFeedbackRuntime.js'],
    ['app/service/framework/EffectRuntime.js']
  ];

  return contractFiles.map((candidates) => {
    const file = resolveExistingPath(candidates);
    return {
      name: `feedback:${file}`,
      ok: fs.existsSync(file),
      detail: 'feedback/effect runtime contract file exists'
    };
  });
}

module.exports = { runFeedbackSmoke };
