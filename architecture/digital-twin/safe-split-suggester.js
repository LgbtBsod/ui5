#!/usr/bin/env node

function suggestSplit(patch, risk) {
  const files = patch.files || [];
  const low = files.filter((f) => /docs\//.test(f) || /manifest\.json$/.test(f) || /i18n\//.test(f));
  const med = files.filter((f) => /helper|mixin|statepaths|usecases|controller\//i.test(f) && !low.includes(f));
  const high = files.filter((f) => /lock|autosave|cache/i.test(f) && !low.includes(f) && !med.includes(f));

  return {
    currentRisk: risk.riskLevel,
    batches: [
      { order: 1, level: 'LOW', files: low, note: 'imports/statepaths/docs hygiene' },
      { order: 2, level: 'MED', files: med, note: 'helpers extraction and wiring' },
      { order: 3, level: 'HIGH', files: high, note: 'lock/autosave/cache only with proof mode + manual review' }
    ]
  };
}

module.exports = { suggestSplit };
