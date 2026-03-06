#!/usr/bin/env node
const { loadRules } = require('./rules-runtime');
const rules = loadRules('pricing-rules.json');

function computeBaseCost(ctx){
  const files = ctx.files||[];
  const loc = Number(ctx.locChanged||0);
  let base = files.length*rules.fileCost + loc*rules.locCost;
  const low = files.join(' ').toLowerCase();
  if(/service\/domain\/lock|lock/.test(low)) base += rules.lockTouch;
  if(/autosave/.test(low)) base += rules.autosaveTouch;
  if(/cache/.test(low)) base += rules.cacheTouch;
  if((ctx.touchedStatePaths||[]).some(p=>['/mode','/lockOperationState'].includes(p))) base += rules.statePathTouch;
  base += Number(ctx.newLayerEdges||0)*rules.layerRiskEdge;
  return Number(base.toFixed(2));
}

module.exports={computeBaseCost,rules};
