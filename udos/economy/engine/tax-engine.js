#!/usr/bin/env node
const { loadRules } = require('./rules-runtime');
const rules = loadRules('tax-rules.json');

function computeRiskPremium(baseCost, ctx){
  const risk = String(ctx.riskLevel||'LOW').toUpperCase();
  let p = baseCost*(rules.riskMultipliers[risk]||0);
  if(Number(ctx.historicalFailRate||0)>rules.historicalFailRateThreshold) p += baseCost*rules.historicalFailSurcharge;
  if(ctx.exclusiveWindowRequired) p += baseCost*rules.exclusiveWindowSurcharge;
  return Number(p.toFixed(2));
}

function computeDebtTax(ctx){
  let t=0;
  const dup=Number(ctx.duplicationDelta||0);
  if(dup>0) t += dup*rules.duplicationDeltaTax;
  if(ctx.controllerThresholdExceeded) t += rules.controllerThresholdTax;
  if(Number(ctx.layerViolations||0)>0) t += rules.layerViolationTax*Number(ctx.layerViolations);
  if(ctx.budgetExceeded) t += rules.budgetExceededTax;
  if(ctx.godHelperAdded) t += rules.godHelperTax;
  return Number(t.toFixed(2));
}

module.exports={computeRiskPremium,computeDebtTax,rules};
