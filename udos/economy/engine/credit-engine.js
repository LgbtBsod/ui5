#!/usr/bin/env node
const { loadRules } = require('./rules-runtime');
const rules = loadRules('credit-rules.json');

function computeCredits(ctx){
  let c=0;
  const dup=Number(ctx.duplicationDelta||0);
  if(dup<0) c += Math.abs(dup)*rules.duplicationReducedPerGroup;
  c += Math.floor(Math.max(0,Number(ctx.controllerLocReduced||0))/10)*rules.controllerLocReducedPer10;
  c += Number(ctx.layerViolationsFixed||0)*rules.layerViolationFixed;
  c += Math.floor(Math.max(0,Number(ctx.invariantScenariosAdded||0))/5)*rules.invariantScenariosPer5;
  c += Math.max(0,Number(ctx.adtDelta||0))*rules.adtScorePer1;
  return Number(c.toFixed(2));
}

module.exports={computeCredits,rules};
