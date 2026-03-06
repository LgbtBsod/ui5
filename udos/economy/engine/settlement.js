#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const {computeBaseCost}=require('./pricing-engine');
const {computeRiskPremium,computeDebtTax}=require('./tax-engine');
const {computeCredits}=require('./credit-engine');

const ROOT = path.resolve(__dirname, '..', '..', '..');
const LEDGER = path.join(ROOT,'udos/economy/ledger/ledger.jsonl');

function readLastBalance(){
  if(!fs.existsSync(LEDGER)) return 100;
  const lines=fs.readFileSync(LEDGER,'utf8').trim().split('\n').filter(Boolean);
  if(!lines.length) return 100;
  return Number(JSON.parse(lines[lines.length-1]).balanceAfter||100);
}

function settle(ctx){
  const balanceBefore = readLastBalance();
  const base = computeBaseCost(ctx);
  const riskPremium = computeRiskPremium(base, ctx);
  const debtTax = computeDebtTax(ctx);
  const total = Number((base+riskPremium+debtTax).toFixed(2));
  const creditsEarned = computeCredits(ctx);
  const netCost = Number((total-creditsEarned).toFixed(2));
  let balanceAfter = Number((balanceBefore-netCost).toFixed(2));

  const emergency = process.env.UDOS_EMERGENCY_OVERRIDE_TOKEN ? true : false;
  let decision = 'CLEAR';
  let suggestedActions=[];
  if(netCost>balanceBefore && !emergency){
    decision='DENY';
    suggestedActions=[
      'Split PR into smaller batches',
      'Run debt-repay mission to earn ArchCredits',
      'Add proof scenarios to reduce RiskPremium'
    ];
  }
  if(netCost>balanceBefore && emergency){
    decision='CLEAR WITH CONDITIONS';
    balanceAfter = Number((balanceAfter-50).toFixed(2));
    suggestedActions=['Emergency override used: auto-create repay-debt mission, require proof-mode next PR'];
  }

  const tx = {
    ts:new Date().toISOString(),
    id:ctx.flightId||`FP-${new Date().toISOString().slice(0,10)}-ECO`,
    type:ctx.changeType||'refactor',
    modules:ctx.modules||[],
    cost:{base,riskPremium,debtTax,total},
    creditsEarned,
    netCost,
    balanceBefore,
    balanceAfter,
    reason:{
      statePathsTouched:(ctx.touchedStatePaths||[]).length>0,
      lockTouched:!!ctx.lockTouched,
      duplicationDelta:Number(ctx.duplicationDelta||0)
    },
    decision,
    suggestedActions,
    emergencyOverride: emergency
  };

  fs.mkdirSync(path.dirname(LEDGER),{recursive:true});
  fs.appendFileSync(LEDGER, JSON.stringify(tx)+'\n');
  return tx;
}

module.exports={settle};
