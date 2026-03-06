#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { settle } = require('./settlement');

const ROOT = path.resolve(__dirname, '..', '..', '..');
const REPORT_DIR = path.join(ROOT,'udos/economy/reports');

function run(ctx){
  const tx = settle(ctx);
  fs.mkdirSync(REPORT_DIR,{recursive:true});
  fs.writeFileSync(path.join(REPORT_DIR,'pr-cost-breakdown.md'),
`# PR COST BREAKDOWN\n\nBaseCost: ${tx.cost.base}\nRiskPremium: ${tx.cost.riskPremium}\nDebtTax: ${tx.cost.debtTax}\nCreditsEarned: ${tx.creditsEarned}\nNetCost: ${tx.netCost}\nBalance: ${tx.balanceBefore} -> ${tx.balanceAfter} => ${tx.decision}\n\nSuggested actions:\n${tx.suggestedActions.map(s=>`- ${s}`).join('\n') || '- none'}\n`);
  fs.writeFileSync(path.join(REPORT_DIR,'ledger-summary.md'),
`# Ledger Summary\n\n- Last transaction: ${tx.id}\n- Balance after: ${tx.balanceAfter}\n- Top taxes: risk=${tx.cost.riskPremium}, debt=${tx.cost.debtTax}\n`);
  fs.writeFileSync(path.join(REPORT_DIR,'economy-report.md'),
`# Economy Report\n\n- Decision: ${tx.decision}\n- Emergency override: ${tx.emergencyOverride}\n- NetCost: ${tx.netCost}\n- Plan out: ${tx.suggestedActions[0] || 'none'}\n`);
  return tx;
}

if(require.main===module){
  const tx=run({flightId:'FP-LOCAL',changeType:'refactor',modules:['detail'],files:[],riskLevel:'LOW'});
  console.log(`PASS economy-engine decision=${tx.decision}`);
}

module.exports={run};
