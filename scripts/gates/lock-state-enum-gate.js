#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const root = path.resolve(__dirname, '../..');
const bad = [];
(function walk(d){ for (const e of fs.readdirSync(d,{withFileTypes:true})) { if (e.name === '.git' || e.name==='node_modules') continue; const p = path.join(d,e.name); if(e.isDirectory()) walk(p); else if(/\.(js|xml)$/.test(e.name)){ const t=fs.readFileSync(p,'utf8'); if ((/lockState[^\n]*"(FAILED|SUCCESS|ERROR)"/.test(t) || /lockOperationState[^\n]*"(FAILED|SUCCESS|ERROR)"/.test(t)) || ((/['"]FAILED['"]/.test(t)) && (/lockState/.test(t) || /lockOperationState/.test(t)))) bad.push(path.relative(root,p)); } } })(root);
if (bad.length){ console.error('Invalid lock enum usage found:\n'+bad.join('\n')); process.exit(1);} 
console.log('lock-state-enum-gate passed');
