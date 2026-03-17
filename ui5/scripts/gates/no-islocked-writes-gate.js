#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const root = path.resolve(__dirname, '../..');
const bad = [];
(function walk(d){ for (const e of fs.readdirSync(d,{withFileTypes:true})) { if (e.name === '.git' || e.name==='node_modules') continue; const p = path.join(d,e.name); if(e.isDirectory()) walk(p); else if(/\.js$/.test(e.name)){ const t=fs.readFileSync(p,'utf8'); if (/setProperty\(\s*["']\/isLocked["']/.test(t) || /modelPatch\([^\n]*"\/isLocked"/.test(t)) bad.push(path.relative(root,p)); } } })(root);
if (bad.length){ console.error('Forbidden /isLocked writes found:\n'+bad.join('\n')); process.exit(1);} 
console.log('no-islocked-writes-gate passed');
