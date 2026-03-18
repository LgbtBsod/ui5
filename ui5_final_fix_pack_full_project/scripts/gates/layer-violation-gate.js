#!/usr/bin/env node
const fs=require('fs'); const path=require('path'); const root=path.resolve(__dirname,'../..');
const bad=[];
for (const f of ['controller/Detail.controller.js','controller/Search.controller.js']) {
  const p=path.join(root,f); if(!fs.existsSync(p)) continue; const t=fs.readFileSync(p,'utf8');
  if (/infra\/adapters\//.test(t)) bad.push(`${f}: controller importing adapter`);
}
const usecases=[];
(function walk(d){ for(const e of fs.readdirSync(d,{withFileTypes:true})){ const p=path.join(d,e.name); if(e.isDirectory()) walk(p); else if(/UseCase\.js$/.test(e.name)) usecases.push(p);} })(path.join(root,'service/domain'));
for(const p of usecases){ const t=fs.readFileSync(p,'utf8'); if(/sap\/m\//.test(t)||/sap\.m\./.test(t)) bad.push(path.relative(root,p)+': usecase importing UI module'); }
if (bad.length){ console.error('Layer violations:\n'+bad.join('\n')); process.exit(1);} 
console.log('layer-violation-gate passed');
