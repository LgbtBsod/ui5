#!/usr/bin/env node
const fs=require('fs'); const path=require('path'); const root=path.resolve(__dirname,'../..'); const bad=[];
(function walk(d){ for(const e of fs.readdirSync(d,{withFileTypes:true})){ if(['.git','node_modules','themes','docs'].includes(e.name)) continue; const p=path.join(d,e.name); if(e.isDirectory()) walk(p); else { const n=fs.readFileSync(p,'utf8').split(/\r?\n/).length; if(n>400) bad.push(`${path.relative(root,p)}:${n}`); } } })(root);
if(bad.length){ console.error('Files >400 lines:\n'+bad.join('\n')); process.exit(1);} console.log('file-size-gate passed');
