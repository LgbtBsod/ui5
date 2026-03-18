#!/usr/bin/env node
const fs=require('fs'); const path=require('path'); const root=path.resolve(__dirname,'../..'); const bad=[];
const re=/function\s+[\w$]*\s*\([^)]*\)\s*\{/g;
(function walk(d){ for(const e of fs.readdirSync(d,{withFileTypes:true})){ if(['.git','node_modules','themes','docs'].includes(e.name)) continue; const p=path.join(d,e.name); if(e.isDirectory()) walk(p); else if(e.name.endsWith('.js')){ const lines=fs.readFileSync(p,'utf8').split(/\r?\n/); for(let i=0;i<lines.length;i++){ if(re.test(lines[i])){ let depth=0,started=false,j=i; for(;j<lines.length;j++){ for(const ch of lines[j]){ if(ch==='{'){depth++;started=true;} else if(ch==='}') depth--; } if(started&&depth===0) break; } const len=j-i+1; if(len>80) bad.push(`${path.relative(root,p)}:${i+1}:${len}`); re.lastIndex=0; } } } } })(root);
if(bad.length){ console.error('Functions >80 lines:\n'+bad.join('\n')); process.exit(1);} console.log('function-size-gate passed');
