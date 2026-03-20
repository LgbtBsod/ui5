"use strict";
var fs=require("fs"),path=require("path"),BASE=path.join(__dirname,"..","app","i18n");
function keys(f){return fs.readFileSync(f,"utf8").split("\n").filter(function(l){var t=l.trim();return t&&t[0]!=="#"&&t[0]!=="!"&&t.indexOf("=")>0;}).map(function(l){return l.substring(0,l.indexOf("=")).trim();});}
var en=keys(path.join(BASE,"i18n.properties")),ru=new Set(keys(path.join(BASE,"i18n_ru.properties")));
var bad=en.filter(function(k){return!ru.has(k);});
if(bad.length){process.stderr.write("ERROR [i18n]: "+bad.length+" missing from RU:\n  "+bad.join("\n  ")+"\n");process.exit(1);}
console.log("OK [i18n]: all "+en.length+" EN keys in RU");
