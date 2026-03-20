/**
 * verify-i18n-completeness.js
 * CI gate: all keys in i18n.properties (EN baseline) must exist in i18n_ru.properties.
 * Exit 1 on missing keys — blocks validate/build pipeline.
 * Usage: node scripts/verify-i18n-completeness.js
 */
"use strict";

var fs   = require("fs");
var path = require("path");

var BASE = path.join(__dirname, "..", "app", "i18n");

function parseKeys(filePath) {
    return fs.readFileSync(filePath, "utf8")
        .split("\n")
        .filter(function (l) {
            var t = l.trim();
            return t && t[0] !== "#" && t[0] !== "!" && t.indexOf("=") > 0;
        })
        .map(function (l) { return l.substring(0, l.indexOf("=")).trim(); });
}

var enKeys  = parseKeys(path.join(BASE, "i18n.properties"));
var ruSet   = new Set(parseKeys(path.join(BASE, "i18n_ru.properties")));
var missing = enKeys.filter(function (k) { return !ruSet.has(k); });

if (missing.length) {
    process.stderr.write(
        "ERROR [i18n-completeness]: " + missing.length +
        " key(s) missing from i18n_ru.properties:\n  " +
        missing.join("\n  ") + "\n"
    );
    process.exit(1);
}
console.log("OK [i18n-completeness]: all " + enKeys.length + " EN keys present in RU");
