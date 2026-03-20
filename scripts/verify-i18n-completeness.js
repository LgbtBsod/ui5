/**
 * verify-i18n-completeness.js
 *
 * CI gate: every key in i18n.properties (EN baseline) must exist in i18n_ru.properties.
 * Fails with exit code 1 if any keys are missing — blocks build/validate pipeline.
 *
 * Usage: node scripts/verify-i18n-completeness.js
 * Add to package.json validate: "node scripts/verify-i18n-completeness.js && ..."
 */
"use strict";

var fs = require("fs");
var path = require("path");

var BASE = path.join(__dirname, "..", "app", "i18n");
var EN_FILE = path.join(BASE, "i18n.properties");
var RU_FILE = path.join(BASE, "i18n_ru.properties");

function parseKeys(filePath) {
    var content = fs.readFileSync(filePath, "utf8");
    var keys = [];
    content.split("\n").forEach(function (line) {
        var trimmed = line.trim();
        if (!trimmed || trimmed.charAt(0) === "#" || trimmed.charAt(0) === "!") {
            return;
        }
        var eqIdx = trimmed.indexOf("=");
        if (eqIdx > 0) {
            keys.push(trimmed.substring(0, eqIdx).trim());
        }
    });
    return keys;
}

var enKeys = parseKeys(EN_FILE);
var ruKeys = new Set(parseKeys(RU_FILE));

var missing = enKeys.filter(function (k) { return !ruKeys.has(k); });

if (missing.length > 0) {
    process.stderr.write(
        "ERROR [verify-i18n-completeness]: " + missing.length +
        " key(s) in i18n.properties are missing from i18n_ru.properties:\n  " +
        missing.join("\n  ") + "\n"
    );
    process.exit(1);
}

console.log(
    "OK [verify-i18n-completeness]: all " + enKeys.length +
    " EN keys present in i18n_ru.properties"
);
