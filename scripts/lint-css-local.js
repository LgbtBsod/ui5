"use strict";

var fs = require("fs");
var path = require("path");

function walkCssFiles(sRootDir, aResult) {
    fs.readdirSync(sRootDir, { withFileTypes: true }).forEach(function (oEntry) {
        var sFullPath = path.join(sRootDir, oEntry.name);
        if (oEntry.isDirectory()) {
            walkCssFiles(sFullPath, aResult);
            return;
        }
        if (oEntry.isFile() && path.extname(oEntry.name).toLowerCase() === ".css") {
            aResult.push(sFullPath);
        }
    });
    return aResult;
}

function buildLineIndex(sText, iOffset) {
    return sText.slice(0, iOffset).split(/\r?\n/).length;
}

function lintCssFile(sFilePath) {
    var sText = fs.readFileSync(sFilePath, "utf8");
    var aErrors = [];
    var iBraceBalance = 0;
    var aLines = sText.split(/\r?\n/);
    var i;

    for (i = 0; i < sText.length; i += 1) {
        if (sText[i] === "{") {
            iBraceBalance += 1;
        } else if (sText[i] === "}") {
            iBraceBalance -= 1;
        }
        if (iBraceBalance < 0) {
            aErrors.push("Unexpected closing brace at line " + buildLineIndex(sText, i));
            iBraceBalance = 0;
        }
    }
    if (iBraceBalance !== 0) {
        aErrors.push("Unbalanced braces");
    }
    if (/\t/.test(sText)) {
        aErrors.push("Tab indentation is not allowed");
    }
    if (aLines.some(function (sLine) { return /[ \t]+$/.test(sLine); })) {
        aErrors.push("Trailing whitespace detected");
    }
    return aErrors;
}

function main() {
    var sRoot = path.resolve(process.cwd(), "app", "styles");
    var aCssFiles = walkCssFiles(sRoot, []);
    var aFailures = [];

    aCssFiles.forEach(function (sFilePath) {
        var aErrors = lintCssFile(sFilePath);
        if (aErrors.length) {
            aFailures.push(sFilePath + ": " + aErrors.join("; "));
        }
    });

    if (aFailures.length) {
        process.stderr.write("CSS lint failed:\n" + aFailures.join("\n") + "\n");
        process.exit(1);
    }
    process.stdout.write("OK: CSS local lint passed for " + aCssFiles.length + " files\n");
}

main();
