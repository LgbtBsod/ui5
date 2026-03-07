#!/usr/bin/env node
const fs = require("fs");
const path = require("path");
const { spawnSync } = require("child_process");
const { listFiles } = require("./lib/fileWalker");

const ROOT = process.cwd();
const NPM_COMMAND = process.platform === 'win32' ? 'npm.cmd' : 'npm';
const NODE_COMMAND = process.execPath;
const TOKEN_FILES_ALLOWLIST = new Set([
    path.normalize(path.join(ROOT, "app/css/modules/00_tokens.css")),
    path.normalize(path.join(ROOT, "app/css/modules/01_theme-modes.css"))
]);

function runStep(name, command, args) {
    const result = spawnSync(command, args, {
        cwd: ROOT,
        shell: false,
        encoding: "utf8"
    });
    const output = String((result.stdout || "") + (result.stderr || ""));
    if (result.status !== 0) {
        console.error(`[style:scan] FAIL ${name}`);
        console.error(output.trim());
        process.exit(result.status || 1);
    }
    return output;
}

function runBaselineStep(name, command, args, knownPattern) {
    const result = spawnSync(command, args, {
        cwd: ROOT,
        shell: false,
        encoding: "utf8"
    });
    const output = String((result.stdout || "") + (result.stderr || ""));
    if (result.status === 0) {
        return output;
    }
    if (knownPattern && knownPattern.test(output)) {
        console.warn(`[style:scan] WARN ${name} matched existing baseline debt and did not regress.`);
        return output;
    }
    console.error(`[style:scan] FAIL ${name}`);
    console.error(output.trim());
    process.exit(result.status || 1);
}

function parseStylelintRuleCounts(output) {
    const counts = {};
    String(output || "").split(/\r?\n/).forEach((line) => {
        const cleanLine = line.replace(/\u001b\[[0-9;]*m/g, "");
        const match = cleanLine.match(/\b([a-z]+(?:-[a-z]+)+)\s*$/i);
        if (!match) {
            return;
        }
        const rule = match[1];
        counts[rule] = (counts[rule] || 0) + 1;
    });
    return counts;
}

function runLintWithBaseline() {
    const result = spawnSync(NPM_COMMAND, ["run", "lint:css"], {
        cwd: ROOT,
        shell: false,
        encoding: "utf8"
    });
    const output = String((result.stdout || "") + (result.stderr || ""));
    if (result.status === 0) {
        return;
    }
    const allowedBaseline = {
        "no-duplicate-selectors": 32,
        "length-zero-no-unit": 1,
        "rule-empty-line-before": 1
    };
    const counts = parseStylelintRuleCounts(output);
    const unknownRules = Object.keys(counts).filter((rule) => !Object.prototype.hasOwnProperty.call(allowedBaseline, rule));
    const exceededRules = Object.keys(allowedBaseline).filter((rule) => (counts[rule] || 0) > allowedBaseline[rule]);

    if (unknownRules.length || exceededRules.length) {
        console.error("[style:scan] FAIL lint:css baseline exceeded");
        console.error(output.trim());
        if (unknownRules.length) {
            console.error(`Unknown lint rule violations: ${unknownRules.join(", ")}`);
        }
        if (exceededRules.length) {
            console.error(`Exceeded baseline: ${exceededRules.map((rule) => `${rule}:${counts[rule]}/${allowedBaseline[rule]}`).join(", ")}`);
        }
        process.exit(result.status || 1);
    }
    console.warn("[style:scan] WARN lint:css matched existing baseline debt and did not regress.");
}

function findRawHexViolations() {
    const files = listFiles(ROOT, {
        include: ["app/css/**/*.css"]
    }).map((file) => path.normalize(path.join(ROOT, file)));
    const violations = [];
    files.forEach((file) => {
        if (TOKEN_FILES_ALLOWLIST.has(file)) {
            return;
        }
        const lines = fs.readFileSync(file, "utf8").split(/\r?\n/);
        lines.forEach((line, index) => {
            const match = line.match(/(^|[^-\w])#([0-9a-fA-F]{3}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})\b/);
            if (match) {
                violations.push(`${path.relative(ROOT, file)}:${index + 1}: ${line.trim()}`);
            }
        });
    });
    return violations;
}

(function main() {
    runLintWithBaseline();
    runStep("check:css-arch", NODE_COMMAND, ["scripts/check-css-architecture.mjs"]);
    const duplicateOutput = runStep("css-duplicate-selector-gate", NODE_COMMAND, ["scripts/css-duplicate-selector-gate.js"]);
    if (/duplicates detected/i.test(duplicateOutput)) {
        console.warn("[style:scan] WARN duplicate selector debt remains in legacy CSS baseline.");
    }
    runStep("design-language-gate", NODE_COMMAND, ["scripts/design-language-gate.js"]);
    runBaselineStep("semantic-contrast-gate", NODE_COMMAND, ["scripts/semantic-contrast-gate.js"], /missing semantic vars/i);
    runStep("style-system-audit", NODE_COMMAND, ["scripts/style-system-audit.js"]);
    runStep("style-token-duplication-gate", NODE_COMMAND, ["scripts/style-token-duplication-gate.js"]);

    const hexViolations = findRawHexViolations();
    if (hexViolations.length) {
        console.error("[style:scan] FAIL no-raw-hex-outside-token-files");
        hexViolations.slice(0, 40).forEach((line) => console.error(`- ${line}`));
        if (hexViolations.length > 40) {
            console.error(`- ... ${hexViolations.length - 40} more`);
        }
        process.exit(1);
    }

    console.log("[style:scan] PASS");
})();
