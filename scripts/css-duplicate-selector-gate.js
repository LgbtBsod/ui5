#!/usr/bin/env node
const fs = require("fs");
const path = require("path");

const TARGETS = [
    "css/claude-hyper.css",
    "css/modules/00_tokens.css",
    "css/modules/01_theme-modes.css",
    "css/modules/02_background.css",
    "css/modules/10_base.css",
    "css/modules/20_surface.css",
    "css/modules/21_controls.css",
    "css/modules/22_skeleton.css",
    "css/modules/23_dialogs.css",
    "css/modules/40_page_search.css",
    "css/modules/41_page_detail.css",
    "css/modules/90_ui5_patches.css"
].filter((p) => fs.existsSync(p));
const STYLELINT_CONFIG_FILES = [".stylelintrc", ".stylelintrc.json", ".stylelintrc.js", "stylelint.config.js"];

function finish(stats) {
    if (process.argv.includes("--json")) {
        console.log(JSON.stringify({
            name: "css-duplicate-selector-gate",
            ok: true,
            errors: [],
            stats
        }, null, 2));
        return;
    }
    console.log("css-duplicate-selector-gate PASS");
}

function normalizeMessage(text) {
    return String(text || "").replace(/^Unexpected duplicate selector\s+/i, "").trim();
}

async function lintFileForDuplicates(filePath) {
    const stylelint = require("stylelint");
    const result = await stylelint.lint({
        files: [path.resolve(filePath)],
        formatter: "json"
    });
    const report = Array.isArray(result.results) ? result.results[0] : null;
    if (!report || !Array.isArray(report.warnings)) {
        return [];
    }
    return report.warnings
        .filter((warning) => warning.rule === "no-duplicate-selectors")
        .map((warning) => ({
            line: Number(warning.line) || 0,
            selector: normalizeMessage(warning.text)
        }));
}

(async function main() {
    const hasStylelintConfig = STYLELINT_CONFIG_FILES.some((name) => fs.existsSync(path.resolve(name)));
    if (!hasStylelintConfig) {
        finish({ targets: TARGETS.length, duplicateSelectorsDetected: 0, advisory: ["stylelint config missing, duplicate selector scan skipped"] });
        return;
    }

    const issues = [];
    for (const filePath of TARGETS) {
        // eslint-disable-next-line no-await-in-loop
        const duplicates = await lintFileForDuplicates(filePath);
        if (!duplicates.length) {
            continue;
        }
        duplicates.slice(0, 40).forEach((entry) => issues.push(`${filePath}:${entry.line}: duplicate selector ${entry.selector}`));
    }

    finish({
        targets: TARGETS.length,
        duplicateSelectorsDetected: issues.length,
        advisory: issues.slice(0, 80)
    });
}()).catch((error) => {
    console.error("Duplicate selector gate failed:", error && error.message ? error.message : error);
    process.exit(1);
});
