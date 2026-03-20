#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const ROOT = process.cwd();
const APP_ROOT = path.join(ROOT, "app");
const ALLOWED_LEGACY_PATHS = [
    path.normalize("app/styles/modules/20_surface.css"),
    path.normalize("app/styles/modules/controls/26_control_skin.css"),
    path.normalize("app/styles/modules/controls/30_action_and_shell_buttons.css"),
    path.normalize("app/styles/modules/detail/46_detail_rail_shell.css"),
    path.normalize("app/styles/modules/detail/50_detail_object_shell.css"),
    path.normalize("app/styles/modules/detail/52_detail_info_cards.css"),
    path.normalize("app/styles/modules/search/42_search_action_toolbar.css")
];
const SCAN_EXTENSIONS = new Set([".css", ".xml", ".js"]);
const LEGACY_TOKENS = [
    "brandActionBtn",
    "brandActionPrimary",
    "brandActionGhost",
    "actionPriorityPrimary",
    "actionPrioritySecondary",
    "actionPriorityDanger",
    "detailSectionCard",
    "detailControlInlineCard",
    "infoCardTile"
];
const TYPO_SELECTORS = [".cnkApp"];

function walk(dir) {
    const out = [];
    fs.readdirSync(dir, { withFileTypes: true }).forEach((entry) => {
        const full = path.join(dir, entry.name);
        if (entry.isDirectory()) {
            out.push(...walk(full));
            return;
        }
        if (SCAN_EXTENSIONS.has(path.extname(entry.name))) {
            out.push(full);
        }
    });
    return out;
}

function countLine(source, index) {
    return source.slice(0, index).split(/\r?\n/).length;
}

const errors = [];

walk(APP_ROOT).forEach((filePath) => {
    const relPath = path.relative(ROOT, filePath);
    const source = fs.readFileSync(filePath, "utf8");
    const allowLegacy = ALLOWED_LEGACY_PATHS.includes(path.normalize(relPath));

    TYPO_SELECTORS.forEach((token) => {
        const idx = source.indexOf(token);
        if (idx >= 0) {
            errors.push(`${relPath}:${countLine(source, idx)} contains typo-like selector ${token}`);
        }
    });

    if (allowLegacy) {
        return;
    }

    LEGACY_TOKENS.forEach((token) => {
        const matcher = new RegExp(`\\b${token}\\b`, "g");
        let match = matcher.exec(source);
        while (match) {
            errors.push(`${relPath}:${countLine(source, match.index)} uses legacy style token ${token}`);
            match = matcher.exec(source);
        }
    });
});

if (errors.length) {
    console.error("legacy-style-governance FAIL");
    errors.forEach((entry) => console.error(`- ${entry}`));
    process.exit(1);
}

console.log("legacy-style-governance PASS");
