#!/usr/bin/env node
const fs = require("fs");
const path = require("path");
const { listCssFiles, toProjectRel } = require("./lib/cssFiles");
const { readJsonSafe, readTextSafe } = require("./lib/auditInput");

const ROOT = process.cwd();
const CSS_ROOT = path.join(ROOT, "css");
const ARTIFACT_PATH = path.join(ROOT, "docs", "artifacts", "style-token-duplication-report.json");
const ALLOWLIST_PATH = path.join(ROOT, "scripts", "style-token-duplication-allowlist.json");

function normalizePath(p) {
    return path.normalize(path.resolve(ROOT, p));
}

function readAllowlist() {
    const defaults = {
        tokenFiles: [
            "css/modules/00_tokens.css",
            "css/modules/01_theme-modes.css"
        ],
        allowedCrossFileTokens: []
    };
    if (!fs.existsSync(ALLOWLIST_PATH)) {
        return defaults;
    }
    const parsed = readJsonSafe(ALLOWLIST_PATH, null);
    if (!parsed || typeof parsed !== "object") {
        console.error("[style-token-duplication-gate] Failed to parse allowlist:", ALLOWLIST_PATH);
        process.exit(1);
    }
    return {
        tokenFiles: Array.isArray(parsed.tokenFiles) && parsed.tokenFiles.length ? parsed.tokenFiles : defaults.tokenFiles,
        allowedCrossFileTokens: Array.isArray(parsed.allowedCrossFileTokens) ? parsed.allowedCrossFileTokens : defaults.allowedCrossFileTokens
    };
}

function stripComments(content) {
    return String(content || "").replace(/\/\*[\s\S]*?\*\//g, "");
}

function lineFromIndex(content, index) {
    return content.slice(0, index).split(/\r?\n/).length;
}

function collectTokenDeclarations(files) {
    const map = new Map();
    const tokenDeclRegex = /--([a-zA-Z0-9_-]+)\s*:\s*([^;]+);/g;

    files.forEach((filePath) => {
        const raw = readTextSafe(filePath, "");
        const content = stripComments(raw);
        let match;
        while ((match = tokenDeclRegex.exec(content)) !== null) {
            const token = `--${match[1]}`;
            const value = String(match[2] || "").replace(/\s+/g, " ").trim();
            const occurrence = {
                fileAbs: normalizePath(filePath),
                fileRel: toProjectRel(ROOT, filePath),
                line: lineFromIndex(content, match.index),
                value
            };
            if (!map.has(token)) {
                map.set(token, []);
            }
            map.get(token).push(occurrence);
        }
    });
    return map;
}

function writeArtifact(payload) {
    fs.mkdirSync(path.dirname(ARTIFACT_PATH), { recursive: true });
    fs.writeFileSync(ARTIFACT_PATH, JSON.stringify(payload, null, 2), "utf8");
}

function main() {
    const allowlist = readAllowlist();
    const tokenFiles = new Set(allowlist.tokenFiles.map(normalizePath));
    const allowedCrossFileTokens = new Set(
        allowlist.allowedCrossFileTokens.map((token) => String(token || "").trim()).filter(Boolean)
    );
    const cssFiles = listCssFiles(ROOT, CSS_ROOT);
    const declarations = collectTokenDeclarations(cssFiles);

    const duplicateTokens = [];
    const violatingTokens = [];

    declarations.forEach((occurrences, token) => {
        if (!occurrences || occurrences.length < 2) {
            return;
        }
        const uniqueFiles = Array.from(new Set(occurrences.map((o) => o.fileAbs)));
        const crossFile = uniqueFiles.length > 1;
        const tokenFileOnly = uniqueFiles.every((abs) => tokenFiles.has(abs));
        const allowlisted = allowedCrossFileTokens.has(token);
        const record = {
            token,
            count: occurrences.length,
            files: occurrences.map((o) => `${o.fileRel}:${o.line}`),
            crossFile,
            tokenFileOnly,
            allowlisted
        };
        duplicateTokens.push(record);
        if (crossFile && !tokenFileOnly && !allowlisted) {
            violatingTokens.push(record);
        }
    });

    const payload = {
        generatedAt: new Date().toISOString(),
        cssFileCount: cssFiles.length,
        tokenDeclarationCount: Array.from(declarations.values()).reduce((sum, list) => sum + list.length, 0),
        duplicateTokenCount: duplicateTokens.length,
        violatingTokenCount: violatingTokens.length,
        duplicates: duplicateTokens,
        violations: violatingTokens
    };
    writeArtifact(payload);

    if (violatingTokens.length) {
        console.error("[style-token-duplication-gate] FAIL duplicate token declarations across files:");
        violatingTokens.forEach((item) => {
            console.error(`- ${item.token}`);
            item.files.forEach((loc) => console.error(`  - ${loc}`));
        });
        process.exit(1);
    }

    console.log(
        `[style-token-duplication-gate] PASS (declarations=${payload.tokenDeclarationCount}, duplicates=${payload.duplicateTokenCount}, violations=0)`
    );
}

main();
