#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const ROOT = process.cwd();
const TARGETS = [
    { relPath: "app/views/App.view.xml", maxClassTokens: 4 },
    { relPath: "app/views/Analytics.view.xml", maxClassTokens: 3 },
    { relPath: "app/views/Search.view.xml", maxClassTokens: 3 },
    { relPath: "app/views/Detail.view.xml", maxClassTokens: 4 },
    { relPath: "app/views/fragment/DetailAttachmentsSection.fragment.xml", maxClassTokens: 5 }
];
const FORBIDDEN_CLASS_PATTERNS = [/sapUi(?:Tiny|Small|Medium|Large)Margin[A-Za-z]*/g];

function fail(errors) {
    console.error("xml-class-governance FAIL");
    errors.forEach((entry) => console.error(`- ${entry}`));
    process.exit(1);
}

function read(relPath) {
    return fs.readFileSync(path.join(ROOT, relPath), "utf8");
}

function countLine(source, index) {
    return source.slice(0, index).split(/\r?\n/).length;
}

const errors = [];

TARGETS.forEach((target) => {
    const source = read(target.relPath);
    const classRegex = /class="([^"]+)"/g;
    let match = classRegex.exec(source);

    while (match) {
        const classNames = match[1].trim().split(/\s+/).filter(Boolean);
        const line = countLine(source, match.index);

        if (classNames.length > target.maxClassTokens) {
            errors.push(`${target.relPath}:${line} has ${classNames.length} class tokens (max ${target.maxClassTokens})`);
        }

        FORBIDDEN_CLASS_PATTERNS.forEach((pattern) => {
            const found = classNames.find((className) => pattern.test(className));
            pattern.lastIndex = 0;
            if (found) {
                errors.push(`${target.relPath}:${line} uses forbidden presentational utility class ${found}`);
            }
        });

        match = classRegex.exec(source);
    }
});

if (errors.length) {
    fail(errors);
}

console.log(`xml-class-governance PASS (${TARGETS.length} xml entry files)`);
