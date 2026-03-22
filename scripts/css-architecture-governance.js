#!/usr/bin/env node

const fs = require("fs");
const path = require("path");
const { detectRuntimeRoot } = require("./qa-shared");

const ROOT = process.cwd();
const RUNTIME_ROOT = detectRuntimeRoot(ROOT);
const STYLES_ROOT = path.join(ROOT, RUNTIME_ROOT, "styles");
const MODULES_ROOT = path.join(STYLES_ROOT, "modules");
const APP_STYLES_PATH = path.join(STYLES_ROOT, "app-styles.css");

function normalizeRelative(filePath) {
    return String(filePath || "").split(path.sep).join("/");
}

function read(filePath) {
    return fs.readFileSync(filePath, "utf8");
}

function listCssFiles(dirPath) {
    return fs.readdirSync(dirPath)
        .filter((entry) => entry.toLowerCase().endsWith(".css"))
        .sort();
}

function parseImports(filePath) {
    const importRegex = /@import\s+url\("([^"]+)"\);/g;
    const imports = [];
    const source = read(filePath);
    let match = importRegex.exec(source);
    while (match) {
        imports.push(match[1]);
        match = importRegex.exec(source);
    }
    return imports;
}

function fail(errors) {
    console.error("css-architecture-governance FAIL");
    errors.forEach((entry) => console.error(`- ${entry}`));
    process.exit(1);
}

if (!fs.existsSync(APP_STYLES_PATH) || !fs.existsSync(MODULES_ROOT)) {
    fail(["styles entrypoint or module root is missing"]);
}

const errors = [];
const appImports = parseImports(APP_STYLES_PATH);
const importedRootModules = new Set(appImports.map((entry) => path.basename(entry)));
const rootModules = listCssFiles(MODULES_ROOT);

rootModules.forEach((moduleName) => {
    if (!importedRootModules.has(moduleName)) {
        errors.push(`app/styles/app-styles.css does not import modules/${moduleName}`);
    }
});

[ 
    { rootModule: "21_controls.css", childDir: "controls" }
].forEach((entry) => {
    const modulePath = path.join(MODULES_ROOT, entry.rootModule);
    if (!fs.existsSync(modulePath)) {
        errors.push(`missing root module modules/${entry.rootModule}`);
        return;
    }

    const moduleImports = new Set(
        parseImports(modulePath).map((value) => normalizeRelative(value))
    );
    const childRoot = path.join(MODULES_ROOT, entry.childDir);
    listCssFiles(childRoot).forEach((childName) => {
        const expectedImport = `./${entry.childDir}/${childName}`;
        if (!moduleImports.has(expectedImport)) {
            errors.push(`modules/${entry.rootModule} does not import ${expectedImport}`);
        }
    });
});

["90_ui5_patches.css", "91_ui5_layout_patches.css", "92_ui5_surface_tuning.css"].forEach((moduleName) => {
    if (!importedRootModules.has(moduleName)) {
        errors.push(`compatibility module ${moduleName} is not wired through app-styles.css`);
    }
});

if (errors.length) {
    fail(errors);
}

console.log(`css-architecture-governance PASS (${rootModules.length} root modules, runtime root ${RUNTIME_ROOT})`);
