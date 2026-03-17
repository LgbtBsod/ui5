#!/usr/bin/env node
const fs = require("fs");
const path = require("path");
const { listFiles } = require("./lib/fileWalker");
const { readJsonSafe, readTextSafe } = require("./lib/auditInput");
const { detectRuntimeRoot } = require("./qa-shared");

const ROOT = process.cwd();
const RUNTIME_ROOT = detectRuntimeRoot(ROOT);
const REPORT_PATH = path.join(ROOT, "docs/artifacts/feature-scan-report.json");
const ALLOWLIST_PATH = path.join(ROOT, "scripts/feature-scan-allowlist.json");
const EVENT_ATTRS = [
    "press", "change", "liveChange", "search", "selectionChange", "itemPress",
    "confirm", "cancel", "close", "initialise", "filterChange", "beforeRebindTable",
    "dataReceived", "suggest", "suggestionItemSelected", "valueHelpRequest"
];

function read(file) {
    return readTextSafe(file, "");
}

function relative(file) {
    return path.relative(ROOT, file).replace(/\\/g, "/");
}

function parseAllowlist() {
    if (!fs.existsSync(ALLOWLIST_PATH)) {
        return {
            emptyHandlers: [],
            todoTriggers: [],
            unreachableDialogs: [],
            unreachableRoutes: []
        };
    }
    const parsed = readJsonSafe(ALLOWLIST_PATH, null);
    if (!parsed || typeof parsed !== "object") {
        console.error(`[feature:scan] invalid allowlist JSON: ${ALLOWLIST_PATH}`);
        process.exit(1);
    }
    return parsed;
}

function collectControllers() {
    return listFiles(ROOT, {
        include: ["controller/*.controller.js", "controller/**/*.controller.js"]
    }).map((file) => path.join(ROOT, file));
}

function collectViews() {
    return listFiles(ROOT, {
        include: ["views/**/*.xml"]
    }).map((file) => path.join(ROOT, file));
}

function extractEmptyHandlers(controllerFile) {
    const src = read(controllerFile);
    const findings = [];
    const re = /([A-Za-z0-9_]+)\s*:\s*function\s*\([^)]*\)\s*\{\s*(?:return\s+null\s*;\s*)?\}/g;
    let match;
    while ((match = re.exec(src)) !== null) {
        findings.push({
            file: relative(controllerFile),
            method: match[1],
            key: `${relative(controllerFile)}:${match[1]}`
        });
    }
    return findings;
}

function resolveControllerPathFromView(xmlText) {
    const match = xmlText.match(/controllerName\s*=\s*"([^"]+)"/);
    if (!match) {
        return "";
    }
    return path.join(ROOT, match[1].replace(/^PRODUCTION_CONTROL_CHECKLIST\./, "").replace(/\./g, "/") + ".controller.js");
}

function extractEventHandlers(xmlText) {
    const handlers = [];
    const attrExpr = EVENT_ATTRS.join("|");
    const re = new RegExp(`\\s(?:${attrExpr})\\s*=\\s*"\\.?([A-Za-z0-9_]+)"`, "g");
    let match;
    while ((match = re.exec(xmlText)) !== null) {
        handlers.push(match[1]);
    }
    return handlers;
}

function collectUiBoundEmptyHandlers(emptyHandlersByController) {
    const findings = [];
    collectViews().forEach((viewFile) => {
        const xml = read(viewFile);
        const controllerPath = resolveControllerPathFromView(xml);
        if (!controllerPath || !emptyHandlersByController[controllerPath]) {
            return;
        }
        const usedHandlers = extractEventHandlers(xml);
        usedHandlers.forEach((methodName) => {
            const key = `${relative(controllerPath)}:${methodName}`;
            if (emptyHandlersByController[controllerPath].has(methodName)) {
                findings.push({
                    file: relative(viewFile),
                    controller: relative(controllerPath),
                    method: methodName,
                    key
                });
            }
        });
    });
    return findings;
}

function collectTodoTriggers() {
    const findings = [];
    const xmlFiles = collectViews();
    xmlFiles.forEach((file) => {
        const text = read(file);
        const lines = text.split(/\r?\n/);
        lines.forEach((line, index) => {
            if (/TODO/i.test(line) && /(press|change|search|selectionChange)\s*=/.test(line)) {
                findings.push({
                    file: relative(file),
                    line: index + 1,
                    text: line.trim(),
                    key: `${relative(file)}:${index + 1}`
                });
            }
        });
    });
    return findings;
}


function readNavigationRouteMap() {
    var navigationContracts = path.join(ROOT, RUNTIME_ROOT, "contracts/NavigationContracts.js");
    var text;
    var map = {};
    var routesBlock;
    var match;
    if (!fs.existsSync(navigationContracts)) {
        return map;
    }
    text = read(navigationContracts);
    routesBlock = text.match(/var\s+ROUTES\s*=\s*Object\.freeze\(\{([\s\S]*?)\}\);/);
    if (!routesBlock) {
        return map;
    }
    var re = /([A-Z_]+)\s*:\s*"([a-zA-Z0-9_]+)"/g;
    while ((match = re.exec(routesBlock[1])) !== null) {
        map[match[1]] = match[2];
    }
    return map;
}

function collectUnreachableRoutes() {
    const manifestPath = path.join(ROOT, RUNTIME_ROOT, "manifest.json");
    if (!fs.existsSync(manifestPath)) {
        return [];
    }
    const manifest = readJsonSafe(manifestPath, null);
    if (!manifest || typeof manifest !== "object") {
        return [];
    }
    const routes = (((manifest["sap.ui5"] || {}).routing || {}).routes || []).map((route) => ({
        name: String(route.name || ""),
        pattern: String(route.pattern || "")
    }));
    const jsFiles = listFiles(ROOT, {
        include: ["**/*.js"]
    }).map((file) => path.join(ROOT, file));
    const used = new Set();
    const usageRegex = /(navTo|getRoute|attachPatternMatched|attachRouteMatched)\s*\(\s*["']([^"']+)["']/g;
    const routeMap = readNavigationRouteMap();
    Object.keys(routeMap).forEach((key) => used.add(routeMap[key]));
    jsFiles.forEach((file) => {
        const src = read(file);
        let match;
        while ((match = usageRegex.exec(src)) !== null) {
            used.add(match[2]);
        }
        Object.keys(routeMap).forEach((routeKey) => {
            if (src.includes('NavigationContracts.ROUTES.' + routeKey) || src.includes('infra/contracts/NavigationContracts') && src.includes('NavigationContracts.ROUTES.' + routeKey)) {
                used.add(routeMap[routeKey]);
            }
        });
    });
    return routes
        .filter((route) => route.name)
        .filter((route) => !(route.pattern && route.pattern.includes("{")))
        .filter((route) => !used.has(route.name))
        .map((route) => ({ route: route.name, key: route.name }));
}

function collectUnreachableDialogs() {
    const findings = [];
    const dialogFragments = listFiles(ROOT, {
        include: ["views/fragment/**/*Dialog.fragment.xml"]
    }).map((file) => path.join(ROOT, file));
    const jsFiles = listFiles(ROOT, {
        include: ["controller/**/*.js", "service/**/*.js"]
    }).map((file) => path.join(ROOT, file));
    const joinedSource = jsFiles.map((file) => read(file)).join("\n");
    dialogFragments.forEach((fragmentPath) => {
        const baseName = path.basename(fragmentPath, ".fragment.xml");
        if (!joinedSource.includes(baseName)) {
            findings.push({
                dialog: baseName,
                file: relative(fragmentPath),
                key: baseName
            });
        }
    });
    return findings;
}

function sleepSync(ms) {
    if (!Number.isFinite(ms) || ms <= 0) {
        return;
    }
    Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, ms);
}

function writeReport(report) {
    fs.mkdirSync(path.dirname(REPORT_PATH), { recursive: true });
    const text = JSON.stringify(report, null, 2);
    let lastError = null;
    for (let i = 0; i < 5; i += 1) {
        try {
            fs.writeFileSync(REPORT_PATH, text);
            return REPORT_PATH;
        } catch (error) {
            lastError = error;
            if (!error || error.code !== "EPERM") {
                throw error;
            }
            sleepSync(80 * (i + 1));
        }
    }
    const fallbackPath = path.join(ROOT, "docs/artifacts/feature-scan-report.fallback.json");
    fs.writeFileSync(fallbackPath, text);
    console.warn(`[feature:scan] WARN report path locked, wrote fallback report: ${fallbackPath}`);
    return fallbackPath;
}

function filterAllowlisted(findings, allowSet, keyField) {
    return findings.filter((finding) => !allowSet.has(String(finding[keyField] || "")));
}

(function main() {
    const allowlist = parseAllowlist();
    const controllerFiles = collectControllers();
    const emptyHandlersByController = {};
    const emptyHandlerFindings = [];

    controllerFiles.forEach((controllerFile) => {
        const emptyHandlers = extractEmptyHandlers(controllerFile);
        emptyHandlersByController[controllerFile] = new Set(emptyHandlers.map((entry) => entry.method));
        emptyHandlers.forEach((entry) => emptyHandlerFindings.push(entry));
    });

    const uiBoundEmpty = collectUiBoundEmptyHandlers(emptyHandlersByController);
    const todoTriggers = collectTodoTriggers();
    const unreachableRoutes = collectUnreachableRoutes();
    const unreachableDialogs = collectUnreachableDialogs();

    const finalFindings = {
        emptyHandlers: filterAllowlisted(uiBoundEmpty, new Set((allowlist.emptyHandlers || []).map(String)), "key"),
        todoTriggers: filterAllowlisted(todoTriggers, new Set((allowlist.todoTriggers || []).map(String)), "key"),
        unreachableDialogs: filterAllowlisted(unreachableDialogs, new Set((allowlist.unreachableDialogs || []).map(String)), "key"),
        unreachableRoutes: filterAllowlisted(unreachableRoutes, new Set((allowlist.unreachableRoutes || []).map(String)), "key")
    };

    const report = {
        generatedAt: new Date().toISOString(),
        scanned: {
            controllers: controllerFiles.length,
            views: collectViews().length
        },
        totals: {
            emptyHandlers: finalFindings.emptyHandlers.length,
            todoTriggers: finalFindings.todoTriggers.length,
            unreachableDialogs: finalFindings.unreachableDialogs.length,
            unreachableRoutes: finalFindings.unreachableRoutes.length
        },
        allowlist,
        findings: finalFindings
    };
    writeReport(report);

    const hasFailures = Object.values(report.totals).some((count) => count > 0);
    if (hasFailures) {
        console.error("[feature:scan] FAIL");
        console.error(JSON.stringify(report.totals, null, 2));
        process.exit(1);
    }
    console.log("[feature:scan] PASS");
})();
