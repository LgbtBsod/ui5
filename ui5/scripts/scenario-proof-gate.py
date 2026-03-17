#!/usr/bin/env python3
import json
import os
import sys

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))

SCENARIO_ARTIFACT = os.path.join(ROOT, "docs", "artifacts", "scenario-suite-report.json")
QA_CRAWL_ARTIFACT = os.path.join(ROOT, "docs", "artifacts", "qa-crawl-report.json")
SCENARIO_DOC = os.path.join(ROOT, "docs", "SCENARIO_SUITE.md")
QA_DOC = os.path.join(ROOT, "docs", "QA_CRAWL_REPORT.md")
LAYOUT_DOC = os.path.join(ROOT, "docs", "LAYOUT_AUDIT.md")
HEURISTIC_DOC = os.path.join(ROOT, "docs", "HEURISTIC_REVIEWS.md")
EVIDENCE_INDEX_DOC = os.path.join(ROOT, "docs", "SCENARIO_EVIDENCE_INDEX.md")

EXPECTED_SCENARIO_TOTAL = 42
EXPECTED_BREAKPOINTS = {720, 1080, 1440}
EXPECTED_THEMES = {"morning", "night"}
EXPECTED_KEYBOARD_VIEWPORTS = {"desktop", "tablet", "phone"}
REQUIRED_RUNTIME_CATEGORIES = {
    "menu-shell",
    "dialog-shell",
    "network-backend",
    "concurrency-locking",
    "dirty-navigation",
    "validation-forms",
    "tables-powerflows",
    "dialogs-overlays",
    "feedback-hygiene",
    "themes-visual",
    "accessibility",
    "performance-polish",
    "security-integrity",
    "i18n-l10n",
}


def read_json(path: str):
    with open(path, "r", encoding="utf-8") as handle:
        return json.load(handle)


def read_text(path: str) -> str:
    with open(path, "r", encoding="utf-8") as handle:
        return handle.read()


def require_file(path: str, errors: list[str]) -> None:
    if not os.path.exists(path):
        errors.append(f"missing file: {os.path.relpath(path, ROOT).replace('\\', '/')}")


def check_scenario_report(data: dict, errors: list[str]) -> None:
    summary = data.get("summary", {})
    results = data.get("results", [])

    if int(summary.get("total", -1)) != EXPECTED_SCENARIO_TOTAL:
        errors.append(f"scenario total mismatch: expected {EXPECTED_SCENARIO_TOTAL}, got {summary.get('total')}")
    if int(summary.get("pass", -1)) != EXPECTED_SCENARIO_TOTAL:
        errors.append(f"scenario pass mismatch: expected {EXPECTED_SCENARIO_TOTAL}, got {summary.get('pass')}")
    if int(summary.get("manualRequired", -1)) != 0:
        errors.append(f"manualRequired must be 0, got {summary.get('manualRequired')}")
    if int(summary.get("fail", -1)) != 0:
        errors.append(f"scenario fail count must be 0, got {summary.get('fail')}")
    if int(summary.get("openBlockers", -1)) != 0:
        errors.append(f"openBlockers must be 0, got {summary.get('openBlockers')}")

    if len(results) != EXPECTED_SCENARIO_TOTAL:
        errors.append(f"scenario results length mismatch: expected {EXPECTED_SCENARIO_TOTAL}, got {len(results)}")
    else:
        non_pass = [entry.get("id") for entry in results if str(entry.get("status")) != "PASS"]
        if non_pass:
            errors.append(f"non-pass scenarios detected: {', '.join(non_pass)}")
        no_evidence = [entry.get("id") for entry in results if len(entry.get("evidence") or []) == 0]
        if no_evidence:
            errors.append(f"scenarios with empty evidence: {', '.join(no_evidence)}")


def check_qa_crawl(data: dict, errors: list[str]) -> None:
    broken = data.get("broken", [])
    remaining = data.get("remaining", [])
    matrix = data.get("matrix", {})
    coverage = data.get("coverage", {})
    runtime_actions = data.get("runtimeActions", [])

    if broken:
        errors.append(f"qa-crawl broken must be empty, got {len(broken)} items")
    if remaining:
        errors.append(f"qa-crawl remaining must be empty, got {len(remaining)} items")

    themes = {str(item).lower() for item in matrix.get("themes", [])}
    if not EXPECTED_THEMES.issubset(themes):
        errors.append(f"matrix themes must include {sorted(EXPECTED_THEMES)}, got {sorted(themes)}")

    breakpoints = {int(item) for item in matrix.get("breakpoints", [])}
    if breakpoints != EXPECTED_BREAKPOINTS:
        errors.append(f"matrix breakpoints mismatch: expected {sorted(EXPECTED_BREAKPOINTS)}, got {sorted(breakpoints)}")

    keyboard_rows = matrix.get("keyboard", [])
    keyboard_viewports = {str(item.get('viewport', '')).lower() for item in keyboard_rows if isinstance(item, dict)}
    if keyboard_viewports != EXPECTED_KEYBOARD_VIEWPORTS:
        errors.append(
            f"keyboard matrix viewports mismatch: expected {sorted(EXPECTED_KEYBOARD_VIEWPORTS)}, got {sorted(keyboard_viewports)}"
        )
    for row in keyboard_rows:
        if not isinstance(row, dict):
            continue
        if not (bool(row.get("enter")) and bool(row.get("tab")) and bool(row.get("esc"))):
            errors.append(f"keyboard row has non-pass state: {row}")

    if len(runtime_actions) < 20:
        errors.append(f"runtime action trail is too small: expected >=20 actions, got {len(runtime_actions)}")

    missing_categories = sorted(cat for cat in REQUIRED_RUNTIME_CATEGORIES if cat not in coverage)
    if missing_categories:
        errors.append(f"missing runtime coverage categories: {', '.join(missing_categories)}")

    for category in REQUIRED_RUNTIME_CATEGORIES:
        row = coverage.get(category)
        if not isinstance(row, dict):
            continue
        if int(row.get("total", 0)) <= 0:
            errors.append(f"coverage category has zero total: {category}")
        if int(row.get("fail", 0)) != 0:
            errors.append(f"coverage category has failures: {category} -> {row.get('fail')}")


def check_docs(errors: list[str], scenario_ids: set[str]) -> None:
    scenario_doc = read_text(SCENARIO_DOC)
    qa_doc = read_text(QA_DOC)
    layout_doc = read_text(LAYOUT_DOC)
    heuristic_doc = read_text(HEURISTIC_DOC)
    evidence_doc = read_text(EVIDENCE_INDEX_DOC)

    for snippet in (
        "P0: must pass each commit",
        "P1: must pass each phase",
        "P2: must pass before release",
        "| ID | Phase | Severity | Steps | Expected | Theme A/B | Breakpoints | Keyboard Notes | Automation | Status |",
    ):
        if snippet not in scenario_doc:
            errors.append(f"SCENARIO_SUITE.md missing snippet: {snippet}")

    for snippet in (
        "## Runtime Action Trail",
        "## Category Coverage",
        "## What Broke",
        "## What Remains",
    ):
        if snippet not in qa_doc:
            errors.append(f"QA_CRAWL_REPORT.md missing section: {snippet}")

    if "FAIL" in layout_doc:
        errors.append("LAYOUT_AUDIT.md contains FAIL")

    for snippet in ("### Shell: PASS", "### Search: PASS", "### Detail: PASS"):
        if snippet not in heuristic_doc:
            errors.append(f"HEURISTIC_REVIEWS.md missing pass gate: {snippet}")

    for snippet in (
        "# Scenario Evidence Index",
        "## Matrix",
        "## Screenshot Mapping",
    ):
        if snippet not in evidence_doc:
            errors.append(f"SCENARIO_EVIDENCE_INDEX.md missing section: {snippet}")

    ids_missing = []
    for scenario_id in sorted(scenario_ids):
        marker = f"| {scenario_id} |"
        if marker not in evidence_doc:
            ids_missing.append(scenario_id)
    if ids_missing:
        errors.append("SCENARIO_EVIDENCE_INDEX.md missing scenario rows: " + ", ".join(ids_missing))


def main() -> int:
    errors: list[str] = []

    for required in (
        SCENARIO_ARTIFACT,
        QA_CRAWL_ARTIFACT,
        SCENARIO_DOC,
        QA_DOC,
        LAYOUT_DOC,
        HEURISTIC_DOC,
        EVIDENCE_INDEX_DOC,
    ):
        require_file(required, errors)

    if errors:
        print("[scenario-proof-gate] FAIL")
        for issue in errors:
            print(f"- {issue}")
        return 2

    scenario_data = read_json(SCENARIO_ARTIFACT)
    qa_data = read_json(QA_CRAWL_ARTIFACT)
    scenario_ids = {
        str(item.get("id")).strip()
        for item in scenario_data.get("results", [])
        if isinstance(item, dict) and item.get("id")
    }

    check_scenario_report(scenario_data, errors)
    check_qa_crawl(qa_data, errors)
    check_docs(errors, scenario_ids)

    if errors:
        print("[scenario-proof-gate] FAIL")
        for issue in errors:
            print(f"- {issue}")
        return 2

    summary = {
        "scenarioTotal": scenario_data.get("summary", {}).get("total"),
        "scenarioPass": scenario_data.get("summary", {}).get("pass"),
        "qaBroken": len(qa_data.get("broken", [])),
        "qaRemaining": len(qa_data.get("remaining", [])),
        "runtimeActions": len(qa_data.get("runtimeActions", [])),
        "coverageCategories": len(qa_data.get("coverage", {})),
    }
    print("[scenario-proof-gate] PASS")
    print(json.dumps(summary, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    sys.exit(main())
