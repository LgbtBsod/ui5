#!/usr/bin/env python3
import json
import os
from datetime import datetime, timezone

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))

SCENARIO_REPORT = os.path.join(ROOT, "docs", "artifacts", "scenario-suite-report.json")
QA_CRAWL_REPORT = os.path.join(ROOT, "docs", "artifacts", "qa-crawl-report.json")
MANUAL_EVIDENCE = os.path.join(ROOT, "docs", "artifacts", "manual-p1p2-browser-evidence.json")
DOC_PATH = os.path.join(ROOT, "docs", "SCENARIO_EVIDENCE_INDEX.md")


def read_json(path: str):
    with open(path, "r", encoding="utf-8") as handle:
        return json.load(handle)


def table_cell(value) -> str:
    return str(value).replace("|", "\\|")


def collect_runtime_index(qa_payload: dict):
    by_scenario = {}
    for action in qa_payload.get("runtimeActions", []):
        if not isinstance(action, dict):
            continue
        scenario_id = str(action.get("scenarioId") or "").strip().upper()
        if not scenario_id:
            continue
        bucket = by_scenario.setdefault(scenario_id, {"count": 0, "categories": set()})
        bucket["count"] += 1
        category = str(action.get("category") or "").strip()
        if category:
            bucket["categories"].add(category)
    for key in list(by_scenario.keys()):
        by_scenario[key]["categories"] = sorted(by_scenario[key]["categories"])
    return by_scenario


def collect_screenshot_index(manual_payload: dict):
    by_scenario = {}
    for row in manual_payload.get("results", []):
        if not isinstance(row, dict):
            continue
        scenario = str(row.get("scenario") or "").strip().upper()
        if not scenario:
            continue
        bucket = by_scenario.setdefault(scenario, [])
        for key in ("screenshot", "morningScreenshot", "nightScreenshot", "dialogScreenshot"):
            value = row.get(key)
            if value:
                bucket.append(str(value))
        for value in row.get("screenshots", []) if isinstance(row.get("screenshots"), list) else []:
            if value:
                bucket.append(str(value))
    for key in list(by_scenario.keys()):
        by_scenario[key] = sorted(set(by_scenario[key]))
    return by_scenario


def evidence_sources(evidence_list):
    sources = set()
    for item in evidence_list or []:
        if not isinstance(item, dict):
            continue
        source = str(item.get("type") or item.get("source") or "").strip()
        if source:
            sources.add(source)
    return sorted(sources)


def write_doc(rows, summary):
    now = datetime.now(timezone.utc).isoformat()
    lines = [
        "# Scenario Evidence Index",
        "",
        f"Generated at: {now}",
        "",
        "Unified proof index for all scenario IDs (P0/P1/P2).",
        "",
        "## Summary",
        "",
        f"- Total scenarios: {summary['total']}",
        f"- PASS scenarios: {summary['pass']}",
        f"- Scenarios with missing evidence: {summary['missingEvidence']}",
        f"- Scenarios with runtime actions: {summary['withRuntime']}",
        f"- Scenarios with screenshot artifacts: {summary['withScreenshots']}",
        "",
        "## Matrix",
        "",
        "| ID | Phase | Severity | Status | Evidence Sources | Evidence Count | Runtime Actions | Runtime Categories | Screenshots |",
        "| --- | --- | --- | --- | --- | --- | --- | --- | --- |",
    ]

    for row in rows:
        lines.append(
            f"| {table_cell(row['id'])} | {table_cell(row['phase'])} | {table_cell(row['severity'])} | {table_cell(row['status'])} | "
            f"{table_cell(', '.join(row['sources']) if row['sources'] else 'none')} | {row['evidenceCount']} | {row['runtimeCount']} | "
            f"{table_cell(', '.join(row['runtimeCategories']) if row['runtimeCategories'] else 'none')} | {row['screenshotCount']} |"
        )

    lines.extend([
        "",
        "## Screenshot Mapping",
        "",
    ])

    mapped = [row for row in rows if row["screenshots"]]
    if not mapped:
        lines.append("- No scenario-linked screenshots recorded.")
    else:
        for row in mapped:
            lines.append(f"- {row['id']}: " + ", ".join(f"`{item}`" for item in row["screenshots"]))

    with open(DOC_PATH, "w", encoding="utf-8") as handle:
        handle.write("\n".join(lines) + "\n")


def main() -> int:
    for path in (SCENARIO_REPORT, QA_CRAWL_REPORT, MANUAL_EVIDENCE):
        if not os.path.exists(path):
            print(f"[scenario-evidence-index] FAIL missing input: {os.path.relpath(path, ROOT).replace('\\', '/')}")
            return 2

    scenario_payload = read_json(SCENARIO_REPORT)
    qa_payload = read_json(QA_CRAWL_REPORT)
    manual_payload = read_json(MANUAL_EVIDENCE)

    runtime_index = collect_runtime_index(qa_payload)
    screenshot_index = collect_screenshot_index(manual_payload)

    rows = []
    missing_evidence = []
    for item in scenario_payload.get("results", []):
        if not isinstance(item, dict):
            continue
        scenario_id = str(item.get("id") or "").strip().upper()
        evidence = item.get("evidence") or []
        sources = evidence_sources(evidence)
        runtime = runtime_index.get(scenario_id, {"count": 0, "categories": []})
        screenshots = screenshot_index.get(scenario_id, [])

        row = {
            "id": scenario_id,
            "phase": str(item.get("phase") or ""),
            "severity": str(item.get("severity") or ""),
            "status": str(item.get("status") or ""),
            "sources": sources,
            "evidenceCount": len(evidence),
            "runtimeCount": int(runtime.get("count", 0)),
            "runtimeCategories": list(runtime.get("categories", [])),
            "screenshotCount": len(screenshots),
            "screenshots": screenshots,
        }
        rows.append(row)
        if row["evidenceCount"] == 0:
            missing_evidence.append(scenario_id)

    rows.sort(key=lambda entry: entry["id"])
    summary = {
        "total": len(rows),
        "pass": len([row for row in rows if row["status"] == "PASS"]),
        "missingEvidence": len(missing_evidence),
        "withRuntime": len([row for row in rows if row["runtimeCount"] > 0]),
        "withScreenshots": len([row for row in rows if row["screenshotCount"] > 0]),
    }

    write_doc(rows, summary)

    if missing_evidence:
        print("[scenario-evidence-index] FAIL")
        print("missing evidence for scenarios: " + ", ".join(missing_evidence))
        return 2

    print("[scenario-evidence-index] PASS")
    print(json.dumps(summary, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
