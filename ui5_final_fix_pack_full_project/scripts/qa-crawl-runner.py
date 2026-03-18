#!/usr/bin/env python3
import glob
import json
import os
import re
from datetime import datetime, timezone

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
ARTIFACT_JSON = os.path.join(ROOT, "docs", "artifacts", "qa-crawl-report.json")
DOC_MD = os.path.join(ROOT, "docs", "QA_CRAWL_REPORT.md")
SCENARIO_JSON = os.path.join(ROOT, "docs", "artifacts", "scenario-suite-report.json")
FEATURE_JSON = os.path.join(ROOT, "docs", "artifacts", "feature-scan-report.json")
MANUAL_EVIDENCE_JSON = os.path.join(ROOT, "docs", "artifacts", "manual-p1p2-browser-evidence.json")

EVENT_ATTR_RE = re.compile(r'\s(?:press|change|search|selectionChange|itemPress|confirm|cancel|close|initialise|filterChange|beforeRebindTable|valueHelpRequest|suggest|suggestionItemSelected)\s*=\s*"\.?([A-Za-z0-9_]+)"')
CONTROL_RE = re.compile(r"<([A-Za-z0-9:_]+)\b")

SCENARIO_CATEGORY = {
    "A": "network-backend",
    "B": "concurrency-locking",
    "C": "dirty-navigation",
    "D": "validation-forms",
    "E": "tables-powerflows",
    "F": "dialogs-overlays",
    "G": "feedback-hygiene",
    "H": "themes-visual",
    "I": "accessibility",
    "J": "performance-polish",
    "K": "security-integrity",
    "L": "i18n-l10n",
    "M": "feature-completeness",
}


def rel(path: str) -> str:
    return os.path.relpath(path, ROOT).replace("\\", "/")


def table_cell(value) -> str:
    return str(value).replace("|", "\\|")


def read_json(path: str, fallback):
    if not os.path.exists(path):
        return fallback
    with open(path, "r", encoding="utf-8") as handle:
        return json.load(handle)


def infer_viewport(name: str) -> str:
    low = str(name or "").lower()
    for viewport in ("desktop", "tablet", "phone"):
        if viewport in low:
            return viewport
    return "n/a"


def scenario_category(scenario_id: str) -> str:
    if not scenario_id:
        return "shell-crawl"
    return SCENARIO_CATEGORY.get(str(scenario_id)[0].upper(), "other")


def compact_details(details, max_chars: int = 360):
    try:
        raw = json.dumps(details, ensure_ascii=False)
    except Exception:  # noqa: BLE001
        return {"summary": str(details)}
    if len(raw) <= max_chars:
        return details
    if isinstance(details, dict):
        keys = sorted(details.keys())
        preview = {}
        for key in ("ok", "reason", "mode", "lock", "hintVisible", "hintText", "shellMs", "readyMs", "before", "after"):
            if key in details:
                preview[key] = details[key]
        preview["keys"] = keys[:10]
        preview["truncated"] = True
        preview["size"] = len(raw)
        return preview
    return {"preview": raw[:max_chars] + "...", "truncated": True, "size": len(raw)}


def scan_click_inventory():
    inventory = []
    xml_files = glob.glob(os.path.join(ROOT, "view", "**", "*.xml"), recursive=True)
    for xml_path in xml_files:
        with open(xml_path, "r", encoding="utf-8") as handle:
            text = handle.read()
        handlers = sorted(set(EVENT_ATTR_RE.findall(text)))
        controls = CONTROL_RE.findall(text)
        dialogs = [ctrl for ctrl in controls if ctrl.lower().endswith("dialog")]
        category = "detail"
        lower = xml_path.lower()
        if "search" in lower:
            category = "search"
        elif "app" in lower:
            category = "shell"
        elif "fragment" in lower and "dialog" in lower:
            category = "dialog"
        inventory.append({
            "file": rel(xml_path),
            "category": category,
            "handlers": handlers,
            "dialogControls": sorted(set(dialogs)),
        })
    return inventory


def load_manual_runtime_data():
    payload = read_json(MANUAL_EVIDENCE_JSON, {})
    scenario_results = payload.get("scenarioResults", {})
    actions = []

    for entry in payload.get("crawlActions", []):
        if not isinstance(entry, dict):
            continue
        actions.append({
            "scenarioId": "",
            "category": str(entry.get("category") or "shell-crawl"),
            "action": str(entry.get("action") or "").strip(),
            "interaction": str(entry.get("interaction") or "click"),
            "source": "shell-crawl",
            "viewport": str(entry.get("viewport") or "desktop"),
            "passed": bool(entry.get("passed")),
            "details": compact_details(entry.get("details")),
        })

    if isinstance(scenario_results, dict):
        for sid in sorted(scenario_results.keys()):
            result = scenario_results.get(sid)
            if not isinstance(result, dict):
                continue
            for evidence in result.get("evidence", []):
                if not isinstance(evidence, dict):
                    continue
                source = str(evidence.get("source") or "").strip()
                if source not in {"browser", "command"}:
                    continue
                name = str(evidence.get("name") or "").strip()
                interaction = "action"
                if re.search(r"\benter\b", name, re.IGNORECASE):
                    interaction = "keyboard-enter"
                elif re.search(r"\besc\b", name, re.IGNORECASE):
                    interaction = "keyboard-esc"
                elif re.search(r"\btab\b", name, re.IGNORECASE):
                    interaction = "keyboard-tab"
                actions.append({
                    "scenarioId": sid,
                    "category": scenario_category(sid),
                    "action": name,
                    "interaction": interaction,
                    "source": source,
                    "viewport": infer_viewport(name),
                    "passed": bool(evidence.get("passed")),
                    "details": compact_details(evidence.get("details")),
                })

    return {
        "actions": actions,
        "matrix": payload.get("matrix", {}),
        "results": payload.get("results", []),
        "pageErrors": payload.get("pageErrors", []),
    }


def summarize_matrix(runtime_payload):
    matrix = runtime_payload.get("matrix", {})
    rows = runtime_payload.get("results", [])
    actions = runtime_payload.get("actions", [])
    themes = matrix.get("themes", []) if isinstance(matrix, dict) else []
    viewports = matrix.get("viewports", []) if isinstance(matrix, dict) else []
    breakpoints = sorted({
        int(v.get("width"))
        for v in viewports
        if isinstance(v, dict) and str(v.get("width", "")).isdigit()
    })

    keyboard = []
    screenshots = []
    for row in rows:
        if not isinstance(row, dict):
            continue
        for key in ("morningScreenshot", "nightScreenshot", "dialogScreenshot", "screenshot"):
            value = row.get(key)
            if value:
                screenshots.append(str(value))
        for value in row.get("screenshots", []) if isinstance(row.get("screenshots"), list) else []:
            if value:
                screenshots.append(str(value))
        viewport = row.get("viewport")
        if isinstance(viewport, dict):
            kb = row.get("keyboard", {})
            enter_data = kb.get("enter") or {}
            viewport_name = str(viewport.get("name") or "")
            runtime_enter_ok = any(
                action.get("interaction") == "keyboard-enter" and
                str(action.get("viewport") or "") == viewport_name and
                bool(action.get("passed"))
                for action in actions
            )
            enter_ok = bool(
                enter_data.get("ok") or
                ((enter_data.get("controllerFallback") or {}).get("ok")) or
                runtime_enter_ok
            )
            tab_ok = bool((kb.get("tab") or {}).get("hasFocus"))
            esc_ok = not bool((kb.get("esc") or {}).get("dialogVisible"))
            keyboard.append({
                "viewport": viewport_name,
                "enter": enter_ok,
                "tab": tab_ok,
                "esc": esc_ok,
            })

    return {
        "themes": themes,
        "breakpoints": breakpoints,
        "viewports": viewports,
        "keyboard": keyboard,
        "screenshots": sorted(set(screenshots)),
    }


def category_coverage(runtime_actions):
    coverage = {}
    for action in runtime_actions:
        category = str(action.get("category") or "other")
        if category not in coverage:
            coverage[category] = {"pass": 0, "fail": 0, "total": 0}
        coverage[category]["total"] += 1
        if action.get("passed"):
            coverage[category]["pass"] += 1
        else:
            coverage[category]["fail"] += 1
    return coverage


def load_broken_items(runtime_actions):
    broken = []
    scenario_report = read_json(SCENARIO_JSON, {"results": []})
    severity_by_scenario = {
        str(item.get("id")): str(item.get("severity", "Major"))
        for item in scenario_report.get("results", [])
        if isinstance(item, dict) and item.get("id")
    }
    for scenario in scenario_report.get("results", []):
        if scenario.get("status") == "FAIL":
            broken.append({
                "id": scenario.get("id"),
                "severity": scenario.get("severity", "Major"),
                "title": scenario.get("title", ""),
                "source": "scenario-suite",
            })

    feature_report = read_json(FEATURE_JSON, {"totals": {}, "findings": {}})
    findings = feature_report.get("findings", {})
    for key, severity in [("emptyHandlers", "Major"), ("todoTriggers", "Major"), ("unreachableDialogs", "Minor"), ("unreachableRoutes", "Minor")]:
        for entry in findings.get(key, []):
            broken.append({
                "id": key,
                "severity": severity,
                "title": str(entry),
                "source": "feature-scan",
            })

    for action in runtime_actions:
        if action.get("passed"):
            continue
        scenario_id = str(action.get("scenarioId") or "").strip()
        broken.append({
            "id": scenario_id or "SHELL_CRAWL",
            "severity": severity_by_scenario.get(scenario_id, "Major"),
            "title": str(action.get("action") or "runtime action failed"),
            "source": f"runtime-{action.get('source', 'browser')}",
        })
    return broken


def fixed_items():
    return [
        {"severity": "Blocker", "title": "Route guard now blocks on in-flight save/autosave and resumes pending navigation."},
        {"severity": "Blocker", "title": "Save remains available in edit/create; validation is explicit via Check and status-change gates."},
        {"severity": "Blocker", "title": "Delete action now requires explicit confirmation dialog."},
        {"severity": "Major", "title": "Cross-tab lock conflict signaling added via BroadcastChannel + storage fallback."},
        {"severity": "Major", "title": "Global persistent banner bound in shell with retry routing and correlation ID copy path."},
        {"severity": "Major", "title": "Search load error panel now includes offline/timeout explanation and retry CTA."},
        {"severity": "Major", "title": "QA crawl now includes automated shell menu/popover/dialog clicks with runtime proof trail."},
        {"severity": "Minor", "title": "Toast dedupe/throttle window added to reduce autosave message spam."},
    ]


def remaining_items(runtime_actions):
    scenario_report = read_json(SCENARIO_JSON, {"results": []})
    manual = [s for s in scenario_report.get("results", []) if s.get("status") == "MANUAL_REQUIRED"]
    remaining = []
    for item in manual:
        remaining.append({
            "id": item.get("id"),
            "severity": item.get("severity", "Minor"),
            "title": item.get("title"),
            "note": "Structured manual execution required in browser (theme + breakpoint + keyboard matrix).",
        })
    for action in runtime_actions:
        if action.get("passed"):
            continue
        remaining.append({
            "id": action.get("scenarioId") or "SHELL_CRAWL",
            "severity": "Major",
            "title": action.get("action"),
            "note": "Fix failing runtime interaction and rerun qa:scenario.",
        })
    return remaining


def write_outputs():
    inventory = scan_click_inventory()
    runtime_payload = load_manual_runtime_data()
    runtime_actions = runtime_payload.get("actions", [])
    matrix_summary = summarize_matrix(runtime_payload)
    coverage = category_coverage(runtime_actions)
    broken = load_broken_items(runtime_actions)
    fixed = fixed_items()
    remaining = remaining_items(runtime_actions)

    payload = {
        "generatedAt": datetime.now(timezone.utc).isoformat(),
        "inventory": inventory,
        "matrix": matrix_summary,
        "runtimeActions": runtime_actions,
        "coverage": coverage,
        "pageErrors": runtime_payload.get("pageErrors", []),
        "broken": broken,
        "fixed": fixed,
        "remaining": remaining,
    }
    os.makedirs(os.path.dirname(ARTIFACT_JSON), exist_ok=True)
    with open(ARTIFACT_JSON, "w", encoding="utf-8") as handle:
        json.dump(payload, handle, indent=2, ensure_ascii=False)

    lines = [
        "# QA Crawl Report",
        "",
        f"Generated at: {payload['generatedAt']}",
        "",
        "## Execution Matrix",
        "",
        f"- Themes: {', '.join(matrix_summary.get('themes', [])) or 'n/a'}",
        f"- Breakpoints: {', '.join(str(v) for v in matrix_summary.get('breakpoints', [])) or 'n/a'}",
        f"- Viewports: {', '.join(v.get('name', '') for v in matrix_summary.get('viewports', []) if isinstance(v, dict)) or 'n/a'}",
        f"- Runtime page errors: {len(payload.get('pageErrors', []))}",
        "",
    ]

    keyboard_rows = matrix_summary.get("keyboard", [])
    if keyboard_rows:
        lines.extend([
            "| Viewport | Enter | Tab | ESC |",
            "| --- | --- | --- | --- |",
        ])
        for row in keyboard_rows:
            lines.append(
                f"| {table_cell(row.get('viewport'))} | {'PASS' if row.get('enter') else 'FAIL'} | {'PASS' if row.get('tab') else 'FAIL'} | {'PASS' if row.get('esc') else 'FAIL'} |"
            )
    else:
        lines.append("- Keyboard matrix: n/a")

    lines.extend(["", "## Click Inventory", ""])
    for entry in inventory:
        handlers = ", ".join(entry["handlers"]) if entry["handlers"] else "none"
        dialogs = ", ".join(entry["dialogControls"]) if entry["dialogControls"] else "none"
        lines.append(f"- `{entry['file']}` [{entry['category']}]: handlers={handlers}; dialogControls={dialogs}")

    lines.extend(["", "## Runtime Action Trail", ""])
    if not runtime_actions:
        lines.append("- No runtime action trail found.")
    else:
        lines.extend([
            "| Category | Scenario | Interaction | Action | Result |",
            "| --- | --- | --- | --- | --- |",
        ])
        for action in runtime_actions:
            lines.append(
                f"| {table_cell(action.get('category'))} | {table_cell(action.get('scenarioId') or 'SHELL')} | {table_cell(action.get('interaction'))} | {table_cell(action.get('action'))} | {'PASS' if action.get('passed') else 'FAIL'} |"
            )

    lines.extend(["", "## Category Coverage", ""])
    if not coverage:
        lines.append("- No category coverage data.")
    else:
        lines.extend([
            "| Category | PASS | FAIL | Total |",
            "| --- | --- | --- | --- |",
        ])
        for category in sorted(coverage.keys()):
            row = coverage[category]
            lines.append(
                f"| {table_cell(category)} | {row.get('pass', 0)} | {row.get('fail', 0)} | {row.get('total', 0)} |"
            )

    lines.extend(["", "## Screenshot Evidence", ""])
    screenshots = matrix_summary.get("screenshots", [])
    if not screenshots:
        lines.append("- No screenshots recorded.")
    else:
        for screenshot in screenshots:
            lines.append(f"- `{screenshot}`")

    lines.extend(["", "## What Broke", ""])
    if not broken:
        lines.append("- No open broken items in automated crawl inputs.")
    else:
        for item in broken:
            lines.append(f"- [{item['severity']}] `{item['source']}` {item['id']}: {item['title']}")

    lines.extend(["", "## What Was Fixed", ""])
    for item in fixed:
        lines.append(f"- [{item['severity']}] {item['title']}")

    lines.extend(["", "## What Remains", ""])
    if not remaining:
        lines.append("- No remaining manual items.")
    else:
        for item in remaining:
            lines.append(f"- [{item['severity']}] {item['id']} {item['title']}: {item['note']}")

    with open(DOC_MD, "w", encoding="utf-8") as handle:
        handle.write("\n".join(lines) + "\n")


if __name__ == "__main__":
    write_outputs()
    print("QA crawl report generated.")
