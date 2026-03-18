#!/usr/bin/env python3
import json
import os
from datetime import datetime, timezone

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
ARTIFACT_JSON = os.path.join(ROOT, "docs", "artifacts", "layout-audit-report.json")
DOC_MD = os.path.join(ROOT, "docs", "LAYOUT_AUDIT.md")

THEMES = ["Morning", "Night"]
BREAKPOINTS = [1440, 1080, 720]
SCREENS = [
    {
        "id": "shell",
        "file": "view/App.view.xml",
        "checks": [
            ("f:FlexibleColumnLayout", "FlexibleColumnLayout host exists"),
            ("AppShellHeader", "Shell header exists"),
            ("searchPaneHost", "Search pane anchor exists"),
            ("detailPaneHost", "Detail pane anchor exists")
        ]
    },
    {
        "id": "search",
        "file": "view/Search.view.xml",
        "checks": [
            ("searchExperienceStack", "Search shell stack class exists"),
            ("smartFilterBar:SmartFilterBar", "SmartFilterBar exists"),
            ("smartTable:SmartTable", "SmartTable exists"),
            ("searchAnalyticsRail", "Analytics rail exists")
        ]
    },
    {
        "id": "detail",
        "file": "view/Detail.view.xml",
        "checks": [
            ("uxap:ObjectPageLayout", "ObjectPageLayout exists"),
            ("detailControlStickyHost", "Control rail sticky host exists"),
            ("LockKilledBanner", "Lock killed banner fragment exists"),
            ("detailSectionCard", "Section card class coverage exists")
        ]
    }
]


def read(rel_path: str) -> str:
    abs_path = os.path.join(ROOT, rel_path)
    if not os.path.exists(abs_path):
        return ""
    with open(abs_path, "r", encoding="utf-8") as handle:
        return handle.read()


def audit_screen(screen):
    content = read(screen["file"])
    results = []
    for pattern, label in screen["checks"]:
        results.append({
            "label": label,
            "pattern": pattern,
            "passed": pattern in content
        })
    return {
        "screen": screen["id"],
        "file": screen["file"],
        "checks": results,
        "passed": all(item["passed"] for item in results)
    }


def main():
    audits = []
    for theme in THEMES:
        for breakpoint in BREAKPOINTS:
            for screen in SCREENS:
                result = audit_screen(screen)
                result["theme"] = theme
                result["breakpoint"] = breakpoint
                audits.append(result)

    report = {
        "generatedAt": datetime.now(timezone.utc).isoformat(),
        "themes": THEMES,
        "breakpoints": BREAKPOINTS,
        "audits": audits,
        "summary": {
            "total": len(audits),
            "passed": len([item for item in audits if item["passed"]]),
            "failed": len([item for item in audits if not item["passed"]])
        }
    }

    os.makedirs(os.path.dirname(ARTIFACT_JSON), exist_ok=True)
    with open(ARTIFACT_JSON, "w", encoding="utf-8") as handle:
        json.dump(report, handle, indent=2)

    lines = [
        "# Layout Audit",
        "",
        f"Generated at: {report['generatedAt']}",
        "",
        "Checklist basis: shell, search, detail structure checks per theme and breakpoint.",
        "",
        "| Screen | Theme | Breakpoint | Result | Checklist |",
        "| --- | --- | --- | --- | --- |"
    ]

    for item in audits:
        checklist = "; ".join(
            [f"{entry['label']}={'PASS' if entry['passed'] else 'FAIL'}" for entry in item["checks"]]
        )
        lines.append(
            f"| {item['screen']} | {item['theme']} | {item['breakpoint']} | "
            f"{'PASS' if item['passed'] else 'FAIL'} | {checklist} |"
        )

    with open(DOC_MD, "w", encoding="utf-8") as handle:
        handle.write("\n".join(lines) + "\n")

    print("Layout audit generated.")


if __name__ == "__main__":
    main()
