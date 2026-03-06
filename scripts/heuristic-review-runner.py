#!/usr/bin/env python3
import json
import os
from datetime import datetime, timezone

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
ARTIFACT_JSON = os.path.join(ROOT, "docs", "artifacts", "heuristic-review-report.json")
DOC_MD = os.path.join(ROOT, "docs", "HEURISTIC_REVIEWS.md")

GATES = [
    {
        "gate": "Shell",
        "checks": [
            ("view/App.view.xml", "globalFeedbackBanner", "Global banner rendered in shell"),
            ("controller/support/AppControllerShellActions.js", "onGlobalBannerRetry", "Retry action routing present"),
            ("controller/support/AppControllerOverlayActions.js", "_restoreShellOverlayFocus", "Focus return on shell overlays")
        ]
    },
    {
        "gate": "Search",
        "checks": [
            ("view/fragment/SearchLoadStatePanel.fragment.xml", "onRetrySearchLoad", "Search retry CTA present"),
            ("controller/Search.controller.js", "onRetrySearchLoad", "Search retry handler implemented"),
            ("controller/support/SearchViewSupport.js", "workingMessageLong", "Long-running search messaging present")
        ]
    },
    {
        "gate": "Detail",
        "checks": [
            ("view/fragment/DetailControlRail.fragment.xml", "press=\".onValidateChecklist\"", "Validation remains explicit on demand"),
            ("controller/support/DetailChecklistStateActions.js", "EffectApplier.actions.DELETE", "Dangerous action confirmation"),
            ("service/domain/detail/usecases/ChangeStatusUseCase.js", "checklistValidationFailedToast", "Status change is validation-gated")
        ]
    }
]


def read(rel_path: str) -> str:
    abs_path = os.path.join(ROOT, rel_path)
    if not os.path.exists(abs_path):
        return ""
    with open(abs_path, "r", encoding="utf-8") as handle:
        return handle.read()


def run_gate(gate):
    checks = []
    for rel_path, pattern, label in gate["checks"]:
        passed = pattern in read(rel_path)
        checks.append({
            "path": rel_path,
            "pattern": pattern,
            "label": label,
            "passed": passed
        })
    return {
        "gate": gate["gate"],
        "checks": checks,
        "passed": all(check["passed"] for check in checks)
    }


def main():
    results = [run_gate(gate) for gate in GATES]
    report = {
        "generatedAt": datetime.now(timezone.utc).isoformat(),
        "results": results,
        "summary": {
            "totalGates": len(results),
            "passedGates": len([result for result in results if result["passed"]]),
            "failedGates": len([result for result in results if not result["passed"]])
        }
    }

    os.makedirs(os.path.dirname(ARTIFACT_JSON), exist_ok=True)
    with open(ARTIFACT_JSON, "w", encoding="utf-8") as handle:
        json.dump(report, handle, indent=2)

    lines = [
        "# Heuristic Reviews",
        "",
        f"Generated at: {report['generatedAt']}",
        "",
        "## Gate Results",
        ""
    ]
    for gate in results:
        lines.append(f"### {gate['gate']}: {'PASS' if gate['passed'] else 'FAIL'}")
        for check in gate["checks"]:
            lines.append(
                f"- {'PASS' if check['passed'] else 'FAIL'} `{check['path']}` pattern `{check['pattern']}`: {check['label']}"
            )
        lines.append("")

    with open(DOC_MD, "w", encoding="utf-8") as handle:
        handle.write("\n".join(lines) + "\n")

    print("Heuristic review report generated.")


if __name__ == "__main__":
    main()
