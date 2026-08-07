import json


def build_sap_message(
    main_message: str, severity: str = "info", details: list[dict] | None = None, code: str = ""
) -> str:
    payload = {
        "code": code,
        "message": str(main_message or ""),
        "severity": str(severity or "info"),
        "details": [],
    }
    for item in details or []:
        payload["details"].append(
            {
                "code": str(item.get("code") or ""),
                "message": str(item.get("message") or ""),
                "severity": str(item.get("severity") or "info"),
                "target": str(item.get("target") or ""),
            }
        )
    return json.dumps(payload, ensure_ascii=False, separators=(",", ":"))
