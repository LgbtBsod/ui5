from __future__ import annotations

from typing import Any

from state import db, iso


def query_checklist_flat(
    top: int = 20,
    skip: int = 0,
    status: str | None = None,
    sort_by: str = "changed_on",
    order: str = "desc",
) -> dict[str, Any]:
    rows = []
    for root in db["roots"].values():
        data = root.get("data", {})
        row = {
            "id": root["id"],
            "status": data.get("status", "NEW"),
            "title": data.get("title", root["id"]),
            "version_number": root["version_number"],
            "changed_on": iso(root["changed_on"]),
        }
        rows.append(row)

    if status:
        rows = [r for r in rows if r["status"] == status]

    reverse = order.lower() == "desc"
    rows = sorted(rows, key=lambda r: r.get(sort_by) or "", reverse=reverse)

    total = len(rows)
    paged = rows[skip : skip + top]
    return {
        "d": {
            "results": paged,
            "__count": str(total),
            "paging": {
                "$top": top,
                "$skip": skip,
                "returned": len(paged),
            },
        }
    }
