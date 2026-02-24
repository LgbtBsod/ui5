from __future__ import annotations

from state import db, now


def apply_delta(root_id: str, root_delta: dict | None = None, checks_delta: list[dict] | None = None) -> dict:
    root = db["roots"].setdefault(
        root_id,
        {
            "id": root_id,
            "version_number": 1,
            "changed_on": now(),
            "created_on": now(),
            "data": {},
        },
    )

    if root_delta:
        root["data"].update(root_delta)

    if checks_delta:
        for row in checks_delta:
            check_id = row["id"]
            mode = row.get("edit_mode", "U")
            bucket = db["checks"]
            if mode == "D":
                bucket.pop(check_id, None)
                continue

            existing = bucket.get(check_id, {"id": check_id, "root_id": root_id})
            patch = {k: v for k, v in row.items() if k not in {"edit_mode"}}
            existing.update(patch)
            existing["root_id"] = root_id
            bucket[check_id] = existing

    root["changed_on"] = now()
    return root
