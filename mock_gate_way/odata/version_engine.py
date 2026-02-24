from __future__ import annotations

from fastapi import HTTPException

from state import db, iso, now


def ensure_root(root_id: str, payload: dict | None = None) -> dict:
    root = db["roots"].get(root_id)
    if root:
        return root

    created_at = now()
    root = {
        "id": root_id,
        "version_number": 1,
        "changed_on": created_at,
        "created_on": created_at,
        "data": payload or {},
    }
    db["roots"][root_id] = root
    return root


def validate_client_version(root: dict, client_version: int) -> None:
    if root["version_number"] != client_version:
        raise HTTPException(
            status_code=409,
            detail={
                "code": "CONFLICT",
                "message": "Version mismatch",
                "server_version": root["version_number"],
            },
        )


def bump_version(root: dict) -> dict:
    root["version_number"] += 1
    root["changed_on"] = now()
    return {
        "root_id": root["id"],
        "version_number": root["version_number"],
        "changed_on": iso(root["changed_on"]),
    }
