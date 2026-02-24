from __future__ import annotations

from state import generate_uuid


def _is_temp_key(key: str) -> bool:
    return key.startswith("tmp_") or key.startswith("temp_")


def build_key_mapping(root_id: str, checks: list[dict] | None = None) -> dict:
    mapping: dict[str, str] = {}
    if _is_temp_key(root_id):
        mapping[root_id] = generate_uuid()

    for row in checks or []:
        check_id = row.get("id")
        if isinstance(check_id, str) and _is_temp_key(check_id):
            mapping[check_id] = generate_uuid()

    return mapping


def apply_mapping(root_id: str, checks: list[dict] | None, mapping: dict) -> tuple[str, list[dict]]:
    resolved_root_id = mapping.get(root_id, root_id)
    resolved_checks: list[dict] = []

    for row in checks or []:
        check_id = row.get("id")
        patched = dict(row)
        if check_id in mapping:
            patched["id"] = mapping[check_id]
        resolved_checks.append(patched)

    return resolved_root_id, resolved_checks
