from __future__ import annotations

from datetime import date as date_type


def parse_iso_date(value: str) -> date_type:
    return date_type.fromisoformat(value)
