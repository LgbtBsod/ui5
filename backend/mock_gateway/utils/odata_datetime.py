from __future__ import annotations

import re
from datetime import date, datetime, timezone


def to_odata_date_ms(dt_utc: datetime) -> str:
    dt = dt_utc if dt_utc.tzinfo else dt_utc.replace(tzinfo=timezone.utc)
    ms = int(dt.astimezone(timezone.utc).timestamp() * 1000)
    return f"/Date({ms})/"


def from_odata_date_ms(raw: str) -> datetime:
    m = re.fullmatch(r"/Date\((-?\d+)(?:[+-]\d+)?\)/", str(raw or "").strip())
    if not m:
        raise ValueError("Invalid OData DateTime")
    return datetime.fromtimestamp(int(m.group(1)) / 1000, tz=timezone.utc)


def date_only_to_odata(value: date) -> str:
    dt = datetime(value.year, value.month, value.day, tzinfo=timezone.utc)
    return to_odata_date_ms(dt)


def normalize_date_only(dt: datetime) -> date:
    val = dt if dt.tzinfo else dt.replace(tzinfo=timezone.utc)
    return val.astimezone(timezone.utc).date()
