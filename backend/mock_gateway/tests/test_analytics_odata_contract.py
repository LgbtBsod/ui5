from __future__ import annotations

import os
import sys
from datetime import datetime, timezone

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from api.analytics_api import _to_refresh_state_row, _to_summary_row  # noqa: E402


def test_summary_row_serializes_refreshed_at_as_odata_datetime():
    row = _to_summary_row({
        "selectedYear": 2026,
        "source": "ALL",
        "refreshedAt": "2026-03-28T08:25:59Z",
    })

    assert row["RefreshedAt"] == "/Date(1774686359000)/"


def test_refresh_state_row_serializes_datetime_fields_as_odata_datetime():
    row = _to_refresh_state_row({
        "taskKey": "ANALYTICS_REFRESH",
        "requestedAt": datetime(2026, 3, 28, 8, 0, 0, tzinfo=timezone.utc),
        "startedAt": datetime(2026, 3, 28, 8, 1, 0, tzinfo=timezone.utc),
        "finishedAt": datetime(2026, 3, 28, 8, 2, 0, tzinfo=timezone.utc),
        "lastSuccessAt": datetime(2026, 3, 28, 8, 3, 0, tzinfo=timezone.utc),
    })

    assert row["RequestedAt"] == "/Date(1774684800000)/"
    assert row["StartedAt"] == "/Date(1774684860000)/"
    assert row["FinishedAt"] == "/Date(1774684920000)/"
    assert row["LastSuccessAt"] == "/Date(1774684980000)/"
