from __future__ import annotations

import json

from config import FRONTEND_TIMER_TEST_PROFILE, LOCK_TTL
from repo.settings_repo import SettingsRepo


DEFAULT_REQUIRED_FIELDS = [
    "/basic/date",
    "/basic/time",
    "/basic/timezone",
    "/basic/equipment",
    "/basic/OBSERVER_FULLNAME",
    "/basic/OBSERVED_FULLNAME",
    "/basic/LOCATION_KEY",
    "/basic/LPC_KEY",
    "/basic/PROF_KEY",
]

DEFAULT_UPLOAD_POLICY = {
    "maxSizeMb": 10,
    "allowedMime": [
        "image/jpeg",
        "image/png",
        "application/pdf",
        "application/msword",
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "application/vnd.ms-excel",
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        "text/plain",
        "text/csv",
        "audio/mpeg",
        "audio/mp3",
        "audio/wav",
        "audio/x-wav",
        "audio/wave",
        "audio/aac",
        "audio/ogg",
        "audio/mp4",
        "audio/x-m4a",
        "audio/flac",
        "audio/x-flac",
        "audio/webm",
    ],
    "allowedExtensions": ["jpg", "jpeg", "png", "pdf", "doc", "docx", "xls", "xlsx", "txt", "csv", "mp3", "wav", "m4a", "ogg", "aac", "flac", "webm"],
}

DEFAULT_FRONTEND_VARIABLES = {
    "fclBeginColumnPercent": 38,
}


def _normalize_string_list(values):
    seen = set()
    result = []
    for value in values or []:
        item = str(value or "").strip().lower()
        if not item or item in seen:
            continue
        seen.add(item)
        result.append(item)
    return result


def _normalize_upload_policy(raw_value):
    parsed = _load_json(raw_value, DEFAULT_UPLOAD_POLICY)
    if not isinstance(parsed, dict):
        parsed = {}
    allowed_mime = _normalize_string_list(DEFAULT_UPLOAD_POLICY.get("allowedMime")) + [
        item for item in _normalize_string_list(parsed.get("allowedMime")) if item not in _normalize_string_list(DEFAULT_UPLOAD_POLICY.get("allowedMime"))
    ]
    allowed_extensions = _normalize_string_list(DEFAULT_UPLOAD_POLICY.get("allowedExtensions")) + [
        item for item in _normalize_string_list(parsed.get("allowedExtensions")) if item not in _normalize_string_list(DEFAULT_UPLOAD_POLICY.get("allowedExtensions"))
    ]
    try:
        max_size_mb = float(parsed.get("maxSizeMb") or DEFAULT_UPLOAD_POLICY["maxSizeMb"])
    except Exception:
        max_size_mb = float(DEFAULT_UPLOAD_POLICY["maxSizeMb"])
    if max_size_mb <= 0:
        max_size_mb = float(DEFAULT_UPLOAD_POLICY["maxSizeMb"])
    max_size_mb = min(float(DEFAULT_UPLOAD_POLICY["maxSizeMb"]), max_size_mb)
    return {
        "maxSizeMb": int(max_size_mb) if float(max_size_mb).is_integer() else max_size_mb,
        "allowedMime": allowed_mime,
        "allowedExtensions": allowed_extensions,
    }


def _load_json(raw_value, fallback):
    try:
        parsed = json.loads(str(raw_value or "").strip() or json.dumps(fallback))
    except Exception:
        parsed = fallback
    return parsed if parsed is not None else fallback


def _merge_defaults(raw_value, fallback):
    parsed = _load_json(raw_value, fallback)
    if not isinstance(fallback, dict) or not isinstance(parsed, dict):
        return parsed
    merged = dict(fallback)
    merged.update(parsed)
    return merged


class SettingsService:
    GLOBAL_CACHE_TOLERANCE_MS = 5500

    @staticmethod
    def load_global(settings_repo: SettingsRepo) -> dict:
        row = settings_repo.get_global()
        required_fields = _load_json(getattr(row, "required_fields_json", ""), DEFAULT_REQUIRED_FIELDS)
        upload_policy = _normalize_upload_policy(getattr(row, "upload_policy_json", ""))
        return {
            "Key": "GLOBAL",
            "CacheToleranceMs": SettingsService.GLOBAL_CACHE_TOLERANCE_MS,
            "HeartbeatIntervalSec": max(1, int((row.heartbeat_ms or FRONTEND_TIMER_TEST_PROFILE["heartbeat_ms"]) / 1000)),
            "StatusPollIntervalSec": max(1, int((row.lock_status_ms or FRONTEND_TIMER_TEST_PROFILE["lock_status_ms"]) / 1000)),
            "LockPollingIntervalSec": max(1, int((row.lock_status_ms or FRONTEND_TIMER_TEST_PROFILE["lock_status_ms"]) / 1000)),
            "LockTtlSec": max(1, int(LOCK_TTL.total_seconds())),
            "IdleTimeoutSec": max(1, int((row.idle_ms or FRONTEND_TIMER_TEST_PROFILE["idle_ms"]) / 1000)),
            "AutoSaveDebounceMs": int(row.autosave_debounce_ms or FRONTEND_TIMER_TEST_PROFILE["autosave_debounce_ms"]),
            "AutoSaveIntervalMs": int(row.autosave_interval_ms or FRONTEND_TIMER_TEST_PROFILE["autosave_interval_ms"]),
            "GcdIntervalMs": int(row.gcd_ms or FRONTEND_TIMER_TEST_PROFILE["gcd_ms"]),
            "NetworkGraceMs": int(row.network_grace_ms or FRONTEND_TIMER_TEST_PROFILE["network_grace_ms"]),
            "CacheFreshMs": int(row.cache_fresh_ms or FRONTEND_TIMER_TEST_PROFILE["cache_fresh_ms"]),
            "CacheStaleOkMs": int(row.cache_stale_ok_ms or FRONTEND_TIMER_TEST_PROFILE["cache_stale_ok_ms"]),
            "AnalyticsRefreshMs": int(row.analytics_refresh_ms or FRONTEND_TIMER_TEST_PROFILE["analytics_refresh_ms"]),
            "RequiredFieldsJson": json.dumps(required_fields),
            "FrontendVariablesJson": json.dumps(DEFAULT_FRONTEND_VARIABLES),
            "UploadPolicyJson": json.dumps(upload_policy),
        }
