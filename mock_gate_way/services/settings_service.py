from __future__ import annotations

import json

from repo.settings_repo import SettingsRepo


class SettingsService:
    GLOBAL_CACHE_TOLERANCE_MS = 5500

    @staticmethod
    def load_global(settings_repo: SettingsRepo) -> dict:
        row = settings_repo.get_global()
        return {
            "Key": "GLOBAL",
            "CacheToleranceMs": SettingsService.GLOBAL_CACHE_TOLERANCE_MS,
            "HeartbeatIntervalSec": max(1, int((row.heartbeat_ms or 240000) / 1000)),
            "StatusPollIntervalSec": max(1, int((row.lock_status_ms or 60000) / 1000)),
            "LockTtlSec": 300,
            "IdleTimeoutSec": max(1, int((row.idle_ms or 600000) / 1000)),
            "AutoSaveDebounceMs": int(row.autosave_debounce_ms or 1200),
            "RequiredFieldsJson": json.dumps([
                "/basic/date", "/basic/time", "/basic/timezone", "/basic/OBSERVER_FULLNAME",
                "/basic/OBSERVED_FULLNAME", "/basic/LOCATION_KEY", "/basic/LPC_KEY", "/basic/PROF_KEY",
            ]),
            "UploadPolicyJson": json.dumps({"maxSizeMb": 15, "allowedMime": ["image/jpeg", "image/png", "application/pdf"]}),
        }
