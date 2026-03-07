from __future__ import annotations

import uuid


class CsrfStore:
    def __init__(self):
        self._tokens: dict[str, str] = {}

    def issue(self, session_id: str | None = None) -> tuple[str, str]:
        sid = session_id or uuid.uuid4().hex
        token = uuid.uuid4().hex
        self._tokens[sid] = token
        return sid, token

    def validate(self, session_id: str | None, token: str | None) -> bool:
        if not session_id or not token:
            return False
        return self._tokens.get(session_id) == token
