from __future__ import annotations

import secrets
from collections import OrderedDict


class CsrfStore:
    MAX_TOKENS = 1000

    def __init__(self):
        self._tokens: OrderedDict[str, str] = OrderedDict()

    def issue(self, session_id: str | None = None) -> tuple[str, str]:
        sid = session_id or secrets.token_urlsafe(32)
        token = secrets.token_urlsafe(32)
        self._tokens[sid] = token
        if len(self._tokens) > self.MAX_TOKENS:
            self._tokens.popitem(last=False)
        return sid, token

    def validate(self, session_id: str | None, token: str | None) -> bool:
        if not session_id or not token:
            return False
        return self._tokens.get(session_id) == token

    def cleanup(self) -> int:
        count = len(self._tokens)
        self._tokens.clear()
        return count
