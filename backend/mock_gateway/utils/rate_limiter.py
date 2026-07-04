"""
Rate limiting: per-IP request throttling to prevent DDoS attacks.
Complements CSRF protection (which covers token validity, not volume).
"""
from collections import defaultdict, deque
from datetime import datetime, timedelta
from threading import Lock
from typing import Optional, Tuple
import hashlib


class RateLimiter:
    """Thread-safe per-IP rate limiter with sliding window."""

    def __init__(self, requests_per_window: int = 100, window_seconds: int = 60):
        """
        Args:
            requests_per_window: Max requests allowed per time window
            window_seconds: Time window in seconds
        """
        self.requests_per_window = requests_per_window
        self.window_seconds = window_seconds
        self.client_requests: dict = defaultdict(deque)
        self._lock = Lock()
        self.MAX_CLIENTS = 10000  # Prevent unbounded growth

    def is_allowed(self, client_ip: str) -> Tuple[bool, dict]:
        """
        Check if client is rate-limited.

        Returns:
            (allowed: bool, headers: dict with X-RateLimit-* headers)
        """
        if not client_ip or not isinstance(client_ip, str):
            return False, {"X-RateLimit-Error": "Invalid client IP"}

        with self._lock:
            now = datetime.utcnow()
            window_start = now - timedelta(seconds=self.window_seconds)

            # Get or create client bucket
            requests_deque = self.client_requests[client_ip]

            # Remove old requests outside window
            while requests_deque and requests_deque[0] < window_start:
                requests_deque.popleft()

            # Check limit
            allowed = len(requests_deque) < self.requests_per_window
            if allowed:
                requests_deque.append(now)

            # Prune old clients (keep only recent ones)
            if len(self.client_requests) > self.MAX_CLIENTS:
                self._prune_inactive_clients(window_start)

            # Build response headers
            headers = {
                "X-RateLimit-Limit": str(self.requests_per_window),
                "X-RateLimit-Remaining": str(max(0, self.requests_per_window - len(requests_deque))),
                "X-RateLimit-Reset": str(int((window_start + timedelta(seconds=self.window_seconds)).timestamp())),
            }

            if not allowed:
                headers["X-RateLimit-Exceeded"] = "true"

            return allowed, headers

    def _prune_inactive_clients(self, cutoff_time: datetime) -> None:
        """Remove clients with no activity in current window (not thread-safe, call with lock)."""
        stale_clients = [
            ip for ip, reqs in self.client_requests.items()
            if not reqs or reqs[-1] < cutoff_time
        ]
        for ip in stale_clients[:len(stale_clients) // 2]:  # Remove 50% of stale
            del self.client_requests[ip]

    def reset_client(self, client_ip: str) -> None:
        """Reset rate limit for specific client (e.g., after whitelisting)."""
        with self._lock:
            if client_ip in self.client_requests:
                del self.client_requests[client_ip]

    def reset_all(self) -> None:
        """Clear all rate limit data."""
        with self._lock:
            self.client_requests.clear()


class RateLimitExceeded(Exception):
    """Raised when client exceeds rate limit."""

    def __init__(self, client_ip: str, limit: int, window_seconds: int):
        self.client_ip = client_ip
        self.limit = limit
        self.window_seconds = window_seconds
        super().__init__(
            f"Rate limit exceeded for {client_ip}: {limit} requests per {window_seconds}s"
        )


def extract_client_ip(request) -> str:
    """
    Extract client IP from FastAPI request, accounting for proxies.

    Checks (in order):
    1. X-Forwarded-For header (behind proxy)
    2. X-Real-IP header (alternative proxy header)
    3. client.host (direct connection)
    """
    if hasattr(request, "headers"):
        # X-Forwarded-For can be comma-separated list (take first/leftmost)
        forwarded = request.headers.get("X-Forwarded-For", "").split(",")[0].strip()
        if forwarded:
            return forwarded

        real_ip = request.headers.get("X-Real-IP", "").strip()
        if real_ip:
            return real_ip

    if hasattr(request, "client") and request.client:
        return request.client.host

    return "unknown"
