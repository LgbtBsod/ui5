"""
Lock acquisition timeout/retry queue: gracefully handle lock contention via backoff.
Improves UX by allowing clients to retry rather than instant-fail on locked resources.
"""
import asyncio
from dataclasses import dataclass
from datetime import datetime, timedelta
from enum import Enum
from typing import Optional, Callable, Any
import logging

logger = logging.getLogger("lock_queue")


class LockAcquisitionStrategy(Enum):
    """Lock acquisition strategy when resource is locked."""
    INSTANT_FAIL = "instant"  # Current default: immediate error (fast, hard UX)
    RETRY_WITH_BACKOFF = "backoff"  # New: queue retry with exponential backoff
    QUEUE_FIFO = "fifo"  # Advanced: FIFO queue (fairness, slower)


@dataclass
class LockAcquisitionRequest:
    """Queued lock acquisition request."""
    resource_id: str  # Entity key being locked
    client_ip: str  # Client attempting lock
    session_guid: str  # Session identifier
    requested_at: datetime  # Timestamp of request
    max_wait_seconds: int = 30  # Max time to wait for lock
    retry_count: int = 0  # Number of retries attempted
    last_retry_at: Optional[datetime] = None  # Last retry timestamp


class LockAcquisitionQueue:
    """Thread-safe lock acquisition queue with exponential backoff retry."""

    def __init__(
        self,
        strategy: LockAcquisitionStrategy = LockAcquisitionStrategy.RETRY_WITH_BACKOFF,
        initial_backoff_ms: int = 100,
        max_backoff_ms: int = 5000,
        max_retries: int = 5,
    ):
        """
        Args:
            strategy: INSTANT_FAIL (default), RETRY_WITH_BACKOFF, or QUEUE_FIFO
            initial_backoff_ms: Initial retry delay in milliseconds
            max_backoff_ms: Maximum retry delay cap
            max_retries: Maximum number of retry attempts
        """
        self.strategy = strategy
        self.initial_backoff_ms = initial_backoff_ms
        self.max_backoff_ms = max_backoff_ms
        self.max_retries = max_retries
        self.queue: dict[str, list[LockAcquisitionRequest]] = {}  # resource_id → [requests]

    async def acquire_with_timeout(
        self,
        resource_id: str,
        client_ip: str,
        session_guid: str,
        acquire_fn: Callable[[], Any],
        max_wait_seconds: int = 30,
    ) -> tuple[bool, Optional[Any], Optional[str]]:
        """
        Attempt to acquire lock with timeout and optional retry.

        Workflow:
        1. Try immediate acquisition (zero-wait)
        2. If locked and strategy=RETRY_WITH_BACKOFF:
           - Queue request
           - Retry with exponential backoff (100ms → 5s)
           - Return success or timeout error
        3. If strategy=INSTANT_FAIL:
           - Return error immediately (current behavior)

        Args:
            resource_id: Resource being locked
            client_ip: Client IP (for logging/tracking)
            session_guid: Session ID
            acquire_fn: Callable that attempts lock acquisition (returns True on success)
            max_wait_seconds: Maximum time to wait for lock

        Returns:
            (success: bool, result: Optional, error_msg: Optional)
            - (True, result, None) if acquired
            - (False, None, error_msg) if timeout/failed
        """
        # Attempt immediate acquisition
        try:
            result = acquire_fn()
            if result:
                logger.info(f"Lock acquired immediately for {resource_id} from {client_ip}")
                return True, result, None
        except Exception as e:
            logger.error(f"Lock acquisition error for {resource_id}: {e}")
            return False, None, str(e)

        # Handle locked resource
        if self.strategy == LockAcquisitionStrategy.INSTANT_FAIL:
            msg = f"Resource {resource_id} is locked. Try again later."
            logger.warning(f"Lock contention for {resource_id} from {client_ip}: {msg}")
            return False, None, msg

        if self.strategy == LockAcquisitionStrategy.RETRY_WITH_BACKOFF:
            return await self._retry_with_backoff(
                resource_id, client_ip, session_guid, acquire_fn, max_wait_seconds
            )

        return False, None, f"Unsupported lock strategy: {self.strategy}"

    async def _retry_with_backoff(
        self,
        resource_id: str,
        client_ip: str,
        session_guid: str,
        acquire_fn: Callable[[], Any],
        max_wait_seconds: int,
    ) -> tuple[bool, Optional[Any], Optional[str]]:
        """Internal retry loop with exponential backoff."""
        request = LockAcquisitionRequest(
            resource_id=resource_id,
            client_ip=client_ip,
            session_guid=session_guid,
            requested_at=datetime.utcnow(),
            max_wait_seconds=max_wait_seconds,
        )

        # Add to queue
        if resource_id not in self.queue:
            self.queue[resource_id] = []
        self.queue[resource_id].append(request)

        deadline = datetime.utcnow() + timedelta(seconds=max_wait_seconds)

        try:
            for attempt in range(self.max_retries + 1):
                # Calculate backoff
                backoff_ms = min(
                    self.initial_backoff_ms * (2 ** attempt),
                    self.max_backoff_ms,
                )
                await asyncio.sleep(backoff_ms / 1000)

                # Retry acquisition
                try:
                    result = acquire_fn()
                    if result:
                        request.retry_count = attempt
                        logger.info(
                            f"Lock acquired after {attempt} retries for {resource_id} "
                            f"from {client_ip} (total wait: {(datetime.utcnow() - request.requested_at).total_seconds():.2f}s)"
                        )
                        return True, result, None
                except Exception as e:
                    logger.debug(f"Retry {attempt} failed for {resource_id}: {e}")

                # Check deadline
                if datetime.utcnow() >= deadline:
                    msg = (
                        f"Lock acquisition timeout after {self.max_retries} retries "
                        f"for {resource_id}. Resource is held by another session."
                    )
                    logger.warning(f"Lock timeout for {resource_id} from {client_ip}: {msg}")
                    return False, None, msg

            return False, None, f"Failed to acquire lock after {self.max_retries} retries"

        finally:
            # Remove from queue
            if resource_id in self.queue:
                self.queue[resource_id] = [r for r in self.queue[resource_id] if r != request]
                if not self.queue[resource_id]:
                    del self.queue[resource_id]

    def get_queue_status(self, resource_id: Optional[str] = None) -> dict:
        """Get current queue status for diagnostics."""
        if resource_id:
            return {
                "resource_id": resource_id,
                "waiting_count": len(self.queue.get(resource_id, [])),
                "requests": [
                    {
                        "client_ip": r.client_ip,
                        "session_guid": r.session_guid,
                        "requested_at": r.requested_at.isoformat(),
                        "retry_count": r.retry_count,
                    }
                    for r in self.queue.get(resource_id, [])
                ],
            }

        return {
            "total_resources_waiting": len(self.queue),
            "total_waiting_requests": sum(len(reqs) for reqs in self.queue.values()),
            "resources": {
                rid: {"waiting_count": len(reqs)}
                for rid, reqs in self.queue.items()
            },
        }
