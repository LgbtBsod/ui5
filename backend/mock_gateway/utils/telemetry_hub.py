"""
Unified telemetry hub: consolidates Python mock buffer and UI5 telemetry into single stream.
Replaces MemoryTelemetryBuffer + UxTelemetry duplication.
"""
from dataclasses import dataclass, asdict
from datetime import datetime
from enum import Enum
from typing import Any, Optional, List, Dict, Callable
from collections import deque
from threading import Lock
import json
import logging

logger = logging.getLogger("telemetry")


class TelemetryLevel(Enum):
    """Telemetry event severity/importance."""
    DEBUG = "debug"
    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    CRITICAL = "critical"


class TelemetryType(Enum):
    """Categorization of telemetry events."""
    API_REQUEST = "api.request"
    API_RESPONSE = "api.response"
    DATABASE_QUERY = "db.query"
    DATABASE_MUTATION = "db.mutation"
    LOCK_OPERATION = "lock.operation"
    ATTACHMENT_UPLOAD = "attachment.upload"
    ATTACHMENT_DOWNLOAD = "attachment.download"
    AUTHENTICATION = "auth.check"
    AUTHORIZATION = "authz.check"
    VALIDATION_ERROR = "validation.error"
    BUSINESS_RULE_VIOLATION = "business.rule"
    PERFORMANCE_METRIC = "performance.metric"
    SYSTEM_EVENT = "system.event"
    UI_INTERACTION = "ui.interaction"


@dataclass
class TelemetryEvent:
    """Single telemetry event."""
    type: TelemetryType
    level: TelemetryLevel
    timestamp: datetime
    message: str
    client_ip: Optional[str] = None
    session_guid: Optional[str] = None
    user_id: Optional[str] = None
    resource_id: Optional[str] = None
    duration_ms: Optional[float] = None
    status_code: Optional[int] = None
    error_code: Optional[str] = None
    context: Optional[Dict[str, Any]] = None  # Additional key-value context

    def to_dict(self) -> dict:
        """Convert to JSON-serializable dict."""
        data = asdict(self)
        data["type"] = self.type.value
        data["level"] = self.level.value
        data["timestamp"] = self.timestamp.isoformat()
        return data

    def to_json(self) -> str:
        """Convert to JSON string."""
        return json.dumps(self.to_dict())


class TelemetryCollector:
    """Pluggable telemetry collector (strategy pattern for extensibility)."""

    def collect(self, event: TelemetryEvent) -> None:
        """Called when new event arrives. Implementers log/send to external system."""
        raise NotImplementedError


class MemoryCollector(TelemetryCollector):
    """In-memory ring buffer collector (for testing, diagnostics)."""

    def __init__(self, max_events: int = 10000):
        self.events = deque(maxlen=max_events)
        self._lock = Lock()

    def collect(self, event: TelemetryEvent) -> None:
        with self._lock:
            self.events.append(event)

    def get_events(
        self,
        limit: int = 100,
        event_type: Optional[TelemetryType] = None,
        level: Optional[TelemetryLevel] = None,
    ) -> List[TelemetryEvent]:
        """Retrieve recent events with optional filtering."""
        with self._lock:
            events = list(self.events)

        if event_type:
            events = [e for e in events if e.type == event_type]
        if level:
            events = [e for e in events if e.level == level]

        return events[-limit:]

    def clear(self) -> None:
        """Clear all events."""
        with self._lock:
            self.events.clear()


class LogCollector(TelemetryCollector):
    """Standard Python logger collector (sends to syslog, ELK, etc.)."""

    def __init__(self, logger_instance: logging.Logger = logger):
        self.logger = logger_instance

    def collect(self, event: TelemetryEvent) -> None:
        """Log event at appropriate level."""
        log_fn = {
            TelemetryLevel.DEBUG: self.logger.debug,
            TelemetryLevel.INFO: self.logger.info,
            TelemetryLevel.WARNING: self.logger.warning,
            TelemetryLevel.ERROR: self.logger.error,
            TelemetryLevel.CRITICAL: self.logger.critical,
        }.get(event.level, self.logger.info)

        log_fn(
            event.message,
            extra={
                "type": event.type.value,
                "client_ip": event.client_ip,
                "session_guid": event.session_guid,
                "resource_id": event.resource_id,
                "duration_ms": event.duration_ms,
                "error_code": event.error_code,
            },
        )


class TelemetryHub:
    """
    Unified telemetry system: centralized event collection with pluggable collectors.

    Replaces:
    - window.__pcctTelemetryBuffer (Python mock)
    - window.__pcctUxTelemetry (UI5 framework)
    """

    def __init__(self):
        """Initialize hub with default collectors."""
        self.collectors: List[TelemetryCollector] = []
        self._lock = Lock()

        # Default: log to Python logger + in-memory buffer
        self.add_collector(LogCollector(logger))
        self.memory_collector = MemoryCollector()
        self.add_collector(self.memory_collector)

    def add_collector(self, collector: TelemetryCollector) -> None:
        """Register additional telemetry collector (e.g., external service)."""
        with self._lock:
            self.collectors.append(collector)

    def event(
        self,
        event_type: TelemetryType,
        message: str,
        level: TelemetryLevel = TelemetryLevel.INFO,
        **kwargs,
    ) -> None:
        """
        Record telemetry event.

        Args:
            event_type: Category of event
            message: Human-readable description
            level: Severity level (default: INFO)
            **kwargs: Additional fields (client_ip, session_guid, duration_ms, etc.)
        """
        event = TelemetryEvent(
            type=event_type,
            level=level,
            timestamp=datetime.utcnow(),
            message=message,
            **kwargs,
        )

        with self._lock:
            for collector in self.collectors:
                try:
                    collector.collect(event)
                except Exception as e:
                    logger.error(f"Collector error: {e}")

    def debug(self, msg: str, **kwargs) -> None:
        """Shorthand for debug-level event."""
        self.event(TelemetryType.SYSTEM_EVENT, msg, level=TelemetryLevel.DEBUG, **kwargs)

    def info(self, msg: str, **kwargs) -> None:
        """Shorthand for info-level event."""
        self.event(TelemetryType.SYSTEM_EVENT, msg, level=TelemetryLevel.INFO, **kwargs)

    def warning(self, msg: str, **kwargs) -> None:
        """Shorthand for warning-level event."""
        self.event(TelemetryType.SYSTEM_EVENT, msg, level=TelemetryLevel.WARNING, **kwargs)

    def error(self, msg: str, **kwargs) -> None:
        """Shorthand for error-level event."""
        self.event(TelemetryType.SYSTEM_EVENT, msg, level=TelemetryLevel.ERROR, **kwargs)

    def critical(self, msg: str, **kwargs) -> None:
        """Shorthand for critical-level event."""
        self.event(TelemetryType.SYSTEM_EVENT, msg, level=TelemetryLevel.CRITICAL, **kwargs)

    def get_recent_events(self, limit: int = 100) -> List[Dict[str, Any]]:
        """Retrieve recent events for diagnostics endpoint."""
        if self.memory_collector:
            return [e.to_dict() for e in self.memory_collector.get_events(limit=limit)]
        return []

    def get_events_by_type(self, event_type: TelemetryType, limit: int = 100) -> List[Dict[str, Any]]:
        """Retrieve events of specific type."""
        if self.memory_collector:
            return [
                e.to_dict()
                for e in self.memory_collector.get_events(limit=limit, event_type=event_type)
            ]
        return []


# Global singleton instance
_hub: Optional[TelemetryHub] = None


def get_telemetry_hub() -> TelemetryHub:
    """Get or create global telemetry hub."""
    global _hub
    if _hub is None:
        _hub = TelemetryHub()
    return _hub
