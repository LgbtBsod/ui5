"""Logging configuration for the Redux OData mock server.

Provides centralized logging setup with sensible defaults for development
and production environments.
"""
from __future__ import annotations

import logging
import sys
from typing import Optional


def get_logger(name: str) -> logging.Logger:
    """Get a logger instance with the given name.
    
    Args:
        name: Logger name (typically __name__)
    
    Returns:
        Configured logger instance
    """
    return logging.getLogger(name)


def setup_logging(
    level: int = logging.INFO,
    log_format: Optional[str] = None,
    use_colors: bool = False
) -> None:
    """Configure root logger for the application.
    
    Args:
        level: Logging level (default: INFO)
        log_format: Custom format string (optional)
        use_colors: Enable colored output (default: False)
    """
    if log_format is None:
        if use_colors:
            # Color codes for terminal output
            log_format = (
                "\033[36m%(asctime)s\033[0m "  # Cyan timestamp
                "\033[35m%(name)s\033[0m "      # Magenta logger name
                "\033[92m%(levelname)s\033[0m: "  # Green level
                "%(message)s"
            )
        else:
            log_format = "%(asctime)s %(name)s %(levelname)s: %(message)s"
    
    formatter = logging.Formatter(log_format, datefmt="%Y-%m-%d %H:%M:%S")
    
    handler = logging.StreamHandler(sys.stdout)
    handler.setFormatter(formatter)
    
    root_logger = logging.getLogger()
    root_logger.setLevel(level)
    root_logger.addHandler(handler)


# Convenience function for quick setup in serve.py
def init_dev_logging() -> None:
    """Initialize logging for development with INFO level."""
    setup_logging(level=logging.INFO)
