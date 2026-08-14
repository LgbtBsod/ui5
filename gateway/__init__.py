"""Multi-service Gateway: a thin hub that reads service_registry.json and
routes incoming requests to whichever registered service's own package
implements them - modeled on how real SAP Gateway/ICM sits in front of
independent backend services (each its own DPC/MPC), rather than one
generic engine parameterized by config.

See ../.claude/plans (or the repo's own docs, once written up) for the
full design rationale. backend/ (the ZCHECK_SRV service) is registered
as-is, unmodified, as the first plugin - this package adds capability,
it does not change how backend/ works internally.
"""
from .registry import ServiceRegistry, ServicePlugin  # noqa: F401

__all__ = ["ServiceRegistry", "ServicePlugin"]
