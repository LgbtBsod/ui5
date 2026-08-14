"""ZDEMO_SRV - a deliberately tiny, self-contained SECOND service, registered
alongside backend/ (ZCHECK_SRV) in service_registry.json purely to prove the
multi-service gateway's core claim empirically: one process can host several
independent OData services at once, and a service that isn't currently
receiving traffic keeps its data untouched (see tests/test_gateway_isolation.py).

Deliberately does NOT reuse backend/'s patterns of a bare top-level
`import serve_config` or module-level mutable globals shared by name with
another service - see gateway/registry.py's own docstring and the plan
this was built from for why that collision risk matters for a genuinely
reusable multi-service base. Everything here is self-contained under the
`demo_service` package name.
"""
from .dispatch import dispatch_request  # noqa: F401
from .state import store  # noqa: F401

__all__ = ["dispatch_request", "store"]
