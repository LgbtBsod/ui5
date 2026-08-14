#!/usr/bin/env python3
"""
Pure-Python OData v2 Mock Server + Static File Server
for SAP Fiori Elements demo.

ZERO external dependencies — only Python 3.6+ standard library.

Usage:
    python serve.py                # http://localhost:8000
    python serve.py --port 9090    # custom port

Opens browser-friendly demo at the given port.
All OData requests are handled in-process (no Node.js needed).

Thin backwards-compatible shim over the backend/ package (see
backend/__init__.py for the module map: config/state/odata_format/
resolvers/draft_service/query/crud/dispatch/batch/http_server/seed).
Kept as the process entry point (`python serve.py`) and as the flat
`import serve; serve.XXX` surface tests/test_serve.py relies on.

[Fix, gateway pass] The process entry point now boots gateway/http_server.py
instead of backend/http_server.py directly - a thin multi-service router
(see service_registry.json + gateway/__init__.py's docstring) that reads
service_registry.json and delegates each request to whichever registered
service's own package handles it. backend/ (ZCHECK_SRV) is registered there
unmodified, as the first plugin - this is a one-line swap of WHICH server
boots, not a change to what backend/ does or how it's tested. The
`from backend import *` re-export below is untouched, so `import serve;
serve.dispatch_request(...)` etc. (what tests/test_serve.py's whole suite
relies on) keeps working exactly as before, bypassing the gateway/HTTP
layer entirely, same as it always has.
"""
from backend import *  # noqa: F401,F403
from gateway.http_server import main

if __name__ == "__main__":
    main()
