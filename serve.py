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
"""
from backend import *  # noqa: F401,F403
from backend.http_server import main

if __name__ == "__main__":
    main()
