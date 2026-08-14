"""Service registry: loads service_registry.json, resolves each entry's
Python package into the plugin contract this gateway expects, and provides
prefix-based lookup for incoming requests.

Plugin contract (see service_registry.json's own "_comment" for the same
list): a registered service's top-level package must expose
`dispatch_request(method, rel_url, body=None, headers=None) ->
(status, body, content_type)` - the one mandatory member. Everything else
(`handle_batch`, `csrf_check_failed`, `CSRF_TOKEN`, `pop_pending_headers`,
`read_static_file`, `seed_test_data`) is optional-with-safe-default, so a
minimal future service doesn't need to implement batch/CSRF/mailbox-header
support just to be registrable.

Fail-fast: an entry whose package doesn't import, or is missing the one
mandatory contract member, raises at registry-load time (process startup) -
not silently deferred to first-request time, where a misconfigured
service_registry.json would otherwise 404 forever with no clear diagnostic.
"""
from __future__ import annotations

import importlib
import json
from dataclasses import dataclass
from typing import Any, Callable, Dict, List, Optional


@dataclass(frozen=True)
class ServicePlugin:
    """Resolved view of one service_registry.json entry - the imported
    module plus everything the gateway needs to route to/serve it."""
    name: str
    module: Any
    url_prefix: str
    web_root: str
    metadata_rel_path: str
    is_default_static: bool

    @property
    def dispatch_request(self) -> Callable:
        return self.module.dispatch_request

    @property
    def handle_batch(self) -> Optional[Callable]:
        return getattr(self.module, "handle_batch", None)

    @property
    def csrf_check_failed(self) -> Optional[Callable]:
        return getattr(self.module, "csrf_check_failed", None)

    @property
    def csrf_token(self) -> Optional[str]:
        return getattr(self.module, "CSRF_TOKEN", None)

    @property
    def read_static_file(self) -> Optional[Callable]:
        return getattr(self.module, "read_static_file", None)

    @property
    def seed_test_data(self) -> Optional[Callable]:
        return getattr(self.module, "seed_test_data", None)

    def pop_pending_headers(self) -> Dict[str, str]:
        """Optional per-plugin "mailbox" header hand-off (sap-message/ETag
        for ZCHECK_SRV today - see backend.state.pop_pending_headers). A
        plugin that doesn't implement this simply has no transient headers
        to surface, not an error."""
        fn = getattr(self.module, "pop_pending_headers", None)
        return fn() if fn else {}


class ServiceRegistry:
    """Loads and resolves service_registry.json once at process startup;
    provides longest-prefix-match lookup for incoming request paths."""

    def __init__(self, registry_path: str):
        self._plugins: List[ServicePlugin] = []  # sorted longest-prefix-first
        self._default_static: Optional[ServicePlugin] = None
        self._load(registry_path)

    def _load(self, registry_path: str) -> None:
        with open(registry_path, "r", encoding="utf-8") as f:
            data = json.load(f)
        services = data.get("services", {})
        if not services:
            raise ValueError(f"service_registry.json at {registry_path!r} declares no services")

        plugins = [self._resolve_entry(name, entry) for name, entry in services.items()]
        # Longest prefix first, so resolve() can't accidentally match a
        # shorter, unrelated prefix ahead of a more specific one.
        plugins.sort(key=lambda p: len(p.url_prefix), reverse=True)
        self._plugins = plugins

        default_statics = [p for p in plugins if p.is_default_static]
        if len(default_statics) > 1:
            names = ", ".join(repr(p.name) for p in default_statics)
            raise ValueError(f"Multiple services marked isDefaultStatic=true ({names}) - exactly one is required.")
        self._default_static = default_statics[0] if default_statics else None

    @staticmethod
    def _resolve_entry(name: str, entry: dict) -> ServicePlugin:
        package_name = entry.get("package")
        if not package_name:
            raise ValueError(f"Service {name!r} in service_registry.json has no 'package'")
        try:
            module = importlib.import_module(package_name)
        except ImportError as exc:
            raise ImportError(
                f"Service {name!r} declares package {package_name!r}, which failed to import: {exc}"
            ) from exc
        if not hasattr(module, "dispatch_request"):
            raise AttributeError(
                f"Service {name!r} (package {package_name!r}) has no dispatch_request - every "
                f"registered service must expose dispatch_request(method, rel_url, body=None, headers=None)."
            )
        url_prefix = entry.get("urlPrefix")
        if not url_prefix:
            raise ValueError(f"Service {name!r} in service_registry.json has no 'urlPrefix'")
        return ServicePlugin(
            name=name,
            module=module,
            url_prefix=url_prefix.rstrip("/"),
            web_root=entry.get("webRoot", "."),
            metadata_rel_path=entry.get("metadataRelPath", "localService/metadata.xml"),
            is_default_static=bool(entry.get("isDefaultStatic", False)),
        )

    def resolve(self, path: str) -> Optional[ServicePlugin]:
        """Given a request path, return the ServicePlugin whose urlPrefix it
        starts with (longest match wins), or None if no registered service
        matches - the gateway should 404 in that case, matching real
        Gateway behavior for an unregistered service."""
        for plugin in self._plugins:
            if path == plugin.url_prefix or path.startswith(plugin.url_prefix + "/"):
                return plugin
        return None

    def default_static(self) -> Optional[ServicePlugin]:
        return self._default_static

    def all_plugins(self) -> List[ServicePlugin]:
        return list(self._plugins)
