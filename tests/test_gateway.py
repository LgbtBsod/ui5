#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Tests for gateway/registry.py - the multi-service registry that lets one
process host several independent OData services (see service_registry.json
and gateway/__init__.py's own docstring for the design rationale).

Run: python3 -m unittest discover -s tests
"""
import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from gateway import ServiceRegistry  # noqa: E402


class ShippedRegistryTests(unittest.TestCase):
    """Sanity checks against the repo's own real service_registry.json -
    catches accidental drift/breakage of the live registry, not just the
    ServiceRegistry class's own logic."""

    REGISTRY_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "service_registry.json")

    def setUp(self):
        self.registry = ServiceRegistry(self.REGISTRY_PATH)

    def test_zcheck_srv_resolves_by_exact_prefix(self):
        plugin = self.registry.resolve("/sap/opu/odata/sap/ZCHECK_SRV")
        self.assertIsNotNone(plugin)
        self.assertEqual(plugin.name, "ZCHECK_SRV")

    def test_zcheck_srv_resolves_by_nested_path(self):
        plugin = self.registry.resolve("/sap/opu/odata/sap/ZCHECK_SRV/CheckRoots('x')")
        self.assertIsNotNone(plugin)
        self.assertEqual(plugin.name, "ZCHECK_SRV")

    def test_unregistered_service_resolves_to_none(self):
        # This is the "404 for an unknown service" behavior the whole point
        # of the registry is to enable - see gateway/http_server.py.
        self.assertIsNone(self.registry.resolve("/sap/opu/odata/sap/SOME_OTHER_SRV/Foo"))

    def test_similar_but_not_matching_prefix_does_not_false_positive(self):
        # A path that merely starts with the same characters as a real
        # prefix but isn't actually rooted at it (no trailing "/" or exact
        # match) must not resolve - guards against a naive str.startswith
        # on the raw prefix without the boundary check.
        self.assertIsNone(self.registry.resolve("/sap/opu/odata/sap/ZCHECK_SRV_EXTRA/Foo"))

    def test_zcheck_srv_is_the_default_static_host(self):
        plugin = self.registry.default_static()
        self.assertIsNotNone(plugin)
        self.assertEqual(plugin.name, "ZCHECK_SRV")

    def test_zcheck_srv_plugin_exposes_the_full_contract(self):
        plugin = self.registry.resolve("/sap/opu/odata/sap/ZCHECK_SRV")
        self.assertTrue(callable(plugin.dispatch_request))
        self.assertTrue(callable(plugin.handle_batch))
        self.assertTrue(callable(plugin.csrf_check_failed))
        self.assertIsNotNone(plugin.csrf_token)
        self.assertTrue(callable(plugin.read_static_file))
        self.assertTrue(callable(plugin.seed_test_data))

    def test_zcheck_srv_dispatch_request_is_the_real_backend_one(self):
        # Confirms the registry didn't accidentally wrap/shadow the plugin's
        # function - it must be the actual backend.dispatch_request object,
        # since 79 existing tests already exercise that function directly
        # and this must be the SAME code path a live request would hit.
        import backend
        plugin = self.registry.resolve("/sap/opu/odata/sap/ZCHECK_SRV")
        self.assertIs(plugin.dispatch_request, backend.dispatch_request)


class RegistryValidationTests(unittest.TestCase):
    """Fail-fast behavior on a malformed registry file - these write a
    throwaway JSON file per test rather than touching the real one."""

    def _write_registry(self, data):
        fd, path = tempfile.mkstemp(suffix=".json")
        with os.fdopen(fd, "w", encoding="utf-8") as f:
            json.dump(data, f)
        self.addCleanup(os.remove, path)
        return path

    def test_empty_services_raises(self):
        path = self._write_registry({"services": {}})
        with self.assertRaises(ValueError):
            ServiceRegistry(path)

    def test_missing_package_raises(self):
        path = self._write_registry({"services": {"X": {"urlPrefix": "/x"}}})
        with self.assertRaises(ValueError):
            ServiceRegistry(path)

    def test_missing_url_prefix_raises(self):
        path = self._write_registry({"services": {"X": {"package": "backend"}}})
        with self.assertRaises(ValueError):
            ServiceRegistry(path)

    def test_unimportable_package_raises(self):
        path = self._write_registry({"services": {"X": {"package": "no_such_module_xyz", "urlPrefix": "/x"}}})
        with self.assertRaises(ImportError):
            ServiceRegistry(path)

    def test_package_without_dispatch_request_raises(self):
        # "os" is a real, importable stdlib module with no dispatch_request -
        # exercises the contract check independent of import success.
        path = self._write_registry({"services": {"X": {"package": "os", "urlPrefix": "/x"}}})
        with self.assertRaises(AttributeError):
            ServiceRegistry(path)

    def test_two_default_static_entries_raises(self):
        path = self._write_registry({
            "services": {
                "A": {"package": "backend", "urlPrefix": "/a", "isDefaultStatic": True},
                "B": {"package": "backend", "urlPrefix": "/b", "isDefaultStatic": True},
            }
        })
        with self.assertRaises(ValueError):
            ServiceRegistry(path)

    def test_longest_prefix_wins_on_overlap(self):
        # A deliberately adversarial case: two prefixes where one is a
        # parent of the other - the more specific one must win regardless
        # of declaration order in the JSON (dict order would otherwise
        # silently determine behavior, which is fragile).
        path = self._write_registry({
            "services": {
                "SHORT": {"package": "backend", "urlPrefix": "/sap/opu/odata/sap"},
                "LONG": {"package": "backend", "urlPrefix": "/sap/opu/odata/sap/ZCHECK_SRV"},
            }
        })
        registry = ServiceRegistry(path)
        plugin = registry.resolve("/sap/opu/odata/sap/ZCHECK_SRV/CheckRoots")
        self.assertEqual(plugin.name, "LONG")


if __name__ == "__main__":
    unittest.main()
