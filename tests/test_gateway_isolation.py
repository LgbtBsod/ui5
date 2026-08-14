#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Proves the multi-service gateway's core claim empirically, rather than
asserting it: one process can host several independent OData services at
once, and mutating one service's data never touches another's - even a
service that isn't currently receiving any traffic keeps its data intact.

Uses the repo's real service_registry.json (ZCHECK_SRV = backend/,
ZDEMO_SRV = demo_service/ - a deliberately tiny second service that exists
only to make this test possible, see demo_service/__init__.py) rather than
a synthetic throwaway registry, so this also catches real drift in the
shipped registry, not just the isolation mechanism in the abstract.

Run: python3 -m unittest discover -s tests
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import serve_config  # noqa: E402
import serve  # noqa: E402
from gateway import ServiceRegistry  # noqa: E402

REGISTRY_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "service_registry.json")


class MultiServiceIsolationTests(unittest.TestCase):
    def setUp(self):
        self.registry = ServiceRegistry(REGISTRY_PATH)
        self.zcheck = self.registry.resolve("/sap/opu/odata/sap/ZCHECK_SRV")
        self.zdemo = self.registry.resolve("/sap/opu/odata/sap/ZDEMO_SRV")
        self.assertIsNotNone(self.zcheck)
        self.assertIsNotNone(self.zdemo)

        # A known CheckRoot to mutate, independent of seed.py's own fixtures
        # (avoids coupling this test to whatever TEST-001/INT-001 currently
        # look like - see tests/test_serve.py's own setUp patterns for the
        # same reasoning).
        self.root_id = "gateway-isolation-test-root"
        serve.store["CheckRoots"][self.root_id] = {
            "RootId": self.root_id, "ActiveUUID": self.root_id, "DraftUUID": serve.ZERO_GUID,
            "IsActiveEntity": True, "DocId": "ISOLATION-TEST", "Status": "OK",
            "LastChangedAt": serve.odata_date(), "CreatedAt": serve.odata_date(),
        }
        serve.store["CheckBasics"][self.root_id] = {
            "RootId": self.root_id, "Equipment": "before", "LastChangedAt": serve.odata_date(),
        }

    def tearDown(self):
        serve.store["CheckRoots"].pop(self.root_id, None)
        serve.store["CheckBasics"].pop(self.root_id, None)
        self.zdemo.module.state.store["Widgets"].clear()

    def test_creating_a_widget_never_touches_backend_store(self):
        status, body, _ct = self.zdemo.dispatch_request("POST", "Widgets", body={"Name": "Isolation Widget"})
        self.assertEqual(status, 201)
        widget_id = body["d"]["Id"]
        self.assertIn(widget_id, self.zdemo.module.state.store["Widgets"])
        # The whole point: ZCHECK_SRV's own store was never touched by a
        # request that never resolved to it - neither the widget's id nor
        # its data leaked in under any key.
        self.assertNotIn(widget_id, self.zcheck.module.store["CheckRoots"])
        for row in self.zcheck.module.store["CheckRoots"].values():
            self.assertNotEqual(row.get("DocId"), "Isolation Widget")

    def test_mutating_zcheck_srv_leaves_zdemo_srv_data_untouched(self):
        # 1) Create data via ZDEMO_SRV.
        status, body, _ct = self.zdemo.dispatch_request("POST", "Widgets", body={"Name": "Untouched Widget"})
        self.assertEqual(status, 201)
        widget_id = body["d"]["Id"]

        # 2) Confirm it's genuinely absent from ZCHECK_SRV's own store -
        # not just "a different key", but not present under ANY key.
        self.assertNotIn(widget_id, self.zcheck.module.store["CheckRoots"])
        self.assertNotIn(widget_id, self.zcheck.module.store["CheckBasics"])

        # 3) Mutate ZCHECK_SRV - a real PATCH through its own dispatch_request,
        # exactly like a live request would.
        draft_key = "CheckRoots(ActiveUUID=guid'%s',DraftUUID=guid'%s')" % (self.root_id, serve.ZERO_GUID)
        status, _resp, _ct = self.zcheck.dispatch_request(
            "PATCH", draft_key, body={"Equipment": "after"}, headers={"if-match": "*"}
        )
        self.assertEqual(status, 204)
        self.assertEqual(serve.store["CheckBasics"][self.root_id]["Equipment"], "after")

        # 4) The ZDEMO_SRV row from step 1 must still be there, byte-for-byte
        # unchanged - this is the actual "data of a service not currently
        # talking to us isn't lost" guarantee, proven, not asserted.
        widget_after = self.zdemo.module.state.store["Widgets"].get(widget_id)
        self.assertIsNotNone(widget_after)
        self.assertEqual(widget_after["Name"], "Untouched Widget")

    def test_unregistered_service_prefix_404s_through_the_registry(self):
        self.assertIsNone(self.registry.resolve("/sap/opu/odata/sap/ZNOPE_SRV/Anything"))

    def test_both_services_are_genuinely_separate_python_modules(self):
        # Guards the isolation mechanism itself, not just its observed
        # effect - if these ever became the same module object (e.g. a
        # packaging mistake aliasing one import path to another), the
        # isolation tests above would pass for the wrong reason.
        self.assertIsNot(self.zcheck.module, self.zdemo.module)
        self.assertIsNot(self.zcheck.module.store, self.zdemo.module.state.store)


if __name__ == "__main__":
    unittest.main()
