#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Exercises toolkit/crud.py directly against SYNTHETIC entity configs
(Gadgets/Parts - neither exists anywhere else in this repo) rather than
backend's or demo_service's own entities, to prove the generic CRUD engine
is genuinely reusable and not accidentally coupled to any one service's
shape. demo_service/dispatch.py's own live use of this engine is covered
separately by tests/test_gateway_isolation.py.

Run: python3 -m unittest discover -s tests
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from toolkit.crud import EntityConfig, dispatch_request  # noqa: E402


class SingleKeyCrudTests(unittest.TestCase):
    def setUp(self):
        self.store = {"Gadgets": {}}
        self._next = [1]

        def id_factory():
            gid = "G%03d" % self._next[0]
            self._next[0] += 1
            return gid

        self.configs = {
            "Gadgets": EntityConfig(
                entity_set="Gadgets", entity_type="Test.Gadget",
                key_props=("Gid",), url_prefix="/test/GADGET_SRV",
                id_factory=id_factory,
            )
        }

    def _dispatch(self, method, rel_url, body=None):
        return dispatch_request(self.configs, self.store, method, rel_url, body=body)

    def test_service_document_lists_entity_sets(self):
        status, body, _ct = self._dispatch("GET", "")
        self.assertEqual(status, 200)
        self.assertEqual(body["d"]["EntitySets"], ["Gadgets"])

    def test_create_auto_generates_key_via_id_factory(self):
        status, body, _ct = self._dispatch("POST", "Gadgets", body={"Name": "Widget"})
        self.assertEqual(status, 201)
        self.assertEqual(body["d"]["Gid"], "G001")
        self.assertEqual(body["d"]["__metadata"]["uri"], "/test/GADGET_SRV/Gadgets('G001')")
        self.assertIn("G001", self.store["Gadgets"])

    def test_list_returns_every_created_row(self):
        self._dispatch("POST", "Gadgets", body={"Name": "A"})
        self._dispatch("POST", "Gadgets", body={"Name": "B"})
        status, body, _ct = self._dispatch("GET", "Gadgets")
        self.assertEqual(status, 200)
        self.assertEqual(body["d"]["__count"], "2")
        self.assertEqual(len(body["d"]["results"]), 2)

    def test_get_single_by_key(self):
        self._dispatch("POST", "Gadgets", body={"Name": "A"})
        status, body, _ct = self._dispatch("GET", "Gadgets('G001')")
        self.assertEqual(status, 200)
        self.assertEqual(body["d"]["Name"], "A")

    def test_get_missing_key_404s(self):
        status, _body, _ct = self._dispatch("GET", "Gadgets('nope')")
        self.assertEqual(status, 404)

    def test_patch_updates_in_place_and_returns_204_no_body(self):
        self._dispatch("POST", "Gadgets", body={"Name": "A"})
        status, body, _ct = self._dispatch("PATCH", "Gadgets('G001')", body={"Name": "Renamed"})
        self.assertEqual(status, 204)
        self.assertIsNone(body)
        self.assertEqual(self.store["Gadgets"]["G001"]["Name"], "Renamed")

    def test_delete_removes_row_and_returns_204(self):
        self._dispatch("POST", "Gadgets", body={"Name": "A"})
        status, _body, _ct = self._dispatch("DELETE", "Gadgets('G001')")
        self.assertEqual(status, 204)
        self.assertNotIn("G001", self.store["Gadgets"])

    def test_delete_missing_key_404s(self):
        status, _body, _ct = self._dispatch("DELETE", "Gadgets('nope')")
        self.assertEqual(status, 404)

    def test_unmatched_route_404s(self):
        status, _body, _ct = self._dispatch("GET", "Nonexistent")
        self.assertEqual(status, 404)


class CompositeKeyCrudTests(unittest.TestCase):
    """Proves key_props supports composite (tuple) keys generically -
    unlike backend.patterns.COMPOSITE_KEY_RE, which is hardcoded to the
    literal names RootId/ItemId|BarrierId."""

    def setUp(self):
        self.store = {"Parts": {}}
        self.configs = {
            "Parts": EntityConfig(
                entity_set="Parts", entity_type="Test.Part",
                key_props=("OwnerId", "PartId"), url_prefix="/test/GADGET_SRV",
            )
        }

    def _dispatch(self, method, rel_url, body=None):
        return dispatch_request(self.configs, self.store, method, rel_url, body=body)

    def test_create_requires_every_key_part_supplied(self):
        status, _body, _ct = self._dispatch("POST", "Parts", body={"OwnerId": "o1"})
        self.assertEqual(status, 400)
        self.assertEqual(self.store["Parts"], {})

    def test_create_and_fetch_by_composite_key(self):
        status, _body, _ct = self._dispatch(
            "POST", "Parts", body={"OwnerId": "o1", "PartId": "p1", "Name": "Bolt"}
        )
        self.assertEqual(status, 201)
        self.assertIn(("o1", "p1"), self.store["Parts"])
        status, body, _ct = self._dispatch("GET", "Parts(OwnerId='o1',PartId='p1')")
        self.assertEqual(status, 200)
        self.assertEqual(body["d"]["Name"], "Bolt")

    def test_composite_key_path_for_wrong_entity_set_name_404s(self):
        status, _body, _ct = self._dispatch("GET", "Nope(OwnerId='o1',PartId='p1')")
        self.assertEqual(status, 404)


if __name__ == "__main__":
    unittest.main()
