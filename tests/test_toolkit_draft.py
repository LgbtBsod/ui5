#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Exercises toolkit/draft.py's DraftEngine directly against a SYNTHETIC
root+child+basic-proxy shape (Orders/Lines/OrderMeta - none of which exist
anywhere else in this repo) rather than CheckRoots/CheckItems/Barriers/
CheckBasics, to prove the generic draft-protocol engine genuinely works for
more than its one current real consumer (backend/draft_service.py).

Also pins the resolver boundary established during the toolkit design
review: DraftEngine must return RAW store dicts, never anything resolved/
wrapped - backend/draft_service.py is the only place resolvers.wrap_entity
gets applied, deliberately outside this engine.

Run: python3 -m unittest discover -s tests
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from toolkit.draft import ChildEntityConfig, DraftEngine, DraftRootConfig, ZERO_GUID  # noqa: E402


def _make_engine():
    store = {"Orders": {}, "OrderMeta": {}, "Lines": {}}
    draft_store = {"Orders": {}}
    config = DraftRootConfig(
        entity_set="Orders",
        root_key_prop="OrderId",
        children=(ChildEntityConfig(entity_set="Lines", id_prop="LineId", nav_property="to_Lines"),),
        basic_proxy_entity_set="OrderMeta",
        required_root_fields={"CustomerName": "Customer Name"},
        required_child_field="Qty",
        required_child_field_label="Quantity",
        default_owner="ANONYMOUS",
    )
    return DraftEngine(config, store, draft_store), store, draft_store


class DraftEngineRawReturnTests(unittest.TestCase):
    def test_prepare_returns_raw_dict_without_metadata_or_resolver_fields(self):
        engine, store, _draft_store = _make_engine()
        store["Orders"]["O1"] = {
            "OrderId": "O1", "ActiveUUID": "O1", "DraftUUID": ZERO_GUID,
            "IsActiveEntity": True, "CustomerName": "Acme",
        }
        draft_row = engine.prepare("O1")
        self.assertIsInstance(draft_row, dict)
        self.assertNotIn("__metadata", draft_row)
        self.assertEqual(draft_row["ActiveUUID"], "O1")

    def test_prepare_unknown_active_uuid_returns_none(self):
        engine, _store, _draft_store = _make_engine()
        self.assertIsNone(engine.prepare("does-not-exist"))


class DraftEngineEditCycleTests(unittest.TestCase):
    def setUp(self):
        self.engine, self.store, self.draft_store = _make_engine()
        self.store["Orders"]["O1"] = {
            "OrderId": "O1", "ActiveUUID": "O1", "DraftUUID": ZERO_GUID,
            "IsActiveEntity": True, "CustomerName": "Acme",
        }
        self.store["OrderMeta"]["O1"] = {"OrderId": "O1", "Notes": "before"}
        self.store["Lines"][("O1", "L1")] = {
            "OrderId": "O1", "LineId": "L1", "DraftUUID": ZERO_GUID, "Qty": 5,
        }

    def test_prepare_clones_active_children_tagged_with_new_draft_uuid(self):
        draft_row = self.engine.prepare("O1")
        draft_uuid = draft_row["DraftUUID"]
        clones = [k for k in self.store["Lines"] if k[0] == "O1" and k != ("O1", "L1")]
        self.assertEqual(len(clones), 1)
        self.assertEqual(self.store["Lines"][clones[0]]["DraftUUID"], draft_uuid)
        # Original active row must be completely untouched.
        self.assertEqual(self.store["Lines"][("O1", "L1")]["DraftUUID"], ZERO_GUID)

    def test_activate_rejects_when_new_child_row_missing_required_field(self):
        draft_row = self.engine.prepare("O1")
        draft_uuid = draft_row["DraftUUID"]
        # Simulate a CreateChildHandler-equivalent adding a new line during
        # the edit session, tagged with this draft's own uuid, Qty unset.
        self.store["Lines"][("O1", "L2")] = {
            "OrderId": "O1", "LineId": "L2", "DraftUUID": draft_uuid, "Qty": None,
        }
        result = self.engine.activate(draft_uuid)
        self.assertIsInstance(result, tuple)
        self.assertEqual(result[0], "validation_error")
        _tag, missing, missing_children = result
        self.assertEqual(missing, [])
        self.assertEqual(len(missing_children), 1)
        self.assertEqual(missing_children[0], {
            "nav": "to_Lines", "id_prop": "LineId", "item_id": "L2",
            "field": "Qty", "label": "Quantity",
        })
        # Draft left untouched so the user can fix the field and retry.
        self.assertIn(draft_uuid, self.draft_store["Orders"])

    def test_activate_rejects_when_required_root_field_blanked(self):
        draft_row = self.engine.prepare("O1")
        draft_uuid = draft_row["DraftUUID"]
        self.draft_store["Orders"][draft_uuid]["CustomerName"] = ""
        result = self.engine.activate(draft_uuid)
        self.assertEqual(result[0], "validation_error")
        self.assertEqual(result[1], [("CustomerName", "Customer Name")])

    def test_activate_succeeds_merges_fields_and_retags_children(self):
        draft_row = self.engine.prepare("O1")
        draft_uuid = draft_row["DraftUUID"]
        self.draft_store["Orders"][draft_uuid]["CustomerName"] = "Acme Renamed"
        result = self.engine.activate(draft_uuid)
        self.assertIsInstance(result, dict)
        self.assertEqual(result["CustomerName"], "Acme Renamed")
        self.assertNotIn(draft_uuid, self.draft_store["Orders"])
        active_lines = [
            r for (rid, _lid), r in self.store["Lines"].items()
            if rid == "O1" and r.get("DraftUUID") == ZERO_GUID
        ]
        self.assertEqual(len(active_lines), 1)
        self.assertNotIn(("O1", "L1"), self.store["Lines"])  # old active row replaced, not duplicated

    def test_discard_edit_draft_restores_basic_proxy_and_drops_tagged_children(self):
        draft_row = self.engine.prepare("O1")
        draft_uuid = draft_row["DraftUUID"]
        self.store["OrderMeta"]["O1"]["Notes"] = "changed mid-session"
        result = self.engine.discard(draft_uuid)
        self.assertIsInstance(result, dict)
        self.assertEqual(self.store["OrderMeta"]["O1"]["Notes"], "before")
        self.assertNotIn(draft_uuid, self.draft_store["Orders"])
        remaining = [k for k in self.store["Lines"] if k[0] == "O1"]
        self.assertEqual(remaining, [("O1", "L1")])

    def test_discard_create_only_draft_purges_everything_and_returns_bare_string(self):
        self.store["Orders"].pop("O1")
        self.draft_store["Orders"]["D1"] = {
            "OrderId": "O2", "ActiveUUID": ZERO_GUID, "DraftUUID": "D1",
            "IsActiveEntity": False, "InProcessByUser": "ANONYMOUS",
        }
        self.store["OrderMeta"]["O2"] = {"OrderId": "O2", "Notes": "abandoned"}
        self.store["Lines"][("O2", "L9")] = {"OrderId": "O2", "LineId": "L9", "DraftUUID": "D1", "Qty": 1}
        result = self.engine.discard("D1")
        self.assertEqual(result, "discarded-create-only")
        self.assertIsInstance(result, str)
        self.assertNotIn("O2", self.store["OrderMeta"])
        self.assertEqual([k for k in self.store["Lines"] if k[0] == "O2"], [])

    def test_foreign_user_draft_blocks_second_preparer(self):
        self.engine.prepare("O1", requesting_user="alice")
        result = self.engine.prepare("O1", requesting_user="bob")
        self.assertIsInstance(result, tuple)
        self.assertEqual(result[0], "foreign_lock")

    def test_owner_can_resume_own_draft_idempotently(self):
        first = self.engine.prepare("O1", requesting_user="alice")
        second = self.engine.prepare("O1", requesting_user="alice")
        self.assertEqual(first["DraftUUID"], second["DraftUUID"])
        self.assertEqual(len(self.draft_store["Orders"]), 1)


if __name__ == "__main__":
    unittest.main()
