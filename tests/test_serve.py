#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Минимальный unittest-набор для mock-сервера (serve.py/serve_config.py).

Покрывает только точки, изменённые дешёвыми фиксами по итогам аудита:
- resolve_check_item/resolve_barrier (DRY-рефакторинг через _resolve_type_and_result)
- _root_etag_string (дедупликация ETag-выражения)
- RESULT_CODE_SATISFACTORY (единый источник истины кода "X" на Python-стороне)
- _single_response (удаление мёртвой ветки `if False`)
- _is_allowed_cors_origin (CORS: wildcard -> allowlist localhost/preview-домен)
- strip_readonly_fields / READONLY_FIELDS (защита от mass assignment в PUT/PATCH/MERGE)

Запуск: python3 -m unittest discover -s tests
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import serve_config  # noqa: E402
import serve  # noqa: E402


class ResolveCheckItemTests(unittest.TestCase):
    def test_known_code_resolves_text_from_check_types(self):
        row = {"Code": "VISUAL_INSPECTION", "RawText": "", "Result": "X"}
        out = serve.resolve_check_item(row)
        self.assertEqual(out["Text"], "Визуальный осмотр")
        self.assertEqual(out["ResultText"], "Удовлетворительно")

    def test_unknown_code_falls_back_to_raw_text(self):
        row = {"Code": "", "RawText": "Свободный текст из интеграции", "Result": None}
        out = serve.resolve_check_item(row)
        self.assertEqual(out["Text"], "Свободный текст из интеграции")
        self.assertEqual(out["ResultText"], "")

    def test_unknown_code_and_empty_raw_text_gives_empty_string(self):
        out = serve.resolve_check_item({"Code": "", "RawText": "", "Result": None})
        self.assertEqual(out["Text"], "")

    def test_does_not_mutate_input_row(self):
        row = {"Code": "VISUAL_INSPECTION", "RawText": "", "Result": "X"}
        serve.resolve_check_item(row)
        self.assertNotIn("Text", row)

    def test_field_control_is_optional_when_satisfactory(self):
        out = serve.resolve_check_item({"Code": "VISUAL_INSPECTION", "RawText": "", "Result": "X"})
        self.assertEqual(out["CommentFieldControl"], 3)

    def test_field_control_is_mandatory_when_unsatisfactory(self):
        # [Fix, exhaustive-sweep pass] Common.FieldControl - "" is the real
        # CheckResults code for "Неудовлетворительно" (see serve_config's
        # only two reference rows), not "no value yet" (that's None/missing).
        out = serve.resolve_check_item({"Code": "VISUAL_INSPECTION", "RawText": "", "Result": ""})
        self.assertEqual(out["CommentFieldControl"], 7)

    def test_field_control_is_optional_when_not_yet_assessed(self):
        out = serve.resolve_check_item({"Code": "VISUAL_INSPECTION", "RawText": "", "Result": None})
        self.assertEqual(out["CommentFieldControl"], 3)

    def test_deletable_true_regardless_of_result(self):
        # [Fix, use-case pass] Used to be True only while Result was still
        # unset (Result is None) - a proxy for "before the OLD add-row
        # dialog's Confirm step". The NEW add-row dialog (SubTableCrud.js)
        # defaults Result to "X" at createEntry() time, so a row now NEVER
        # reaches the server with Result still None - the old proxy left
        # Delete permanently disabled for every row, old and new alike.
        # Deletability is now unconditional; the draft-node's own Save/
        # Cancel boundary plus the native button's confirmation dialog are
        # the actual safeguards, not a Result-presence check.
        self.assertTrue(serve.resolve_check_item({"Code": "X", "RawText": "", "Result": None})["Deletable"])
        self.assertTrue(serve.resolve_check_item({"Code": "X", "RawText": "", "Result": "X"})["Deletable"])
        self.assertTrue(serve.resolve_check_item({"Code": "X", "RawText": "", "Result": ""})["Deletable"])


class ResolveBarrierTests(unittest.TestCase):
    def test_known_code_resolves_text_from_barrier_types(self):
        row = {"Code": "SAFETY_FENCE", "Result": "X"}
        out = serve.resolve_barrier(row)
        self.assertEqual(out["Text"], "Защитное ограждение")
        self.assertEqual(out["ResultText"], "Удовлетворительно")

    def test_unknown_code_gives_empty_text_without_raw_text_fallback(self):
        # В отличие от resolve_check_item, у Barrier нет RawText-фолбэка —
        # это осознанная разница поведения, зафиксированная тестом, чтобы
        # общий helper _resolve_type_and_result её не потерял при рефакторинге.
        out = serve.resolve_barrier({"Code": "UNKNOWN_CODE", "RawText": "must not leak", "Result": None})
        self.assertEqual(out["Text"], "")

    def test_field_control_and_deletable_mirror_check_item(self):
        # Same shared _resolve_type_and_result computation as CheckItem -
        # one representative test here is enough (the exhaustive cases live
        # on ResolveCheckItemTests) to guard against a Barrier-specific
        # regression in the shared helper.
        satisfactory = serve.resolve_barrier({"Code": "SAFETY_FENCE", "Result": "X"})
        failing = serve.resolve_barrier({"Code": "SAFETY_FENCE", "Result": ""})
        unassessed = serve.resolve_barrier({"Code": "SAFETY_FENCE", "Result": None})
        self.assertEqual(satisfactory["CommentFieldControl"], 3)
        self.assertEqual(failing["CommentFieldControl"], 7)
        self.assertEqual(unassessed["CommentFieldControl"], 3)
        self.assertTrue(unassessed["Deletable"])
        self.assertTrue(satisfactory["Deletable"])
        self.assertTrue(failing["Deletable"])


class RootEtagStringTests(unittest.TestCase):
    def setUp(self):
        serve.store["CheckRoots"].clear()
        serve.store["CheckBasics"].clear()
        serve.store["CheckItems"].clear()
        serve.store["Barriers"].clear()

    def tearDown(self):
        serve.store["CheckRoots"].clear()
        serve.store["CheckBasics"].clear()

    def test_root_etag_string_matches_manual_computation(self):
        root_id = "etag-test-root"
        serve.store["CheckRoots"][root_id] = {"RootId": root_id, "LastChangedAt": serve.odata_date()}

        expected_ms = serve.compute_etag_timestamp_ms(root_id)
        from datetime import datetime, timezone
        expected = serve.odata_date(datetime.fromtimestamp(expected_ms / 1000, tz=timezone.utc))

        self.assertEqual(serve._root_etag_string(root_id), expected)

    def test_root_etag_string_reflects_child_change_not_only_root(self):
        # ETag представления CheckRoots(...) обязан меняться, если изменилась
        # дочерняя строка, даже когда сам Root не PATCH-ился — это причина,
        # по которой _root_etag_string() не может просто читать Root.LastChangedAt.
        root_id = "etag-child-root"
        serve.store["CheckRoots"][root_id] = {"RootId": root_id, "LastChangedAt": serve.odata_date()}
        etag_before = serve._root_etag_string(root_id)

        import time
        time.sleep(0.01)
        serve.store["CheckItems"][(root_id, "item-1")] = {
            "RootId": root_id, "ItemId": "item-1", "LastChangedAt": serve.odata_date(),
        }
        etag_after = serve._root_etag_string(root_id)

        self.assertNotEqual(etag_before, etag_after)


class ResultCodeConstantTests(unittest.TestCase):
    def test_satisfactory_constant_is_x(self):
        self.assertEqual(serve_config.RESULT_CODE_SATISFACTORY, "X")

    def test_reference_check_results_uses_the_constant_value(self):
        satisfactory_rows = [
            row for row in serve_config.REFERENCE_DATA["CheckResults"]
            if row["ResultCode"] == serve_config.RESULT_CODE_SATISFACTORY
        ]
        self.assertEqual(len(satisfactory_rows), 1)
        self.assertEqual(satisfactory_rows[0]["ResultText"], "Удовлетворительно")

    def test_seeded_check_items_use_the_constant(self):
        serve.store["CheckRoots"].clear()
        serve.store["CheckBasics"].clear()
        serve.store["CheckItems"].clear()
        serve.store["Barriers"].clear()
        serve.seed_test_data()
        seeded_root_ids = set(serve.store["CheckRoots"].keys())
        try:
            seeded_results = {
                row["Result"] for (rid, _iid), row in serve.store["CheckItems"].items() if rid in seeded_root_ids
            }
            self.assertEqual(seeded_results, {serve_config.RESULT_CODE_SATISFACTORY})
        finally:
            serve.store["CheckRoots"].clear()
            serve.store["CheckBasics"].clear()
            serve.store["CheckItems"].clear()
            serve.store["Barriers"].clear()


class SingleResponseDeadCodeRemovalTests(unittest.TestCase):
    """_single_response раньше начинался с мёртвой (никогда не исполняемой)
    строки `if False`. Тест фиксирует поведение ДО и ПОСЛЕ её удаления —
    для REFERENCE_DATA-сущности, для транзакционной сущности и для
    неизвестного EntitySet."""

    def test_reference_entity_returns_200_with_wrapped_entity(self):
        status, body, _ctype = serve._single_response("CheckTypes", {"CheckTypeCode": "VISUAL_INSPECTION"}, {})
        self.assertEqual(status, 200)
        self.assertEqual(body["d"]["CheckTypeCode"], "VISUAL_INSPECTION")

    def test_reference_entity_unknown_key_returns_404(self):
        status, _body, _ctype = serve._single_response("CheckTypes", {"CheckTypeCode": "NO_SUCH_CODE"}, {})
        self.assertEqual(status, 404)

    def test_unknown_entity_set_returns_none(self):
        self.assertIsNone(serve._single_response("NoSuchEntitySet", {}, {}))

    def test_transactional_entity_found_returns_200(self):
        serve.store["CheckRoots"].clear()
        root_id = "550e8400-e29b-41d4-a716-446655440321"
        serve.store["CheckRoots"][root_id] = {"RootId": root_id, "LastChangedAt": serve.odata_date(), "DocId": "DOC-1"}
        try:
            status, body, _ctype = serve._single_response(
                "CheckRoots", {"ActiveUUID": root_id, "DraftUUID": serve.ZERO_GUID}, {},
            )
            self.assertEqual(status, 200)
            self.assertEqual(body["d"]["RootId"], root_id)
        finally:
            serve.store["CheckRoots"].clear()


class CorsOriginAllowlistTests(unittest.TestCase):
    def test_localhost_any_port_allowed(self):
        self.assertTrue(serve._is_allowed_cors_origin("http://localhost:3000"))
        self.assertTrue(serve._is_allowed_cors_origin("http://127.0.0.1:8000"))
        self.assertTrue(serve._is_allowed_cors_origin("https://localhost"))

    def test_preview_sandbox_domain_allowed(self):
        self.assertTrue(serve._is_allowed_cors_origin("https://preview-abc123.space-z.ai"))

    def test_arbitrary_third_party_origin_rejected(self):
        self.assertFalse(serve._is_allowed_cors_origin("https://evil.example.com"))
        self.assertFalse(serve._is_allowed_cors_origin("https://space-z.ai.evil.com"))
        self.assertFalse(serve._is_allowed_cors_origin(""))


class ReadonlyFieldsStrippingTests(unittest.TestCase):
    def test_strips_readonly_check_root_fields(self):
        body = {"Equipment": "Новое оборудование", "Status": "HACKED", "ChecksAmount": 999, "DocId": "HACKED"}
        stripped = serve_config.strip_readonly_fields("CheckRoots", body)
        self.assertEqual(stripped, {"Equipment": "Новое оборудование"})

    def test_strips_readonly_check_item_fields(self):
        body = {"Comment": "ok", "ItemId": "HACKED", "RootId": "HACKED", "LastChangedAt": "HACKED"}
        stripped = serve_config.strip_readonly_fields("CheckItems", body)
        self.assertEqual(stripped, {"Comment": "ok"})

    def test_unknown_set_name_passes_through_unfiltered(self):
        body = {"Anything": "value"}
        self.assertEqual(serve_config.strip_readonly_fields("NoSuchSet", body), body)

    def test_patch_check_roots_cannot_overwrite_protected_fields_end_to_end(self):
        serve.store["CheckRoots"].clear()
        serve.store["CheckBasics"].clear()
        root_id = "readonly-guard-root"
        serve.store["CheckRoots"][root_id] = {
            "RootId": root_id, "DocId": "DOC-ORIG", "Status": "OPEN",
            "ChecksAmount": 5, "LastChangedAt": serve.odata_date(),
        }
        try:
            # ProfText — обычное (не read-only, не Basic-proxy) поле CheckRoot,
            # используется как контроль: фильтр не должен блокировать легитимную
            # запись наряду с блокировкой защищённых полей.
            status, _body, _ctype = serve.dispatch_request(
                "PATCH", "CheckRoots('%s')" % root_id,
                body={"DocId": "HACKED", "Status": "HACKED", "ChecksAmount": 12345, "ProfText": "Электромонтёр"},
            )
            self.assertEqual(status, 204)
            stored = serve.store["CheckRoots"][root_id]
            self.assertEqual(stored["DocId"], "DOC-ORIG")
            self.assertEqual(stored["Status"], "OPEN")
            self.assertEqual(stored["ChecksAmount"], 5)
            self.assertEqual(stored["ProfText"], "Электромонтёр")
        finally:
            serve.store["CheckRoots"].clear()
            serve.store["CheckBasics"].clear()

    def test_strips_readonly_check_basics_fields_in_isolation(self):
        # LpcText/TimezoneText/... — read-only резолвленный текст на CheckBasics
        # (metadata.xml: creatable=false/updatable=false) — они НЕ входят в
        # BASIC_PROXY_FIELDS (serve.py), поэтому сегодня и не доходят до этого
        # фильтра через PATCH CheckRoots('...') — тест фиксирует поведение
        # strip_readonly_fields() саму по себе, на случай если BASIC_PROXY_FIELDS
        # когда-нибудь расширят одним из этих имён.
        body = {"LpcKey": "2", "LpcText": "HACKED", "TimezoneText": "HACKED"}
        stripped = serve_config.strip_readonly_fields("CheckBasics", body)
        self.assertEqual(stripped, {"LpcKey": "2"})

    def test_patch_check_roots_basic_proxy_field_still_writes_through_to_check_basics(self):
        # Регресс-проверка: strip_readonly_fields не должен по ошибке
        # заблокировать легитимную запись LpcKey (он в BASIC_PROXY_FIELDS
        # и не read-only) через Basic-proxy маршрутизацию.
        serve.store["CheckRoots"].clear()
        serve.store["CheckBasics"].clear()
        root_id = "readonly-guard-basic-root"
        serve.store["CheckRoots"][root_id] = {"RootId": root_id, "LastChangedAt": serve.odata_date()}
        serve.store["CheckBasics"][root_id] = {"RootId": root_id, "LpcKey": "1", "LastChangedAt": serve.odata_date()}
        try:
            status, _body, _ctype = serve.dispatch_request(
                "PATCH", "CheckRoots('%s')" % root_id, body={"LpcKey": "2"},
            )
            self.assertEqual(status, 204)
            self.assertEqual(serve.store["CheckBasics"][root_id]["LpcKey"], "2")
        finally:
            serve.store["CheckRoots"].clear()
            serve.store["CheckBasics"].clear()


class DraftOnCreateTests(unittest.TestCase):
    """[Фаза 4] POST /CheckRoots теперь создаёт ЧЕРНОВИК (draft_store), а не
    сразу активную запись — как настоящий BOPF-draft на create."""

    CREATE_BODY = {
        "ObserverFullname": "Inspector X", "ObservedFullname": "Observed Y",
        "Date": "2026-07-27T00:00:00.000Z", "Time": "PT10H0M0S", "Timezone": "Asia/Yerevan",
        "LpcKey": "2", "ProfKey": "ELECTRICIAN",
    }

    def tearDown(self):
        serve.store["CheckRoots"].clear()
        serve.store["CheckBasics"].clear()
        serve.draft_store["CheckRoots"].clear()

    def _create(self):
        status, resp, _ctype = serve.dispatch_request("POST", "CheckRoots", body=dict(self.CREATE_BODY))
        return status, resp["d"]

    def test_create_returns_inactive_draft_not_yet_in_active_store(self):
        status, root = self._create()
        self.assertEqual(status, 201)
        self.assertFalse(root["IsActiveEntity"])
        self.assertFalse(root["HasActiveEntity"])
        self.assertIn(root["RootId"], serve.draft_store["CheckRoots"])
        self.assertNotIn(root["RootId"], serve.store["CheckRoots"])

    def test_fresh_draft_appears_in_active_listing_as_orphan_draft(self):
        """[Fix] A create-draft with no active twin used to be invisible from
        the List Report entirely - there was no way to find/resume/search it
        once you navigated away before Save. Standard Fiori draft apps DO
        surface your own unsaved objects in the list (see
        serve.py _resolve_source's CheckRoots branch), so it must show up
        here too, marked IsActiveEntity=False."""
        _status, root = self._create()
        _s, resp, _c = serve.dispatch_request("GET", "CheckRoots")
        rows_by_id = {r["RootId"]: r for r in resp["d"]["results"]}
        self.assertIn(root["RootId"], rows_by_id)
        self.assertFalse(rows_by_id[root["RootId"]]["IsActiveEntity"])

    # [NW 7.50/pre-S4 1610 конвенция] create-драфт ещё не имеет активного
    # двойника — ActiveUUID должен быть ZERO_GUID, а DraftUUID — собственным
    # (равным RootId для create-only, см. _draft_activate).
    def test_create_draft_has_zero_guid_active_uuid(self):
        _status, root = self._create()
        self.assertEqual(root["ActiveUUID"], serve.ZERO_GUID)
        self.assertEqual(root["DraftUUID"], root["RootId"])

    # [NW 7.50/pre-S4 1610 конвенция] активная запись дублирует значение
    # RootId в ActiveUUID (тот же GUID в двух полях), а DraftUUID — всегда
    # ZERO_GUID, поскольку у активной таблицы своей draft_uuid-колонки нет.
    def test_active_row_duplicates_rootid_into_active_uuid_and_zeroes_draft_uuid(self):
        _status, root = self._create()
        root_id = root["RootId"]
        _s, resp, _c = serve.dispatch_request(
            "POST", "CheckRootActivationAction?DraftUUID=guid'" + root_id + "'",
        )
        self.assertEqual(resp["d"]["ActiveUUID"], root_id)
        self.assertEqual(resp["d"]["DraftUUID"], serve.ZERO_GUID)

    def test_activation_of_fresh_create_draft_promotes_it_to_active(self):
        _status, root = self._create()
        root_id = root["RootId"]  # DraftUUID == RootId for a create-only draft

        status, resp, _c = serve.dispatch_request(
            "POST", "CheckRootActivationAction?DraftUUID=guid'" + root_id + "'",
        )
        self.assertEqual(status, 200)
        self.assertTrue(resp["d"]["IsActiveEntity"])
        self.assertIn(root_id, serve.store["CheckRoots"])
        self.assertNotIn(root_id, serve.draft_store["CheckRoots"])

        _s, listing, _c = serve.dispatch_request("GET", "CheckRoots")
        ids = [r["RootId"] for r in listing["d"]["results"]]
        self.assertIn(root_id, ids)

    def test_discard_of_fresh_create_draft_leaves_no_active_record(self):
        _status, root = self._create()
        root_id = root["RootId"]

        status, resp, _c = serve.dispatch_request(
            "POST", "CheckRootDiscardAction?DraftUUID=guid'" + root_id + "'",
        )
        self.assertEqual(status, 204)
        self.assertIsNone(resp)
        self.assertNotIn(root_id, serve.draft_store["CheckRoots"])
        self.assertNotIn(root_id, serve.store["CheckRoots"])

    def test_activation_action_on_create_draft_does_not_404(self):
        # Раньше гейт FunctionImport-роутинга проверял только
        # store["CheckRoots"] — черновик из CREATE (ещё не активен) 404-ился
        # бы сразу при попытке его активировать.
        _status, root = self._create()
        status, _resp, _c = serve.dispatch_request(
            "POST", "CheckRootActivationAction?DraftUUID=guid'" + root["RootId"] + "'",
        )
        self.assertNotEqual(status, 404)

    def test_existing_edit_flow_prepare_activate_still_works(self):
        # Zero-regression: draft-on-EDIT (Prepare существующей активной
        # записи) не должен сломаться изменениями в create-пути.
        _status, root = self._create()
        root_id = root["RootId"]
        serve.dispatch_request("POST", "CheckRootActivationAction?DraftUUID=guid'" + root_id + "'")
        self.assertIn(root_id, serve.store["CheckRoots"])

        status, resp, _c = serve.dispatch_request(
            "POST", "CheckRootPreparationAction?ActiveUUID=guid'" + root_id + "'",
        )
        self.assertEqual(status, 200)
        self.assertFalse(resp["d"]["IsActiveEntity"])
        edit_draft_uuid = resp["d"]["DraftUUID"]
        self.assertNotEqual(edit_draft_uuid, root_id)  # edit-draft gets its own distinct DraftUUID
        self.assertIn(edit_draft_uuid, serve.draft_store["CheckRoots"])

        status2, resp2, _c = serve.dispatch_request(
            "POST", "CheckRootActivationAction?DraftUUID=guid'" + edit_draft_uuid + "'",
        )
        self.assertEqual(status2, 200)
        self.assertTrue(resp2["d"]["IsActiveEntity"])
        self.assertNotIn(edit_draft_uuid, serve.draft_store["CheckRoots"])

    # [Fix] Object Page запрашивает to_Checks/to_Barriers по draft-адресу
    # (ActiveUUID=zero,DraftUUID=X) для только что созданного черновика —
    # NAV_COLLECTION_RE/NAV_CREATE_RE должны это распознавать.
    def test_get_nav_collection_on_draft_addressed_fresh_create(self):
        _status, root = self._create()
        root_id = root["RootId"]
        status, resp, _c = serve.dispatch_request(
            "GET", "CheckRoots(ActiveUUID=guid'%s',DraftUUID=guid'%s')/to_Checks" % (serve.ZERO_GUID, root_id),
        )
        self.assertEqual(status, 200)
        self.assertEqual(resp["d"]["results"], [])

    def test_post_nav_create_on_draft_addressed_fresh_create(self):
        _status, root = self._create()
        root_id = root["RootId"]
        status, resp, _c = serve.dispatch_request(
            "POST", "CheckRoots(ActiveUUID=guid'%s',DraftUUID=guid'%s')/to_Checks" % (serve.ZERO_GUID, root_id),
            body={"Code": "VISUAL_INSPECTION", "Comment": "", "Result": ""},
        )
        self.assertEqual(status, 201)
        self.assertEqual(resp["d"]["RootId"], root_id)


class ForeignUserLockTests(unittest.TestCase):
    """[Fix] Multi-user draft-lock emulation (X-Mock-User header, see
    serve.py _resolve_mock_user/_draft_prepare). Previously this mock had
    exactly one identity, so sap.ui.generic.app's whole "someone else has
    this draft open" branch (checkForForeignUserLock, DraftAdministrativeData
    /InProcessByUser, the ST_GENERIC_DRAFT_LOCKED_BY_USER message) was
    permanently dormant/untestable - there was never a second user to be
    locked by."""

    def setUp(self):
        self.root_id = "lock-test-root"
        serve.store["CheckRoots"][self.root_id] = {
            "RootId": self.root_id, "ActiveUUID": self.root_id, "DraftUUID": serve.ZERO_GUID,
            "IsActiveEntity": True, "DocId": "LOCK-TEST", "Status": "OK",
            "LastChangedAt": serve.odata_date(), "CreatedAt": serve.odata_date(),
        }
        serve.store["CheckBasics"][self.root_id] = {"RootId": self.root_id, "LastChangedAt": serve.odata_date()}

    def tearDown(self):
        serve.store["CheckRoots"].pop(self.root_id, None)
        serve.store["CheckBasics"].pop(self.root_id, None)
        serve.draft_store["CheckRoots"].clear()

    def _prepare(self, uname, preserve_changes=True):
        pc = "true" if preserve_changes else "false"
        return serve.dispatch_request(
            "POST", "CheckRootPreparationAction?ActiveUUID=guid'%s'&PreserveChanges=%s" % (self.root_id, pc),
            headers={"x-mock-user": uname} if uname else None,
        )

    def test_default_identity_matches_unheadered_requests(self):
        # No X-Mock-User header at all (every existing test in this file,
        # and every real UI5 request) must behave exactly as before.
        status, resp, _c = self._prepare(None)
        self.assertEqual(status, 200)
        self.assertEqual(resp["d"]["DraftUUID"] and True, True)

    def test_same_user_can_reprepare_own_draft_idempotently(self):
        status1, resp1, _c = self._prepare("PETROV")
        self.assertEqual(status1, 200)
        draft_uuid = resp1["d"]["DraftUUID"]
        status2, resp2, _c = self._prepare("PETROV")
        self.assertEqual(status2, 200)
        self.assertEqual(resp2["d"]["DraftUUID"], draft_uuid)  # same draft, not recreated

    def test_different_user_with_preserve_changes_gets_409_foreign_lock(self):
        status1, _resp1, _c = self._prepare("PETROV")
        self.assertEqual(status1, 200)
        status2, resp2, _c = self._prepare("SIDOROVA", preserve_changes=True)
        self.assertEqual(status2, 409)
        self.assertEqual(resp2["error"]["code"], "DRAFT_LOCKED")
        # The draft itself must be completely untouched by the failed attempt.
        draft_uuid, draft_row = serve._find_draft_by_active_uuid(self.root_id)
        self.assertEqual(draft_row["InProcessByUser"], "PETROV")

    def test_different_user_with_preserve_changes_false_takes_over(self):
        status1, resp1, _c = self._prepare("PETROV")
        old_draft_uuid = resp1["d"]["DraftUUID"]
        status2, resp2, _c = self._prepare("SIDOROVA", preserve_changes=False)
        self.assertEqual(status2, 200)
        new_draft_uuid = resp2["d"]["DraftUUID"]
        self.assertNotEqual(new_draft_uuid, old_draft_uuid)
        self.assertNotIn(old_draft_uuid, serve.draft_store["CheckRoots"])
        _draft_uuid, draft_row = serve._find_draft_by_active_uuid(self.root_id)
        self.assertEqual(draft_row["InProcessByUser"], "SIDOROVA")

    def test_draft_administrative_data_resolves_from_active_row_via_expand(self):
        # [Fix] This is the actual gap that made checkForForeignUserLock's
        # $expand=DraftAdministrativeData read (issued against the ACTIVE
        # row's own binding context) always come back empty - an active
        # row's own DraftUUID field is always ZERO_GUID, so apply_expand must
        # fall back to looking up the real draft by RootId instead.
        self._prepare("PETROV")
        status, resp, _c = serve.dispatch_request(
            "GET", "CheckRoots(ActiveUUID=guid'%s',DraftUUID=guid'%s')?$expand=DraftAdministrativeData"
            % (self.root_id, serve.ZERO_GUID),
        )
        self.assertEqual(status, 200)
        admin = resp["d"]["DraftAdministrativeData"]
        self.assertIsNotNone(admin)
        self.assertEqual(admin["InProcessByUser"], "PETROV")
        self.assertEqual(admin["InProcessByUserDescription"], "Инженер по ОТ Иван Петров")

    def test_dev_locks_endpoint_lists_owner(self):
        self._prepare("PETROV")
        locks = [
            {
                "RootId": draft_row["RootId"],
                "InProcessByUser": draft_row.get("InProcessByUser", serve.MOCK_USER),
            }
            for draft_row in serve.draft_store["CheckRoots"].values()
        ]
        matching = [l for l in locks if l["RootId"] == self.root_id]
        self.assertEqual(len(matching), 1)
        self.assertEqual(matching[0]["InProcessByUser"], "PETROV")


class CreateDraftPreparationReinvocationTests(unittest.TestCase):
    """[Fix, exhaustive-sweep pass] Common.SideEffects (EffectTypes="ValueChange",
    our existing ChecksAndBarriersRecalc qualifier - confirmed live elsewhere
    to trigger a PreparationAction call) re-invokes PreparationAction any
    time a to_Checks/to_Barriers row changes, not just once at the initial
    Edit click. For a CREATE-draft (never activated), the framework supplies
    the draft's OWN ActiveUUID field value - genuinely config.ZERO_GUID,
    live-verified via the real browser flow (Создать -> add/observe the
    binding context) - alongside its real DraftUUID. Before this fix,
    _draft_prepare's first line rejected ZERO_GUID outright since it's never
    a key in store["CheckRoots"], 404ing every such re-invocation."""

    def setUp(self):
        status, resp, _c = serve.dispatch_request("POST", "CheckRoots", body={})
        self.assertEqual(status, 201)
        self.draft_uuid = resp["d"]["DraftUUID"]
        self.root_id = resp["d"]["RootId"]

    def tearDown(self):
        serve.draft_store["CheckRoots"].pop(self.draft_uuid, None)
        serve.store["CheckBasics"].pop(self.root_id, None)

    def test_reinvoking_preparation_on_a_create_draft_resolves_instead_of_404(self):
        status, resp, _c = serve.dispatch_request(
            "POST",
            "CheckRootPreparationAction?ActiveUUID=guid'%s'&DraftUUID=guid'%s'&SideEffectsQualifier=ChecksAndBarriersRecalc"
            % (serve.ZERO_GUID, self.draft_uuid),
        )
        self.assertEqual(status, 200)
        self.assertEqual(resp["d"]["DraftUUID"], self.draft_uuid)
        self.assertEqual(resp["d"]["RootId"], self.root_id)
        # Must not mutate/recreate the draft - still the same object.
        self.assertIn(self.draft_uuid, serve.draft_store["CheckRoots"])

    def test_unknown_draft_uuid_with_zero_active_uuid_still_404s(self):
        status, _resp, _c = serve.dispatch_request(
            "POST", "CheckRootPreparationAction?ActiveUUID=guid'%s'&DraftUUID=guid'%s'" % (serve.ZERO_GUID, serve.ZERO_GUID),
        )
        self.assertEqual(status, 404)

    def test_reinvocation_by_a_foreign_user_is_rejected(self):
        status, resp, _c = serve.dispatch_request(
            "POST",
            "CheckRootPreparationAction?ActiveUUID=guid'%s'&DraftUUID=guid'%s'" % (serve.ZERO_GUID, self.draft_uuid),
            headers={"x-mock-user": "SIDOROVA"},
        )
        self.assertEqual(status, 409)
        self.assertEqual(resp["error"]["code"], "DRAFT_LOCKED")


class HandlersRefactorRegressionTests(unittest.TestCase):
    """[Fix] Two regressions caught by a research pass auditing the
    backend/handlers.py Command-pattern refactor (introduced by other work
    during a real multi-day gap between sessions) against the original
    dispatch.py it replaced - neither was caught by the 64 tests passing at
    the time, since no existing test exercised either exact scenario."""

    def test_activation_of_unknown_draft_uuid_returns_404_not_200(self):
        # [Fix] _handle_activate previously fell through straight to
        # ResponseBuilder.ok(result_row) without checking result_row is None
        # first (unlike its sibling _process_draft_result, used by Prepare/
        # Discard, which already had this check) - a nonexistent DraftUUID
        # silently returned 200 with a null body and even fired the
        # "Черновик сохранён" success sap-message, instead of 404.
        status, resp, _c = serve.dispatch_request(
            "POST", "CheckRootActivationAction?DraftUUID=guid'11111111-1111-1111-1111-111111111111'",
        )
        self.assertEqual(status, 404)
        self.assertIsNotNone(resp)

    def test_unmatched_route_returns_404_not_400(self):
        # [Fix] The dispatcher's no-handler-matched fallback silently
        # switched from a plain 404 (the pre-refactor original, matching
        # SAP Gateway convention for a nonexistent resource path) to a 400
        # with an added error.code/lang shape - unguarded by any test in
        # either direction. Pinned back to 404 for behavioral parity.
        status, resp, _c = serve.dispatch_request("GET", "ThisRouteDoesNotExist")
        self.assertEqual(status, 404)
        self.assertIn("value", resp["error"]["message"])

    def test_owner_can_delete_their_own_draft_via_x_mock_user_header(self):
        # [Fix] DeleteHandler._delete_root hardcoded
        # state._resolve_mock_user({}) (an empty dict) instead of threading
        # the real request headers through - so the foreign-lock re-check
        # always compared against the DEFAULT MOCK_USER identity, not
        # whoever the X-Mock-User header actually said. A user deleting
        # their OWN non-default-identity draft got a false 409 DRAFT_LOCKED
        # instead of 204, because the code compared "MOCK_USER != PETROV"
        # instead of "PETROV != PETROV". (The mirror-image "foreign user
        # rejected" test happened to still pass, by coincidence - the
        # hardcoded default also differs from a real foreign user, so it
        # never actually proved the header was being read.)
        root_id = "delete-header-regression-root"
        serve.store["CheckRoots"][root_id] = {
            "RootId": root_id, "ActiveUUID": root_id, "DraftUUID": serve.ZERO_GUID,
            "IsActiveEntity": True, "DocId": "DELETE-HDR-TEST", "Status": "OK",
            "LastChangedAt": serve.odata_date(), "CreatedAt": serve.odata_date(),
        }
        serve.store["CheckBasics"][root_id] = {"RootId": root_id, "LastChangedAt": serve.odata_date()}
        try:
            _status, resp, _c = serve.dispatch_request(
                "POST", "CheckRootPreparationAction?ActiveUUID=guid'%s'" % root_id,
                headers={"x-mock-user": "PETROV"},
            )
            draft_uuid = resp["d"]["DraftUUID"]
            draft_key = "CheckRoots(ActiveUUID=guid'%s',DraftUUID=guid'%s')" % (root_id, draft_uuid)

            status, resp, _c = serve.dispatch_request(
                "DELETE", draft_key, headers={"x-mock-user": "PETROV", "if-match": "*"},
            )
            self.assertEqual(status, 204, "owner deleting their own draft must succeed, not 409")
            self.assertNotIn(draft_uuid, serve.draft_store["CheckRoots"])
        finally:
            serve.store["CheckRoots"].pop(root_id, None)
            serve.store["CheckBasics"].pop(root_id, None)
            serve.draft_store["CheckRoots"].pop(root_id, None)


class ForeignUserWriteLockTests(unittest.TestCase):
    """[Fix, exhaustive-sweep pass] sap.ui.generic.app's TransactionController.
    editEntity only runs the foreign-lock check ONCE, at the initial Edit
    click (see ForeignUserLockTests) - a raw PATCH/MERGE/PUT or DELETE, or a
    CheckRootActivationAction/CheckRootDiscardAction call, addressed directly
    by an EXISTING draft's own DraftUUID key, never went through that gate at
    all. Without draft_service._draft_owner_mismatch, any mock user who knew/
    guessed another user's DraftUUID could hijack their in-progress draft
    outright - these tests cover every write path that needed the same
    re-check CheckRootPreparationAction already had."""

    def setUp(self):
        self.root_id = "write-lock-test-root"
        serve.store["CheckRoots"][self.root_id] = {
            "RootId": self.root_id, "ActiveUUID": self.root_id, "DraftUUID": serve.ZERO_GUID,
            "IsActiveEntity": True, "DocId": "WRITE-LOCK-TEST", "Status": "OK",
            "LastChangedAt": serve.odata_date(), "CreatedAt": serve.odata_date(),
        }
        serve.store["CheckBasics"][self.root_id] = {"RootId": self.root_id, "LastChangedAt": serve.odata_date()}
        status, resp, _c = serve.dispatch_request(
            "POST", "CheckRootPreparationAction?ActiveUUID=guid'%s'" % self.root_id,
            headers={"x-mock-user": "PETROV"},
        )
        self.assertEqual(status, 200)
        self.draft_uuid = resp["d"]["DraftUUID"]

    def tearDown(self):
        serve.store["CheckRoots"].pop(self.root_id, None)
        serve.store["CheckBasics"].pop(self.root_id, None)
        serve.draft_store["CheckRoots"].clear()

    def _draft_key(self):
        return "CheckRoots(ActiveUUID=guid'%s',DraftUUID=guid'%s')" % (self.root_id, self.draft_uuid)

    def test_patch_by_foreign_user_is_rejected(self):
        status, resp, _c = serve.dispatch_request(
            "PATCH", self._draft_key(), body={"Equipment": "hijacked"},
            headers={"x-mock-user": "SIDOROVA", "if-match": "*"},
        )
        self.assertEqual(status, 409)
        self.assertEqual(resp["error"]["code"], "DRAFT_LOCKED")
        self.assertNotEqual(serve.store["CheckBasics"][self.root_id].get("Equipment"), "hijacked")

    def test_patch_by_owning_user_still_succeeds(self):
        status, _resp, _c = serve.dispatch_request(
            "PATCH", self._draft_key(), body={"Equipment": "legit-edit"},
            headers={"x-mock-user": "PETROV", "if-match": "*"},
        )
        self.assertEqual(status, 204)
        self.assertEqual(serve.store["CheckBasics"][self.root_id]["Equipment"], "legit-edit")

    def test_delete_by_foreign_user_is_rejected(self):
        status, resp, _c = serve.dispatch_request(
            "DELETE", self._draft_key(), headers={"x-mock-user": "SIDOROVA", "if-match": "*"},
        )
        self.assertEqual(status, 409)
        self.assertEqual(resp["error"]["code"], "DRAFT_LOCKED")
        self.assertIn(self.draft_uuid, serve.draft_store["CheckRoots"])

    def test_activate_by_foreign_user_is_rejected(self):
        status, resp, _c = serve.dispatch_request(
            "POST", "CheckRootActivationAction?DraftUUID=guid'%s'" % self.draft_uuid,
            headers={"x-mock-user": "SIDOROVA"},
        )
        self.assertEqual(status, 409)
        self.assertEqual(resp["error"]["code"], "DRAFT_LOCKED")
        self.assertIn(self.draft_uuid, serve.draft_store["CheckRoots"])

    def test_discard_by_foreign_user_is_rejected(self):
        status, resp, _c = serve.dispatch_request(
            "POST", "CheckRootDiscardAction?DraftUUID=guid'%s'" % self.draft_uuid,
            headers={"x-mock-user": "SIDOROVA"},
        )
        self.assertEqual(status, 409)
        self.assertEqual(resp["error"]["code"], "DRAFT_LOCKED")
        self.assertIn(self.draft_uuid, serve.draft_store["CheckRoots"])

    def test_activate_by_owning_user_still_succeeds(self):
        # Zero-regression: fill in required fields so activation actually
        # succeeds rather than hitting the (unrelated) validation gate.
        serve.dispatch_request(
            "PATCH", self._draft_key(),
            body={
                "ObserverFullname": "X", "ObservedFullname": "Y", "Date": serve.odata_date(),
                "Time": "PT09H00M00S", "Timezone": "Europe/Moscow", "LpcKey": "1", "ProfKey": "ELECTRICIAN",
            },
            headers={"x-mock-user": "PETROV", "if-match": "*"},
        )
        status, _resp, _c = serve.dispatch_request(
            "POST", "CheckRootActivationAction?DraftUUID=guid'%s'" % self.draft_uuid,
            headers={"x-mock-user": "PETROV"},
        )
        self.assertEqual(status, 200)
        self.assertNotIn(self.draft_uuid, serve.draft_store["CheckRoots"])


class FieldLevelValidationMessageTests(unittest.TestCase):
    """[Fix] VALIDATION_ERROR on Activate now carries innererror.errordetails[]
    with a `target` per missing property, alongside the existing combined-text
    message - see sap/ui/core's ODataMessageParser._parseBodyJSON, which pushes
    every errordetails[] entry through the same _createMessage that reads
    `.target`, letting a SmartField bind its own red-underline instead of only
    a page-level MessageBox."""

    def setUp(self):
        # Deliberately missing ObserverFullname/ObservedFullname/Time/ProfKey -
        # only Date/LpcKey supplied - to get a mix of missing fields.
        body = {"Date": "2026-07-27T00:00:00.000Z", "LpcKey": "2"}
        _status, resp, _c = serve.dispatch_request("POST", "CheckRoots", body=body)
        self.root_id = resp["d"]["RootId"]

    def tearDown(self):
        serve.store["CheckRoots"].pop(self.root_id, None)
        serve.store["CheckBasics"].pop(self.root_id, None)
        serve.draft_store["CheckRoots"].pop(self.root_id, None)

    def test_activation_with_missing_fields_returns_errordetails_with_targets(self):
        status, resp, _c = serve.dispatch_request(
            "POST", "CheckRootActivationAction?DraftUUID=guid'%s'" % self.root_id,
        )
        self.assertEqual(status, 400)
        self.assertEqual(resp["error"]["code"], "VALIDATION_ERROR")
        details = resp["error"]["innererror"]["errordetails"]
        targets = {d["target"] for d in details}
        # ObserverFullname/ObservedFullname have Perner alternates (unset here
        # too, so both are genuinely missing); Time/Timezone/ProfKey are plain.
        self.assertEqual(targets, {"ObserverFullname", "ObservedFullname", "Time", "Timezone", "ProfKey"})
        for d in details:
            self.assertEqual(d["severity"], "error")
        # The draft must be left untouched (not activated, not popped).
        self.assertIn(self.root_id, serve.draft_store["CheckRoots"])

    def test_activation_error_details_are_a_list_of_dicts_with_message_and_code(self):
        _status, resp, _c = serve.dispatch_request(
            "POST", "CheckRootActivationAction?DraftUUID=guid'%s'" % self.root_id,
        )
        details = resp["error"]["innererror"]["errordetails"]
        self.assertTrue(len(details) > 0)
        for d in details:
            self.assertEqual(d["code"], "VALIDATION_ERROR")
            self.assertIn("value", d["message"])


class ChildRowValidationMessageTests(unittest.TestCase):
    """[Fix, exhaustive-sweep pass] CheckItem/Barrier Result is now required
    at Activation too (config.REQUIRED_CHILD_FIELD), not just root fields -
    a checklist line with no recorded outcome is meaningless data. Each
    missing row gets its own nav-qualified errordetails[] target
    ("to_Checks(RootId=..,ItemId=..)/Result") instead of a bare field name,
    since a bare "Result" target would be ambiguous across multiple rows -
    see draft_service._missing_required_child_fields/dispatch.py's
    VALIDATION_ERROR branch. Confirmed via the real ODataMessageParser.
    _createTarget source that this is a first-class supported target shape
    (string-concatenated with the FunctionImport's own resolved entity path,
    then run through the model's normal canonical-path resolution - no
    dependency on $batch/changeset context)."""

    ALL_REQUIRED_ROOT_FIELDS = {
        "ObserverFullname": "X", "ObservedFullname": "Y", "Date": "2026-07-27T00:00:00.000Z",
        "Time": "PT09H00M00S", "Timezone": "Europe/Moscow", "LpcKey": "1", "ProfKey": "ELECTRICIAN",
    }

    def setUp(self):
        _status, resp, _c = serve.dispatch_request("POST", "CheckRoots", body=dict(self.ALL_REQUIRED_ROOT_FIELDS))
        self.root_id = resp["d"]["RootId"]
        self.draft_key = "CheckRoots(ActiveUUID=guid'%s',DraftUUID=guid'%s')" % (serve.ZERO_GUID, self.root_id)

    def tearDown(self):
        serve.store["CheckRoots"].pop(self.root_id, None)
        serve.store["CheckBasics"].pop(self.root_id, None)
        serve.draft_store["CheckRoots"].pop(self.root_id, None)
        for key in [k for k in serve.store["CheckItems"] if k[0] == self.root_id]:
            del serve.store["CheckItems"][key]

    def _add_check_item(self, result=None):
        body = {"Code": "VISUAL_INSPECTION"}
        if result is not None:
            body["Result"] = result
        status, resp, _c = serve.dispatch_request("POST", self.draft_key + "/to_Checks", body=body)
        self.assertEqual(status, 201)
        return resp["d"]["ItemId"]

    def _activate(self):
        return serve.dispatch_request("POST", "CheckRootActivationAction?DraftUUID=guid'%s'" % self.root_id)

    def test_root_fields_alone_activate_successfully_with_no_children(self):
        status, _resp, _c = self._activate()
        self.assertEqual(status, 200)

    def test_child_row_missing_result_blocks_activation_with_nav_qualified_target(self):
        item_id = self._add_check_item()  # Result omitted entirely -> None
        status, resp, _c = self._activate()
        self.assertEqual(status, 400)
        targets = {d["target"] for d in resp["error"]["innererror"]["errordetails"]}
        self.assertEqual(targets, {"to_Checks(RootId='%s',ItemId='%s')/Result" % (self.root_id, item_id)})
        # Draft untouched - not activated, not popped.
        self.assertIn(self.root_id, serve.draft_store["CheckRoots"])

    def test_child_row_with_recorded_result_activates_successfully(self):
        self._add_check_item(result="X")
        status, _resp, _c = self._activate()
        self.assertEqual(status, 200)

    def test_child_row_with_explicit_unsatisfactory_result_is_not_missing(self):
        # "" is a real CheckResults code (Неудовлетворительно), not "unset".
        self._add_check_item(result="")
        status, _resp, _c = self._activate()
        self.assertEqual(status, 200)

    def test_multiple_incomplete_rows_each_get_their_own_target(self):
        item_id_1 = self._add_check_item()
        item_id_2 = self._add_check_item()
        status, resp, _c = self._activate()
        self.assertEqual(status, 400)
        targets = {d["target"] for d in resp["error"]["innererror"]["errordetails"]}
        self.assertEqual(targets, {
            "to_Checks(RootId='%s',ItemId='%s')/Result" % (self.root_id, item_id_1),
            "to_Checks(RootId='%s',ItemId='%s')/Result" % (self.root_id, item_id_2),
        })


class BatchChangesetAtomicityTests(unittest.TestCase):
    """[Fix] OData v2 $batch spec: "If any request within a change set
    fails, the entire change set MUST be rolled back... the response for
    the change set MUST be a single response, and MUST be a response for
    the failed request." Confirmed live (before this fix) that a 2-request
    changeset with one valid and one bogus DELETE came back as a mixed
    204+404 nested multipart, with the valid delete PERMANENTLY applied -
    exactly the non-atomic behavior the spec forbids."""

    def setUp(self):
        self.root_id = "atomic-test-root"
        self.item_id = "atomic-test-item"
        serve.store["CheckRoots"][self.root_id] = {
            "RootId": self.root_id, "ActiveUUID": self.root_id, "DraftUUID": serve.ZERO_GUID,
            "IsActiveEntity": True, "DocId": "ATOMIC-TEST", "Status": "OK",
            "LastChangedAt": serve.odata_date(), "CreatedAt": serve.odata_date(),
        }
        serve.store["CheckItems"][(self.root_id, self.item_id)] = {
            "RootId": self.root_id, "ItemId": self.item_id, "Code": "VISUAL_INSPECTION",
            "RawText": "", "Comment": "", "Result": "X", "DraftUUID": serve.ZERO_GUID,
            "LastChangedAt": serve.odata_date(),
        }

    def tearDown(self):
        serve.store["CheckRoots"].pop(self.root_id, None)
        serve.store["CheckItems"].pop((self.root_id, self.item_id), None)

    def _mixed_changeset_body(self, boundary, cs_boundary, valid_item_id, bogus_item_id):
        return (
            "--%s\r\n" % boundary +
            "Content-Type: multipart/mixed; boundary=%s\r\n\r\n" % cs_boundary +
            "--%s\r\n" % cs_boundary +
            "Content-Type: application/http\r\nContent-Transfer-Encoding: binary\r\n\r\n" +
            "DELETE CheckItems(RootId='%s',ItemId='%s') HTTP/1.1\r\n" % (self.root_id, valid_item_id) +
            "If-Match: *\r\n\r\n" +
            "--%s\r\n" % cs_boundary +
            "Content-Type: application/http\r\nContent-Transfer-Encoding: binary\r\n\r\n" +
            "DELETE CheckItems(RootId='%s',ItemId='%s') HTTP/1.1\r\n" % (self.root_id, bogus_item_id) +
            "If-Match: *\r\n\r\n" +
            "--%s--\r\n" % cs_boundary +
            "--%s--" % boundary
        )

    def test_failing_request_rolls_back_the_whole_changeset(self):
        boundary, cs_boundary = "b1", "cs1"
        body = self._mixed_changeset_body(boundary, cs_boundary, self.item_id, "NONEXISTENT-ITEM")
        status, resp_text, ct = serve.handle_batch(body, "multipart/mixed; boundary=%s" % boundary)
        self.assertEqual(status, 202)  # outer $batch envelope is always 202
        # The valid delete must NOT have been applied - rolled back with the rest.
        self.assertIn((self.root_id, self.item_id), serve.store["CheckItems"])

    def test_failed_changeset_response_is_a_single_flat_error_not_nested_multipart(self):
        boundary, cs_boundary = "b2", "cs2"
        body = self._mixed_changeset_body(boundary, cs_boundary, self.item_id, "NONEXISTENT-ITEM")
        _status, resp_text, _ct = serve.handle_batch(body, "multipart/mixed; boundary=%s" % boundary)
        # Spec: a failed changeset's response is ONE flat error part, not a
        # nested multipart/mixed changesetresponse - so there must be no
        # second inner boundary in the response body.
        self.assertNotIn("changesetresponse", resp_text)
        self.assertIn("404", resp_text)

    def test_successful_changeset_still_applies_all_requests(self):
        # Zero-regression: an all-succeeding changeset must still work
        # exactly as before (nested multipart, both deletes applied).
        item_id_2 = "atomic-test-item-2"
        serve.store["CheckItems"][(self.root_id, item_id_2)] = {
            "RootId": self.root_id, "ItemId": item_id_2, "Code": "DOCUMENT_REVIEW",
            "RawText": "", "Comment": "", "Result": "X", "DraftUUID": serve.ZERO_GUID,
            "LastChangedAt": serve.odata_date(),
        }
        try:
            boundary, cs_boundary = "b3", "cs3"
            body = self._mixed_changeset_body(boundary, cs_boundary, self.item_id, item_id_2)
            status, resp_text, _ct = serve.handle_batch(body, "multipart/mixed; boundary=%s" % boundary)
            self.assertEqual(status, 202)
            self.assertIn("changesetresponse", resp_text)
            self.assertNotIn((self.root_id, self.item_id), serve.store["CheckItems"])
            self.assertNotIn((self.root_id, item_id_2), serve.store["CheckItems"])
        finally:
            serve.store["CheckItems"].pop((self.root_id, item_id_2), None)


class BadgeCriticalityTests(unittest.TestCase):
    """[Fix, minimal-extension-set pass] Checks/Barriers/Integration error
    badges (annotations.xml check-root.xml FieldGroup#General) need a Path-
    bound Criticality field to render as a colored sap.m.ObjectStatus icon
    instead of a plain checkbox - this only surfaced live because the field
    was computed in compute_check_root_view() but had no matching <Property>
    in metadata.xml, so the mock server silently dropped it from the wire
    response. No test covered HasErrorChecks/HasErrorBarriers or their
    Criticality companions at all before this."""

    def setUp(self):
        self.root_id = "badge-criticality-test-root"
        serve.store["CheckRoots"][self.root_id] = {
            "RootId": self.root_id, "ActiveUUID": self.root_id, "DraftUUID": serve.ZERO_GUID,
            "IsActiveEntity": True, "DocId": "BADGE-TEST", "Status": "OK",
            "LastChangedAt": serve.odata_date(), "CreatedAt": serve.odata_date(),
        }
        serve.store["CheckBasics"][self.root_id] = {"RootId": self.root_id, "LastChangedAt": serve.odata_date()}

    def tearDown(self):
        serve.store["CheckRoots"].pop(self.root_id, None)
        serve.store["CheckBasics"].pop(self.root_id, None)
        serve.store["CheckItems"].pop((self.root_id, "badge-item"), None)
        serve.store["Barriers"].pop((self.root_id, "badge-barrier"), None)

    def test_no_error_rows_gives_hidden_badge_and_none_criticality(self):
        out = serve.compute_check_root_view(self.root_id)
        self.assertFalse(out["HasErrorChecks"])
        self.assertTrue(out["ChecksErrorBadgeHidden"])
        self.assertEqual(out["ChecksErrorCriticality"], 0)
        self.assertEqual(out["IntegrationCriticality"], 0)

    def test_unsatisfactory_check_result_gives_visible_error_criticality_badge(self):
        serve.store["CheckItems"][(self.root_id, "badge-item")] = {
            "RootId": self.root_id, "ItemId": "badge-item", "Code": "VISUAL_INSPECTION",
            "RawText": "", "Comment": "", "Result": "",  # "" = Неудовлетворительно
            "DraftUUID": serve.ZERO_GUID, "LastChangedAt": serve.odata_date(),
        }
        out = serve.compute_check_root_view(self.root_id)
        self.assertTrue(out["HasErrorChecks"])
        self.assertFalse(out["ChecksErrorBadgeHidden"])
        # 1 = ValueState.Error via ODataControlFactory's numeric Criticality
        # map (0=None/1=Error/2=Warning/3=Success) - same convention as the
        # already-working ChecksCriticality/BarriersCriticality/
        # StatusCriticality fields this mirrors.
        self.assertEqual(out["ChecksErrorCriticality"], 1)

    def test_unsatisfactory_barrier_result_gives_visible_error_criticality_badge(self):
        serve.store["Barriers"][(self.root_id, "badge-barrier")] = {
            "RootId": self.root_id, "ItemId": "badge-barrier", "Code": "PHYSICAL_BARRIER",
            "RawText": "", "Comment": "", "Result": "",
            "DraftUUID": serve.ZERO_GUID, "LastChangedAt": serve.odata_date(),
        }
        out = serve.compute_check_root_view(self.root_id)
        self.assertTrue(out["HasErrorBarriers"])
        self.assertFalse(out["BarriersErrorBadgeHidden"])
        self.assertEqual(out["BarriersErrorCriticality"], 1)

    def test_criticality_fields_declared_in_metadata_so_they_reach_the_wire(self):
        # [Fix] The actual regression: compute_check_root_view() alone isn't
        # sufficient - the mock server's response serialization is scoped to
        # metadata.xml's declared <Property> list per EntityType, so a
        # computed dict key with no matching declaration there is silently
        # dropped before it ever reaches the client, even though this exact
        # unit-level call correctly returns it. Guards against that class of
        # bug recurring for these 3 fields specifically.
        metadata = serve.read_static_file("localService/metadata.xml")[0].decode("utf-8")
        for field in ("ChecksErrorCriticality", "BarriersErrorCriticality", "IntegrationCriticality"):
            self.assertIn('Name="%s"' % field, metadata, "%s must be declared in metadata.xml or it never reaches the wire" % field)


class ApplyOrderbyTests(unittest.TestCase):
    """[Fix, use-case pass] apply_orderby() called `functools.cmp_to_key`
    with only `from functools import lru_cache` imported - a real NameError
    on every $orderby request, undetected by any test until the new
    standard ValueHelpDialog (see SubTableCrud.js) started sending $orderby
    for its own default column sort and hit this live (server log:
    "NameError: name 'functools' is not defined")."""

    def test_orderby_does_not_raise_and_sorts_ascending(self):
        data = [{"Text": "Проверка документов"}, {"Text": "Аудит процессов"}, {"Text": "Опрос персонала"}]
        out = serve.apply_orderby(data, "Text asc")
        self.assertEqual([r["Text"] for r in out], ["Аудит процессов", "Опрос персонала", "Проверка документов"])

    def test_orderby_desc(self):
        data = [{"Text": "A"}, {"Text": "C"}, {"Text": "B"}]
        out = serve.apply_orderby(data, "Text desc")
        self.assertEqual([r["Text"] for r in out], ["C", "B", "A"])

    def test_orderby_none_returns_input_unchanged(self):
        data = [{"Text": "B"}, {"Text": "A"}]
        self.assertEqual(serve.apply_orderby(data, None), data)


if __name__ == "__main__":
    unittest.main()
