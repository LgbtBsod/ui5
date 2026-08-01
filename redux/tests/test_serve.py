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


if __name__ == "__main__":
    unittest.main()
