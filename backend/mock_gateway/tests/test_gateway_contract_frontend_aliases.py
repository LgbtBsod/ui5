import os
import sys
import uuid

from fastapi.testclient import TestClient

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from main import app  # noqa: E402
from services import current_user_service  # noqa: E402
from utils.odata import ODATA_NS, SERVICE_ROOT  # noqa: E402

def _as_user(user_name: str) -> dict:
    current_user_service.ALLOW_MOCK_USER_HEADER = True
    return {"X-Mock-User": user_name}

def test_gateway_canonical_contract_and_metadata():
    with TestClient(app) as client:
        token = _csrf(client)
        root_key = _sample_root(client)

        acquire = client.post(f"{SERVICE_ROOT}/LockAcquire", params={"DB_KEY": root_key, "SessionGuid": "S1"}, headers={"X-CSRF-Token": token})
        assert acquire.status_code == 200 and "d" in acquire.json()
        assert isinstance(acquire.json().get("d", {}).get("Owner"), str)
        heartbeat = client.post(f"{SERVICE_ROOT}/LockHeartbeat", params={"DB_KEY": root_key, "SessionGuid": "S1"}, headers={"X-CSRF-Token": token})
        assert heartbeat.status_code == 200 and "d" in heartbeat.json()
        release = client.post(f"{SERVICE_ROOT}/LockRelease", params={"DB_KEY": root_key, "SessionGuid": "S1"}, headers={"X-CSRF-Token": token})
        assert release.status_code == 200 and "d" in release.json()

        lock_status = client.get(f"{SERVICE_ROOT}/LockStatusSet({root_key})", params={"SessionGuid": "S1"})
        assert lock_status.status_code == 200 and "d" in lock_status.json()

        permission = client.get(f"{SERVICE_ROOT}/ChecklistPermissionSet({root_key})", headers=_as_user("demoUser"))
        assert permission.status_code == 200 and "d" in permission.json()
        permission_body = permission.json().get("d", {})
        assert permission_body.get("DB_KEY") == root_key
        assert permission_body.get("AuthObject") == "Z_UI5_CHKL"
        assert permission_body.get("ViewOperation") == "03"
        assert permission_body.get("ChangeOperation") == "02"
        assert permission_body.get("DeleteOperation") == "06"

        runtime_permission = client.get(f"{SERVICE_ROOT}/ChecklistPermissionSet({root_key})")
        assert runtime_permission.status_code == 200 and "d" in runtime_permission.json()
        assert runtime_permission.json().get("d", {}).get("UserId") == "operator"
        assert permission_body.get("GrantedOperations") == "02,03,06"
        assert permission_body.get("CanView") is True
        assert permission_body.get("CanEdit") is True
        assert permission_body.get("CanDelete") is True

        binary_permission = client.get(
            f"{SERVICE_ROOT}/ChecklistPermissionSet(binary'{root_key}')",
            headers=_as_user("demoUser")
        )
        assert binary_permission.status_code == 200 and "d" in binary_permission.json()
        assert binary_permission.json().get("d", {}).get("DB_KEY") == root_key

        current_user = client.get(f"{SERVICE_ROOT}/CurrentUserSet('CURRENT')", headers=_as_user("demoUser"))
        assert current_user.status_code == 200 and "d" in current_user.json()
        current_user_body = current_user.json().get("d", {})
        assert "Uname" not in current_user_body
        assert current_user_body.get("FullName")

        create_permission = client.get(f"{SERVICE_ROOT}/ChecklistCreatePermissionSet('CURRENT')")
        assert create_permission.status_code == 200 and "d" in create_permission.json()
        create_permission_body = create_permission.json().get("d", {})
        assert create_permission_body.get("DB_KEY") == "CURRENT"
        assert create_permission_body.get("AuthObject") == "Z_UI5_CHKL"
        assert create_permission_body.get("CreateOperation") == "01"
        assert create_permission_body.get("CanCreate") is True
        assert create_permission_body.get("GrantedOperations") == "01"

def test_create_draft_lock_imports_are_benign_for_local_runtime():
    with TestClient(app) as client:
        token = _csrf(client)
        root_key = _sample_root(client)
        create_status = client.get(f"{SERVICE_ROOT}/LockStatusSet('__CREATE')", params={"SessionGuid": "S-CREATE"})
        assert create_status.status_code == 200 and "d" in create_status.json()
        assert create_status.json().get("d", {}).get("ReasonCode") == "OWNED_BY_YOU"

        heartbeat = client.post(
            f"{SERVICE_ROOT}/LockHeartbeat",
            params={"DB_KEY": "__CREATE", "SessionGuid": "S-CREATE"},
            headers={"X-CSRF-Token": token}
        )
        assert heartbeat.status_code == 200 and "d" in heartbeat.json()
        assert heartbeat.json().get("d", {}).get("Action") == "HEARTBEAT"
        assert heartbeat.json().get("d", {}).get("ReasonCode") == "OWNED_BY_YOU"

        release = client.post(
            f"{SERVICE_ROOT}/LockRelease",
            params={"DB_KEY": "__CREATE", "SessionGuid": "S-CREATE"},
            headers={"X-CSRF-Token": token}
        )
        assert release.status_code == 200 and "d" in release.json()
        assert release.json().get("d", {}).get("Action") == "RELEASED"
        assert release.json().get("d", {}).get("ReasonCode") == "FREE"

        checks = client.get(f"{SERVICE_ROOT}/ChecklistCheckSet", params={"$filter": f"PARENT_KEY eq '{root_key}'"})
        barriers = client.get(f"{SERVICE_ROOT}/ChecklistBarrierSet", params={"$filter": f"PARENT_KEY eq '{root_key}'"})
        assert checks.status_code == 200 and "d" in checks.json()
        assert barriers.status_code == 200 and "d" in barriers.json()
        seeded_basic = client.get(f"{SERVICE_ROOT}/ChecklistBasicInfoSet", params={"$filter": f"DB_KEY eq '{root_key}'"})
        assert seeded_basic.status_code == 200
        seeded_basic_rows = seeded_basic.json().get("d", {}).get("results", [])
        assert len(seeded_basic_rows) == 1
        assert seeded_basic_rows[0].get("DateCheck")
        assert seeded_basic_rows[0].get("EquipName")
        assert seeded_basic_rows[0].get("ChecklistId")
        assert "ObserverFullname" in seeded_basic_rows[0]
        assert "ObserverPosition" in seeded_basic_rows[0]
        assert "ObserverOrgUnit" in seeded_basic_rows[0]
        assert "ObservedFullname" in seeded_basic_rows[0]
        assert "ObservedPosition" in seeded_basic_rows[0]
        assert "ObservedOrgUnit" in seeded_basic_rows[0]
        assert "LocationName" in seeded_basic_rows[0]
        assert "LocationText" in seeded_basic_rows[0]
        assert "LpcText" in seeded_basic_rows[0]
        assert "ProfessionText" in seeded_basic_rows[0]
        assert len(checks.json().get("d", {}).get("results", [])) >= 1
        assert isinstance(barriers.json().get("d", {}).get("results", []), list)

        create_payload = {
            "FullPayload": {
                "root": {"id": "__CREATE", "status": "DRAFT"},
                "basic": {
                    "date": "2026-03-02",
                    "equipment": "Pump Local A1",
                    "LOCATION_KEY": "LOC-001-01-01",
                    "LOCATION_NAME": "Area A",
                    "LOCATION_TEXT": "Area A",
                    "OBSERVER_FULLNAME": "Observer One",
                    "OBSERVED_FULLNAME": "Observed One",
                    "LPC_KEY": "L2",
                    "PROF_KEY": "PR1",
                },
                "checks": [{"ChecksNum": 1, "text": "Check via create payload", "result": False}],
                "barriers": [{"BarriersNum": 1, "comment": "Barrier via create payload", "result": True}],
                "attachments": [],
        }
        }
        created = client.post(f"{SERVICE_ROOT}/CreateChecklist", json=create_payload, headers={"X-CSRF-Token": token})
        assert created.status_code == 200
        created_payload = created.json().get("d", {})
        created_key = created_payload.get("DB_KEY")
        assert created_key
        assert created_payload.get("Id")
        assert created_payload.get("AggChangedOn")
        assert created_payload.get("VersionNumber") == 1
        read_created = client.get(f"{SERVICE_ROOT}/ChecklistRootSet({created_key})")
        assert read_created.status_code == 200 and "d" in read_created.json()
        root_payload = read_created.json().get("d", {})
        assert root_payload.get("VersionNumber") == 1
        created_attachments = client.get(f"{SERVICE_ROOT}/AttachmentSet", params={"$filter": f"PARENT_KEY eq '{created_key}'"})
        assert created_attachments.status_code == 200
        assert isinstance(created_attachments.json().get("d", {}).get("results", []), list)

        status_update = client.post(
            f"{SERVICE_ROOT}/SetChecklistStatus",
            json={"DB_KEY": created_key, "NewStatus": "SUBMITTED", "ClientAggChangedOn": created_payload.get("AggChangedOn")},
            headers={"X-CSRF-Token": token},
        )
        assert status_update.status_code == 200 and "d" in status_update.json()

        missing_lock = client.post(
            f"{SERVICE_ROOT}/SaveChanges",
            json={"root": {"db_key": created_key}, "checks": [], "barriers": [], "client_version": 1, "SessionGuid": "S2"},
            headers={"X-CSRF-Token": token},
        )
        assert missing_lock.status_code == 409

        reacquire = client.post(
            f"{SERVICE_ROOT}/LockAcquire",
            params={"DB_KEY": created_key, "SessionGuid": "S2"},
            headers={"X-CSRF-Token": token},
        )
        assert reacquire.status_code == 200 and reacquire.json().get("d", {}).get("Ok") is True

        save_payload = {
            "root": {"db_key": created_key, "equipment": "Pump Local A2"},
            "checks": [],
            "barriers": [],
            "attachments": [],
            "client_version": 1,
            "SessionGuid": "S2",
        }
        saved = client.post(f"{SERVICE_ROOT}/SaveChanges", json=save_payload, headers={"X-CSRF-Token": token})
        assert saved.status_code == 200
        saved_body = saved.json().get("d", {})
        assert saved_body.get("db_key") == created_key
        assert saved_body.get("version_number") == 2
        assert saved_body.get("changed_on")
        assert saved_body.get("code") == "LOCK_OK"
        assert saved_body.get("lock_refreshed") is True
        assert saved_body.get("lock_expires_at")
        assert saved_body.get("server_now")
        assert saved_body.get("request_id")
        saved_attachments = client.get(f"{SERVICE_ROOT}/AttachmentSet", params={"$filter": f"PARENT_KEY eq '{created_key}'"})
        assert saved_attachments.status_code == 200
        assert isinstance(saved_attachments.json().get("d", {}).get("results", []), list)

        client_row_id = uuid.uuid4().hex.upper()
        autosave_payload = {
            "root": {"db_key": created_key},
            "checks": [{
                "client_row_id": client_row_id,
                "edit_mode": "C",
                "checks_num": 2,
                "text": "Autosave deep delta",
                "comment": "Created by autosave",
                "result": False,
            }],
            "barriers": [],
            "client_version": 2,
            "SessionGuid": "S2",
        }
        autosaved = client.post(f"{SERVICE_ROOT}/AutoSave", json=autosave_payload, headers={"X-CSRF-Token": token})
        assert autosaved.status_code == 200
        autosaved_body = autosaved.json().get("d", {})
        assert autosaved_body.get("db_key") == created_key
        assert autosaved_body.get("version_number") == 3
        assert autosaved_body.get("changed_on")
        assert autosaved_body.get("code") == "LOCK_OK"
        assert autosaved_body.get("lock_refreshed") is True
        assert autosaved_body.get("lock_expires_at")
        assert autosaved_body.get("server_now")
        assert autosaved_body.get("request_id")

        read_checks = client.get(f"{SERVICE_ROOT}/ChecklistCheckSet", params={"$filter": f"PARENT_KEY eq '{created_key}'"})
        assert read_checks.status_code == 200
        check_rows = read_checks.json().get("d", {}).get("results", [])
        assert len(check_rows) >= 2
        assert all(str(row.get("DB_KEY") or "").upper() != client_row_id for row in check_rows)

        copied = client.post(
            f"{SERVICE_ROOT}/CopyChecklist",
            params={"DB_KEY": created_key, "SessionGuid": "S3"},
            headers={"X-CSRF-Token": token},
        )
        assert copied.status_code == 200
        copied_body = copied.json().get("d", {})
        copied_key = copied_body.get("DB_KEY")
        assert copied_key
        assert copied_key != created_key
        assert copied_body.get("ReasonCode") == "COPIED"
        assert copied_body.get("AggChangedOn")
        copied_root = client.get(f"{SERVICE_ROOT}/ChecklistRootSet({copied_key})")
        assert copied_root.status_code == 200
        copied_root_body = copied_root.json().get("d", {})
        assert copied_root_body.get("VersionNumber") == 1
        copied_basic = client.get(f"{SERVICE_ROOT}/ChecklistBasicInfoSet", params={"$filter": f"DB_KEY eq '{copied_key}'"})
        assert copied_basic.status_code == 200
        copied_basic_rows = copied_basic.json().get("d", {}).get("results", [])
        assert copied_basic_rows
        assert copied_basic_rows[0].get("DateCheck")
        copied_attachments = client.get(f"{SERVICE_ROOT}/AttachmentSet", params={"$filter": f"PARENT_KEY eq '{copied_key}'"})
        assert copied_attachments.status_code == 200
        copied_attachment_rows = copied_attachments.json().get("d", {}).get("results", [])
        assert isinstance(copied_attachment_rows, list)
        assert all(row.get("AttachmentKey") for row in copied_attachment_rows)
        copied_checks = client.get(f"{SERVICE_ROOT}/ChecklistCheckSet", params={"$filter": f"PARENT_KEY eq '{copied_key}'"})
        assert copied_checks.status_code == 200
        copied_check_rows = copied_checks.json().get("d", {}).get("results", [])
        assert any(row.get("Comment") == "Created by autosave" for row in copied_check_rows)
        copied_barriers = client.get(f"{SERVICE_ROOT}/ChecklistBarrierSet", params={"$filter": f"PARENT_KEY eq '{copied_key}'"})
        assert copied_barriers.status_code == 200
        copied_barrier_rows = copied_barriers.json().get("d", {}).get("results", [])
        assert isinstance(copied_barrier_rows, list)
        copied_lock = client.get(f"{SERVICE_ROOT}/LockStatusSet({copied_key})", params={"SessionGuid": "S3"})
        assert copied_lock.status_code == 200
        copied_lock_body = copied_lock.json().get("d", {})
        assert copied_lock_body.get("Ok") is True
        assert copied_lock_body.get("ReasonCode") == "OWNED_BY_YOU"
        copied_save = client.post(
            f"{SERVICE_ROOT}/SaveChanges",
            json={
                "root": {"db_key": copied_key, "equipment": "Pump Local Copy"},
                "checks": [],
                "barriers": [],
                "client_version": 1,
                "SessionGuid": "S3",
            },
            headers={"X-CSRF-Token": token},
        )
        assert copied_save.status_code == 200
        copied_save_body = copied_save.json().get("d", {})
        assert copied_save_body.get("db_key") == copied_key
        assert copied_save_body.get("version_number") == 2
        reopened_copy = client.get(f"{SERVICE_ROOT}/ChecklistRootSet({copied_key})")
        assert reopened_copy.status_code == 200
        reopened_copy_body = reopened_copy.json().get("d", {})
        assert reopened_copy_body.get("VersionNumber") == 2

        stale = client.post(f"{SERVICE_ROOT}/AutoSave", json=autosave_payload, headers={"X-CSRF-Token": token})
        assert stale.status_code == 200

        metadata = client.get(f"{SERVICE_ROOT}/$metadata").text
        assert 'EntitySet Name="ChecklistBasicInfoSet"' in metadata
        assert 'EntitySet Name="ChecklistBasicInfoSet" EntityType="' + ODATA_NS + '.ChecklistBasicInfo" sap:creatable="false" sap:updatable="false" sap:deletable="false" sap:searchable="false" sap:requires-filter="true" sap:addressable="true"' in metadata
        assert 'EntitySet Name="ChecklistCheckSet"' in metadata
        assert 'EntitySet Name="ChecklistBarrierSet"' in metadata
        assert 'EntitySet Name="PersonVHSet"' in metadata
        assert 'EntitySet Name="ChecklistPermissionSet"' in metadata
        assert 'EntitySet Name="ChecklistCreatePermissionSet"' in metadata
        assert 'EntityType Name="ChecklistPermission"' in metadata
        assert 'EntityType Name="Attachment"' in metadata
        assert 'm:HasStream="true"' in metadata
        assert 'Property Name="Value" Type="Edm.Binary"' not in metadata
        assert 'Property Name="CreateOperation" Type="Edm.String"' in metadata
        assert 'Property Name="CanCreate" Type="Edm.Boolean"' in metadata
        assert 'Property Name="ViewOperation" Type="Edm.String"' in metadata
        assert 'Property Name="ChangeOperation" Type="Edm.String"' in metadata
        assert 'Property Name="DeleteOperation" Type="Edm.String"' in metadata
        assert 'Property Name="GrantedOperations" Type="Edm.String"' in metadata
        assert 'Property Name="Uname"' not in metadata
        assert 'EntityType Name="SaveChangesResponse"' in metadata
        assert 'Property Name="code" Type="Edm.String"' in metadata
        assert 'Property Name="reason_code" Type="Edm.String"' not in metadata
        assert 'Property Name="lock_refreshed" Type="Edm.Boolean"' in metadata
        assert 'Property Name="lock_expires_at" Type="Edm.DateTime"' in metadata
        assert 'Property Name="server_now" Type="Edm.DateTime"' in metadata
        assert 'Property Name="request_id" Type="Edm.String"' in metadata
        assert 'FunctionImport Name="AutoSave" ReturnType="' + ODATA_NS + '.SaveChangesResponse"' in metadata
        assert 'FunctionImport Name="SaveChanges" ReturnType="' + ODATA_NS + '.SaveChangesResponse"' in metadata
        for name in ["CreateChecklist", "CopyChecklist", "AutoSave", "SaveChanges", "LockAcquire", "LockHeartbeat", "LockRelease"]:
            assert f'FunctionImport Name="{name}"' in metadata
        for name in ["AutosaveChecklist", "UpdateChecklist", "LockStatus", "LockControl"]:
            assert f'FunctionImport Name="{name}"' not in metadata

def test_legacy_alias_paths_are_not_exposed():
    with TestClient(app) as client:
        token = _csrf(client)

        for path in ["/actions/CreateChecklist", "/actions/SaveChecklist", "/actions/AutoSaveChecklist", "/actions/SetChecklistStatus"]:
            resp = client.post(path, headers={"X-CSRF-Token": token})
            assert resp.status_code == 404

        for path in ["/lock/acquire", "/lock/heartbeat", "/lock/release", "/lock/status"]:
            resp = client.post(path, headers={"X-CSRF-Token": token}) if path != "/lock/status" else client.get(path)
            assert resp.status_code == 404

        for path in ["/config/frontend", f"{SERVICE_ROOT}/config/frontend", "/FrontendRuntimeSettings", "/capabilities", f"{SERVICE_ROOT}/SearchRows", f"{SERVICE_ROOT}/ChecklistRoots"]:
            resp = client.get(path)
            assert resp.status_code == 404

def test_error_payload_uses_errordetails():
    with TestClient(app) as client:
        token = _csrf(client)
        resp = client.post(f"{SERVICE_ROOT}/LockAcquire", params={"SessionGuid": "S1"}, headers={"X-CSRF-Token": token})
        assert resp.status_code in (400, 422)
        payload = resp.json()
        if "error" in payload:
            assert "innererror" in payload["error"]
            assert "errordetails" in payload["error"]["innererror"]

def test_odata_unknown_path_is_wrapped_with_standard_odata_error():
    with TestClient(app) as client:
        resp = client.get(f"{SERVICE_ROOT}/DefinitelyMissingSet")
        assert resp.status_code == 404
        payload = resp.json()
        assert "error" in payload
        assert "code" in payload["error"]
        assert payload["error"]["code"] == "NOT_FOUND"

def test_ui5_flex_settings_endpoint_exists():
    with TestClient(app) as client:
        resp = client.get("/sap/bc/lrep/flex/settings")
        assert resp.status_code == 200
        payload = resp.json()
        assert payload.get("isProductiveSystem") is False

def test_checklist_search_exposes_rate_totals_for_column_visibility():
    with TestClient(app) as client:
        payload = client.get(f"{SERVICE_ROOT}/ChecklistSearchSet", params={"$top": 1}).json()
        rows = payload.get("d", {}).get("results", [])
        assert rows
        row = rows[0]
        assert "__metadata" in row
        assert row["__metadata"]["type"] == ODATA_NS + ".ChecklistSearch"
        assert row["Key"] == row["DB_KEY"]
        assert "SuccessChecksRate" in row
        assert "SuccessBarriersRate" in row
        assert "ChecksTotal" in row
        assert "BarriersTotal" in row

def test_checklist_search_preserves_metadata_under_select():
    with TestClient(app) as client:
        payload = client.get(
            f"{SERVICE_ROOT}/ChecklistSearchSet",
            params={"$top": 1, "$select": "Id,Key,LpcText,ProfessionText,ChecksTotal,BarriersTotal"},
        ).json()
        rows = payload.get("d", {}).get("results", [])
        assert rows
        row = rows[0]
        assert "__metadata" in row
        assert row["__metadata"]["type"] == ODATA_NS + ".ChecklistSearch"
        assert row["Key"]

def test_workflow_analytics_exposes_summary_and_breakdowns():
    with TestClient(app) as client:
        summary = client.get(f"{SERVICE_ROOT}/SimpleAnalyticalSet")
        assert summary.status_code == 200
        summary_rows = summary.json().get("d", {}).get("results", [])
        assert summary_rows
        assert "Total" in summary_rows[0]
        assert "FailedChecks" in summary_rows[0]
        selected_year = int(summary_rows[0].get("SelectedYear") or 0) or 2026

        detailed = client.get(
            f"{SERVICE_ROOT}/WorkflowAnalyticsBreakdownSet",
            params={"$filter": f"SelectedYear eq {selected_year} and Source eq 'ALL'"},
        )
        assert detailed.status_code == 200
        detailed_rows = detailed.json().get("d", {}).get("results", [])
        assert isinstance(detailed_rows, list)

        missing_filter = client.get(f"{SERVICE_ROOT}/WorkflowAnalyticsBreakdownSet")
        assert missing_filter.status_code == 400

def test_search_fetch_scope_and_export_scope_stay_separate():
    with TestClient(app) as client:
        token = _csrf(client)
        limited_search = client.get(f"{SERVICE_ROOT}/ChecklistSearchSet", params={"$top": 1})
        limited_rows = limited_search.json().get("d", {}).get("results", [])
        assert len(limited_rows) == 1

        export_all = client.post(
            f"{SERVICE_ROOT}/ReportExport",
            json={
                "SelectionMode": "all",
                "Entity": "screen",
                "SearchContract": {},
                "Limit": 200000,
            },
            headers={"X-CSRF-Token": token},
        )
        assert export_all.status_code == 200
        export_rows = export_all.json().get("d", {}).get("results", [])
        assert len(export_rows) > len(limited_rows)

def test_dictionary_item_set_is_reference_data_only_and_runtime_settings_hold_policy_payloads():
    with TestClient(app) as client:
        payload = client.get(
            f"{SERVICE_ROOT}/DictionaryItemSet",
            params={"$filter": "Domain eq 'LPC'", "$orderby": "Domain asc,Key asc", "$inlinecount": "allpages", "$top": 200},
        ).json()
        rows = payload.get("d", {}).get("results", [])
        assert rows
        domains = {str(row.get("Domain") or "") for row in rows}
        assert "LPC" in domains
        assert "WEB_REQUIRED_FIELD" not in domains
        assert "WEB_VARIABLE" not in domains

        runtime_payload = client.get(f"{SERVICE_ROOT}/RuntimeSettingsSet(Key='GLOBAL')").json().get("d", {})
        assert runtime_payload.get("RequiredFieldsJson")
        assert runtime_payload.get("FrontendVariablesJson")
        assert runtime_payload.get("UploadPolicyJson")

def test_checklist_permission_set_supports_view_edit_delete_deny_patterns():
    with TestClient(app) as client:
        root_key = _sample_root(client)

        demo = client.get(f"{SERVICE_ROOT}/ChecklistPermissionSet({root_key})", headers=_as_user("demoUser")).json().get("d", {})
        assert demo.get("GrantedOperations") == "02,03,06"
        assert demo.get("CanView") is True
        assert demo.get("CanEdit") is True
        assert demo.get("CanDelete") is True
        assert demo.get("ReasonCode") == "AUTHORIZED"

        no_view = client.get(f"{SERVICE_ROOT}/ChecklistPermissionSet({root_key})", headers=_as_user("noview_operator")).json().get("d", {})
        assert no_view.get("GrantedOperations") == ""
        assert no_view.get("CanView") is False
        assert no_view.get("CanEdit") is False
        assert no_view.get("CanDelete") is False
        assert no_view.get("ReasonCode") == "NO_VIEW_AUTH"

        read_only = client.get(f"{SERVICE_ROOT}/ChecklistPermissionSet({root_key})", headers=_as_user("readonly_operator")).json().get("d", {})
        assert read_only.get("GrantedOperations") == "03"
        assert read_only.get("CanView") is True
        assert read_only.get("CanEdit") is False
        assert read_only.get("CanDelete") is False
        assert read_only.get("ReasonCode") == "READ_ONLY_AUTH"

        no_delete = client.get(f"{SERVICE_ROOT}/ChecklistPermissionSet({root_key})", headers=_as_user("nodelete_operator")).json().get("d", {})
        assert no_delete.get("GrantedOperations") == "02,03"
        assert no_delete.get("CanView") is True
        assert no_delete.get("CanEdit") is True
        assert no_delete.get("CanDelete") is False
        assert no_delete.get("ReasonCode") == "NO_DELETE_AUTH"

def test_last_change_entity_supports_binary_root_key_literal():
    with TestClient(app) as client:
        root_key = _sample_root(client)

        response = client.get(f"{SERVICE_ROOT}/LastChangeSet(DB_KEY=binary'{root_key}')")
        assert response.status_code == 200
        payload = response.json().get("d", {})
        assert payload.get("DB_KEY") == root_key
        assert payload.get("AggChangedOn")

def test_odata_x_http_method_override_delete_is_supported():
    with TestClient(app) as client:
        token = _csrf(client)

        create_payload = {
            "FullPayload": {
                "root": {"id": "__CREATE", "status": "DRAFT"},
                "basic": {
                    "date": "2026-03-02",
                    "equipment": "Delete me",
                    "LOCATION_KEY": "LOC-001-01-01",
                    "LOCATION_NAME": "Area A",
                    "LOCATION_TEXT": "Area A",
                    "OBSERVER_FULLNAME": "Observer One",
                    "OBSERVED_FULLNAME": "Observed One",
                    "LPC_KEY": "L2",
                    "PROF_KEY": "PR1",
                },
                "checks": [],
                "barriers": [],
            }
        }
        created = client.post(f"{SERVICE_ROOT}/CreateChecklist", json=create_payload, headers={"X-CSRF-Token": token})
        assert created.status_code == 200
        created_key = created.json().get("d", {}).get("DB_KEY")
        assert created_key

        delete_resp = client.post(
            f"{SERVICE_ROOT}/ChecklistRootSet({created_key})",
            headers={
                "X-CSRF-Token": token,
                "X-HTTP-Method": "DELETE",
            },
        )
        assert delete_resp.status_code == 204

        read_after_delete = client.get(f"{SERVICE_ROOT}/ChecklistRootSet({created_key})")
        assert read_after_delete.status_code == 404
