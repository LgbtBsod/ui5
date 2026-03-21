CLASS zcl_zodata_contract_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS fill_lock_acquire_result
      IMPORTING
        iv_ok                  TYPE abap_bool
        iv_code                TYPE string
        iv_message             TYPE string OPTIONAL
        iv_owner               TYPE string OPTIONAL
        iv_owner_session       TYPE string OPTIONAL
        iv_tab_session_id      TYPE string OPTIONAL
        iv_object_uuid         TYPE sysuuid_x16 OPTIONAL
        iv_lock_expires        TYPE timestampl OPTIONAL
        iv_server_now          TYPE timestampl OPTIONAL
        iv_lock_refreshed      TYPE abap_bool DEFAULT abap_false
        iv_owner_session_match TYPE abap_bool DEFAULT abap_false
      CHANGING
        cs_result              TYPE zstr_pcct_lock_acquire_rs.

    METHODS fill_lock_heartbeat_result
      IMPORTING
        iv_ok                  TYPE abap_bool
        iv_code                TYPE string
        iv_message             TYPE string OPTIONAL
        iv_owner               TYPE string OPTIONAL
        iv_owner_session       TYPE string OPTIONAL
        iv_tab_session_id      TYPE string OPTIONAL
        iv_object_uuid         TYPE sysuuid_x16 OPTIONAL
        iv_lock_expires        TYPE timestampl OPTIONAL
        iv_server_now          TYPE timestampl OPTIONAL
        iv_lock_refreshed      TYPE abap_bool DEFAULT abap_false
        iv_owner_session_match TYPE abap_bool DEFAULT abap_false
      CHANGING
        cs_result              TYPE zstr_pcct_lock_heartbeat_rs.

    METHODS fill_lock_release_result
      IMPORTING
        iv_ok                  TYPE abap_bool
        iv_code                TYPE string
        iv_message             TYPE string OPTIONAL
        iv_owner               TYPE string OPTIONAL
        iv_owner_session       TYPE string OPTIONAL
        iv_tab_session_id      TYPE string OPTIONAL
        iv_object_uuid         TYPE sysuuid_x16 OPTIONAL
        iv_lock_expires        TYPE timestampl OPTIONAL
        iv_server_now          TYPE timestampl OPTIONAL
        iv_lock_refreshed      TYPE abap_bool DEFAULT abap_false
        iv_owner_session_match TYPE abap_bool DEFAULT abap_false
      CHANGING
        cs_result              TYPE zstr_pcct_lock_release_rs.

    METHODS fill_save_response
      IMPORTING
        iv_pcct_uuid      TYPE sysuuid_x16
        iv_changed_on     TYPE timestampl
        iv_version_number TYPE int4
        iv_is_autosave    TYPE abap_bool
        iv_no_changes     TYPE abap_bool
        iv_code           TYPE string DEFAULT 'LOCK_OK'
        iv_reason_code    TYPE string DEFAULT 'SAVED'
        iv_lock_refreshed TYPE abap_bool DEFAULT abap_true
        iv_lock_expires   TYPE timestampl OPTIONAL
        iv_server_now     TYPE timestampl OPTIONAL
        iv_request_id     TYPE string OPTIONAL
      CHANGING
        cs_result         TYPE zstr_pcct_savechanges_rs.

    METHODS fill_permission_result
      IMPORTING
        iv_root_key    TYPE sysuuid_x16
        iv_user_id     TYPE syuname
        iv_can_create  TYPE abap_bool
        iv_can_view    TYPE abap_bool
        iv_can_edit    TYPE abap_bool
        iv_can_delete  TYPE abap_bool
        iv_reason_code TYPE string
        iv_message     TYPE string
      CHANGING
        cs_result      TYPE zstr_pcct_permission_rs.

    METHODS fill_current_user
      IMPORTING
        iv_full_name        TYPE string
        iv_permissions_csv  TYPE string
        iv_permission_rules TYPE string
        iv_can_view         TYPE abap_bool
        iv_can_edit         TYPE abap_bool
        iv_can_delete       TYPE abap_bool
        iv_summary_text     TYPE string
      CHANGING
        cs_result           TYPE zstr_pcct_current_user_rs.

    METHODS fill_runtime_settings
      IMPORTING
        iv_environment          TYPE string
        iv_heartbeat_ms         TYPE int4
        iv_idle_ms              TYPE int4
        iv_autosave_interval_ms TYPE int4
        iv_lock_refresh_ms      TYPE int4
        iv_analytics_refresh_ms TYPE int4
        iv_gcd_interval_ms      TYPE int4
        iv_network_grace_ms     TYPE int4
        iv_cache_tolerance_ms   TYPE int4
      CHANGING
        cs_result      TYPE zstr_pcct_runtime_settings_rs.
ENDCLASS.

CLASS zcl_zodata_contract_service IMPLEMENTATION.

  METHOD fill_lock_acquire_result.
    cs_result-ok = iv_ok.
    cs_result-success = iv_ok.
    cs_result-code = iv_code.
    cs_result-reason_code = iv_code.
    cs_result-action = zif_zodata_contract_constants=>c_action_acquired.
    IF iv_message IS NOT INITIAL.
      cs_result-message = iv_message.
    ENDIF.
    cs_result-owner = iv_owner.
    cs_result-owner_session = iv_owner_session.
    cs_result-tab_session_id = iv_tab_session_id.
    IF iv_object_uuid IS NOT INITIAL.
      cs_result-object_uuid = iv_object_uuid.
    ENDIF.
    IF iv_lock_expires IS NOT INITIAL.
      cs_result-lock_expires = iv_lock_expires.
      cs_result-lock_expires_at = iv_lock_expires.
    ENDIF.
    IF iv_server_now IS NOT INITIAL.
      cs_result-server_now = iv_server_now.
    ENDIF.
    cs_result-lock_refreshed = iv_lock_refreshed.
    cs_result-owner_session_match = iv_owner_session_match.
  ENDMETHOD.

  METHOD fill_lock_heartbeat_result.
    cs_result-ok = iv_ok.
    cs_result-success = iv_ok.
    cs_result-code = iv_code.
    cs_result-reason_code = iv_code.
    cs_result-action = zif_zodata_contract_constants=>c_action_heartbeat.
    IF iv_message IS NOT INITIAL.
      cs_result-message = iv_message.
    ENDIF.
    cs_result-owner = iv_owner.
    cs_result-owner_session = iv_owner_session.
    cs_result-tab_session_id = iv_tab_session_id.
    IF iv_object_uuid IS NOT INITIAL.
      cs_result-object_uuid = iv_object_uuid.
    ENDIF.
    IF iv_lock_expires IS NOT INITIAL.
      cs_result-lock_expires = iv_lock_expires.
      cs_result-lock_expires_at = iv_lock_expires.
    ENDIF.
    IF iv_server_now IS NOT INITIAL.
      cs_result-server_now = iv_server_now.
    ENDIF.
    cs_result-lock_refreshed = iv_lock_refreshed.
    cs_result-owner_session_match = iv_owner_session_match.
  ENDMETHOD.

  METHOD fill_lock_release_result.
    cs_result-ok = iv_ok.
    cs_result-success = iv_ok.
    cs_result-code = iv_code.
    cs_result-reason_code = iv_code.
    cs_result-action = zif_zodata_contract_constants=>c_action_released.
    IF iv_message IS NOT INITIAL.
      cs_result-message = iv_message.
    ENDIF.
    cs_result-owner = iv_owner.
    cs_result-owner_session = iv_owner_session.
    cs_result-tab_session_id = iv_tab_session_id.
    IF iv_object_uuid IS NOT INITIAL.
      cs_result-object_uuid = iv_object_uuid.
    ENDIF.
    IF iv_lock_expires IS NOT INITIAL.
      cs_result-lock_expires = iv_lock_expires.
      cs_result-lock_expires_at = iv_lock_expires.
    ENDIF.
    IF iv_server_now IS NOT INITIAL.
      cs_result-server_now = iv_server_now.
    ENDIF.
    cs_result-lock_refreshed = iv_lock_refreshed.
    cs_result-owner_session_match = iv_owner_session_match.
  ENDMETHOD.

  METHOD fill_save_response.
    cs_result-pcct_uuid = iv_pcct_uuid.
    cs_result-changed_on = iv_changed_on.
    cs_result-version_number = iv_version_number.
    cs_result-is_autosave = iv_is_autosave.
    cs_result-no_changes = iv_no_changes.
    cs_result-code = iv_code.
    cs_result-reason_code = iv_reason_code.
    cs_result-lock_refreshed = iv_lock_refreshed.
    IF iv_lock_expires IS NOT INITIAL.
      cs_result-lock_expires_at = iv_lock_expires.
    ENDIF.
    IF iv_server_now IS NOT INITIAL.
      cs_result-server_now = iv_server_now.
    ENDIF.
    IF iv_request_id IS NOT INITIAL.
      cs_result-request_id = iv_request_id.
    ENDIF.
  ENDMETHOD.

  METHOD fill_permission_result.
    DATA(lv_granted_operations) = ``.

    IF iv_can_create = abap_true.
      lv_granted_operations = zif_zodata_contract_constants=>c_op_create.
    ENDIF.
    IF iv_can_edit = abap_true.
      lv_granted_operations = condense(
        COND string(
          WHEN lv_granted_operations IS INITIAL
          THEN zif_zodata_contract_constants=>c_op_change
          ELSE lv_granted_operations && ',' && zif_zodata_contract_constants=>c_op_change ) ).
    ENDIF.
    IF iv_can_view = abap_true.
      lv_granted_operations = condense(
        COND string(
          WHEN lv_granted_operations IS INITIAL
          THEN zif_zodata_contract_constants=>c_op_view
          ELSE lv_granted_operations && ',' && zif_zodata_contract_constants=>c_op_view ) ).
    ENDIF.
    IF iv_can_delete = abap_true.
      lv_granted_operations = condense(
        COND string(
          WHEN lv_granted_operations IS INITIAL
          THEN zif_zodata_contract_constants=>c_op_delete
          ELSE lv_granted_operations && ',' && zif_zodata_contract_constants=>c_op_delete ) ).
    ENDIF.

    cs_result-rootkey = iv_root_key.
    cs_result-userid = iv_user_id.
    cs_result-authobject = zif_zodata_contract_constants=>c_auth_object_checklist.
    cs_result-createoperation = zif_zodata_contract_constants=>c_op_create.
    cs_result-viewoperation = zif_zodata_contract_constants=>c_op_view.
    cs_result-changeoperation = zif_zodata_contract_constants=>c_op_change.
    cs_result-deleteoperation = zif_zodata_contract_constants=>c_op_delete.
    cs_result-grantedoperations = lv_granted_operations.
    cs_result-cancreate = iv_can_create.
    cs_result-canview = iv_can_view.
    cs_result-canedit = iv_can_edit.
    cs_result-candelete = iv_can_delete.
    cs_result-reasoncode = iv_reason_code.
    cs_result-message = iv_message.
  ENDMETHOD.

  METHOD fill_current_user.
    cs_result-key = 'CURRENT'.
    cs_result-fullname = iv_full_name.
    cs_result-permissionscsv = iv_permissions_csv.
    cs_result-permissionrulesjson = iv_permission_rules.
    cs_result-canview = iv_can_view.
    cs_result-canedit = iv_can_edit.
    cs_result-candelete = iv_can_delete.
    cs_result-summarytext = iv_summary_text.
  ENDMETHOD.

  METHOD fill_runtime_settings.
    cs_result-key = 'GLOBAL'.
    cs_result-environment = iv_environment.
    cs_result-heartbeatms = iv_heartbeat_ms.
    cs_result-idlems = iv_idle_ms.
    cs_result-autosaveintervalms = iv_autosave_interval_ms.
    cs_result-lockrefreshcooldownms = iv_lock_refresh_ms.
    cs_result-analyticsrefreshms = iv_analytics_refresh_ms.
    cs_result-gcdintervalms = iv_gcd_interval_ms.
    cs_result-networkgracems = iv_network_grace_ms.
    cs_result-cachetolerancems = iv_cache_tolerance_ms.
  ENDMETHOD.

ENDCLASS.
