CLASS zcl_zodata_contract_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS fill_lock_result
      IMPORTING
        iv_ok                  TYPE abap_bool
        iv_code                TYPE string
        iv_action              TYPE string
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
        cs_result              TYPE any.

    METHODS fill_save_response
      IMPORTING
        iv_pcct_uuid      TYPE sysuuid_x16
        iv_changed_on     TYPE timestampl
        iv_version_number TYPE int4
        iv_is_autosave    TYPE abap_bool
        iv_no_changes     TYPE abap_bool
        iv_reason_code    TYPE string DEFAULT 'SAVED'
        iv_lock_refreshed TYPE abap_bool DEFAULT abap_true
        iv_lock_expires   TYPE timestampl OPTIONAL
        iv_server_now     TYPE timestampl OPTIONAL
      CHANGING
        cs_result         TYPE any.

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
        cs_result      TYPE any.

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
        cs_result           TYPE any.

    METHODS fill_runtime_settings
      IMPORTING
        iv_environment TYPE string DEFAULT 'production'
      CHANGING
        cs_result      TYPE any.
ENDCLASS.

CLASS zcl_zodata_contract_service IMPLEMENTATION.

  METHOD fill_lock_result.
    FIELD-SYMBOLS:
      <lv_ok>                  TYPE any,
      <lv_success>             TYPE any,
      <lv_code>                TYPE any,
      <lv_reason_code>         TYPE any,
      <lv_action>              TYPE any,
      <lv_message>             TYPE any,
      <lv_owner>               TYPE any,
      <lv_owner_session>       TYPE any,
      <lv_tab_session_id>      TYPE any,
      <lv_object_uuid>         TYPE any,
      <lv_lock_expires>        TYPE any,
      <lv_lock_expires_at>     TYPE any,
      <lv_server_now>          TYPE any,
      <lv_lock_refreshed>      TYPE any,
      <lv_owner_session_match> TYPE any.

    ASSIGN COMPONENT 'OK' OF STRUCTURE cs_result TO <lv_ok>.
    IF <lv_ok> IS ASSIGNED.
      <lv_ok> = iv_ok.
    ENDIF.

    ASSIGN COMPONENT 'SUCCESS' OF STRUCTURE cs_result TO <lv_success>.
    IF <lv_success> IS ASSIGNED.
      <lv_success> = iv_ok.
    ENDIF.

    ASSIGN COMPONENT 'CODE' OF STRUCTURE cs_result TO <lv_code>.
    IF <lv_code> IS ASSIGNED.
      <lv_code> = iv_code.
    ENDIF.

    ASSIGN COMPONENT 'REASON_CODE' OF STRUCTURE cs_result TO <lv_reason_code>.
    IF <lv_reason_code> IS ASSIGNED.
      <lv_reason_code> = iv_code.
    ENDIF.

    ASSIGN COMPONENT 'ACTION' OF STRUCTURE cs_result TO <lv_action>.
    IF <lv_action> IS ASSIGNED.
      <lv_action> = iv_action.
    ENDIF.

    ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE cs_result TO <lv_message>.
    IF <lv_message> IS ASSIGNED AND iv_message IS NOT INITIAL.
      <lv_message> = iv_message.
    ENDIF.

    ASSIGN COMPONENT 'OWNER' OF STRUCTURE cs_result TO <lv_owner>.
    IF <lv_owner> IS ASSIGNED.
      <lv_owner> = iv_owner.
    ENDIF.

    ASSIGN COMPONENT 'OWNER_SESSION' OF STRUCTURE cs_result TO <lv_owner_session>.
    IF <lv_owner_session> IS ASSIGNED.
      <lv_owner_session> = iv_owner_session.
    ENDIF.

    ASSIGN COMPONENT 'TAB_SESSION_ID' OF STRUCTURE cs_result TO <lv_tab_session_id>.
    IF <lv_tab_session_id> IS ASSIGNED.
      <lv_tab_session_id> = iv_tab_session_id.
    ENDIF.

    ASSIGN COMPONENT 'OBJECT_UUID' OF STRUCTURE cs_result TO <lv_object_uuid>.
    IF <lv_object_uuid> IS ASSIGNED AND iv_object_uuid IS NOT INITIAL.
      <lv_object_uuid> = iv_object_uuid.
    ENDIF.

    ASSIGN COMPONENT 'LOCK_EXPIRES' OF STRUCTURE cs_result TO <lv_lock_expires>.
    IF <lv_lock_expires> IS ASSIGNED AND iv_lock_expires IS NOT INITIAL.
      <lv_lock_expires> = iv_lock_expires.
    ENDIF.

    ASSIGN COMPONENT 'LOCK_EXPIRES_AT' OF STRUCTURE cs_result TO <lv_lock_expires_at>.
    IF <lv_lock_expires_at> IS ASSIGNED AND iv_lock_expires IS NOT INITIAL.
      <lv_lock_expires_at> = iv_lock_expires.
    ENDIF.

    ASSIGN COMPONENT 'SERVER_NOW' OF STRUCTURE cs_result TO <lv_server_now>.
    IF <lv_server_now> IS ASSIGNED AND iv_server_now IS NOT INITIAL.
      <lv_server_now> = iv_server_now.
    ENDIF.

    ASSIGN COMPONENT 'LOCK_REFRESHED' OF STRUCTURE cs_result TO <lv_lock_refreshed>.
    IF <lv_lock_refreshed> IS ASSIGNED.
      <lv_lock_refreshed> = iv_lock_refreshed.
    ENDIF.

    ASSIGN COMPONENT 'OWNER_SESSION_MATCH' OF STRUCTURE cs_result TO <lv_owner_session_match>.
    IF <lv_owner_session_match> IS ASSIGNED.
      <lv_owner_session_match> = iv_owner_session_match.
    ENDIF.
  ENDMETHOD.

  METHOD fill_save_response.
    FIELD-SYMBOLS:
      <lv_pcct_uuid>      TYPE any,
      <lv_changed_on>     TYPE any,
      <lv_version_number> TYPE any,
      <lv_is_autosave>    TYPE any,
      <lv_no_changes>     TYPE any,
      <lv_reason_code>    TYPE any,
      <lv_lock_refreshed> TYPE any,
      <lv_lock_expires>   TYPE any,
      <lv_server_now>     TYPE any.

    ASSIGN COMPONENT 'PCCT_UUID' OF STRUCTURE cs_result TO <lv_pcct_uuid>.
    IF <lv_pcct_uuid> IS ASSIGNED.
      <lv_pcct_uuid> = iv_pcct_uuid.
    ENDIF.

    ASSIGN COMPONENT 'CHANGED_ON' OF STRUCTURE cs_result TO <lv_changed_on>.
    IF <lv_changed_on> IS ASSIGNED.
      <lv_changed_on> = iv_changed_on.
    ENDIF.

    ASSIGN COMPONENT 'VERSION_NUMBER' OF STRUCTURE cs_result TO <lv_version_number>.
    IF <lv_version_number> IS ASSIGNED.
      <lv_version_number> = iv_version_number.
    ENDIF.

    ASSIGN COMPONENT 'IS_AUTOSAVE' OF STRUCTURE cs_result TO <lv_is_autosave>.
    IF <lv_is_autosave> IS ASSIGNED.
      <lv_is_autosave> = iv_is_autosave.
    ENDIF.

    ASSIGN COMPONENT 'NO_CHANGES' OF STRUCTURE cs_result TO <lv_no_changes>.
    IF <lv_no_changes> IS ASSIGNED.
      <lv_no_changes> = iv_no_changes.
    ENDIF.

    ASSIGN COMPONENT 'REASON_CODE' OF STRUCTURE cs_result TO <lv_reason_code>.
    IF <lv_reason_code> IS ASSIGNED.
      <lv_reason_code> = iv_reason_code.
    ENDIF.

    ASSIGN COMPONENT 'LOCK_REFRESHED' OF STRUCTURE cs_result TO <lv_lock_refreshed>.
    IF <lv_lock_refreshed> IS ASSIGNED.
      <lv_lock_refreshed> = iv_lock_refreshed.
    ENDIF.

    ASSIGN COMPONENT 'LOCK_EXPIRES_AT' OF STRUCTURE cs_result TO <lv_lock_expires>.
    IF <lv_lock_expires> IS ASSIGNED AND iv_lock_expires IS NOT INITIAL.
      <lv_lock_expires> = iv_lock_expires.
    ENDIF.

    ASSIGN COMPONENT 'SERVER_NOW' OF STRUCTURE cs_result TO <lv_server_now>.
    IF <lv_server_now> IS ASSIGNED AND iv_server_now IS NOT INITIAL.
      <lv_server_now> = iv_server_now.
    ENDIF.
  ENDMETHOD.

  METHOD fill_permission_result.
    DATA(lv_granted_operations) = ``.
    FIELD-SYMBOLS:
      <lv_root_key>            TYPE any,
      <lv_user_id>             TYPE any,
      <lv_auth_object>         TYPE any,
      <lv_create_operation>    TYPE any,
      <lv_view_operation>      TYPE any,
      <lv_change_operation>    TYPE any,
      <lv_delete_operation>    TYPE any,
      <lv_granted_operations>  TYPE any,
      <lv_can_create>          TYPE any,
      <lv_can_view>            TYPE any,
      <lv_can_edit>            TYPE any,
      <lv_can_delete>          TYPE any,
      <lv_reason_code>         TYPE any,
      <lv_message>             TYPE any.

    IF iv_can_create = abap_true.
      lv_granted_operations = zcl_zodata_contract_constants=>c_op_create.
    ENDIF.
    IF iv_can_edit = abap_true.
      lv_granted_operations = condense(
        COND string(
          WHEN lv_granted_operations IS INITIAL
          THEN zcl_zodata_contract_constants=>c_op_change
          ELSE lv_granted_operations && ',' && zcl_zodata_contract_constants=>c_op_change ) ).
    ENDIF.
    IF iv_can_view = abap_true.
      lv_granted_operations = condense(
        COND string(
          WHEN lv_granted_operations IS INITIAL
          THEN zcl_zodata_contract_constants=>c_op_view
          ELSE lv_granted_operations && ',' && zcl_zodata_contract_constants=>c_op_view ) ).
    ENDIF.
    IF iv_can_delete = abap_true.
      lv_granted_operations = condense(
        COND string(
          WHEN lv_granted_operations IS INITIAL
          THEN zcl_zodata_contract_constants=>c_op_delete
          ELSE lv_granted_operations && ',' && zcl_zodata_contract_constants=>c_op_delete ) ).
    ENDIF.

    ASSIGN COMPONENT 'ROOTKEY' OF STRUCTURE cs_result TO <lv_root_key>.
    IF <lv_root_key> IS ASSIGNED.
      <lv_root_key> = iv_root_key.
    ELSE.
      ASSIGN COMPONENT 'ROOT_KEY' OF STRUCTURE cs_result TO <lv_root_key>.
      IF <lv_root_key> IS ASSIGNED.
        <lv_root_key> = iv_root_key.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT 'USERID' OF STRUCTURE cs_result TO <lv_user_id>.
    IF <lv_user_id> IS ASSIGNED.
      <lv_user_id> = iv_user_id.
    ELSE.
      ASSIGN COMPONENT 'USER_ID' OF STRUCTURE cs_result TO <lv_user_id>.
      IF <lv_user_id> IS ASSIGNED.
        <lv_user_id> = iv_user_id.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT 'AUTHOBJECT' OF STRUCTURE cs_result TO <lv_auth_object>.
    IF <lv_auth_object> IS ASSIGNED.
      <lv_auth_object> = zcl_zodata_contract_constants=>c_auth_object_checklist.
    ENDIF.

    ASSIGN COMPONENT 'CREATEOPERATION' OF STRUCTURE cs_result TO <lv_create_operation>.
    IF <lv_create_operation> IS ASSIGNED.
      <lv_create_operation> = zcl_zodata_contract_constants=>c_op_create.
    ENDIF.

    ASSIGN COMPONENT 'VIEWOPERATION' OF STRUCTURE cs_result TO <lv_view_operation>.
    IF <lv_view_operation> IS ASSIGNED.
      <lv_view_operation> = zcl_zodata_contract_constants=>c_op_view.
    ENDIF.

    ASSIGN COMPONENT 'CHANGEOPERATION' OF STRUCTURE cs_result TO <lv_change_operation>.
    IF <lv_change_operation> IS ASSIGNED.
      <lv_change_operation> = zcl_zodata_contract_constants=>c_op_change.
    ENDIF.

    ASSIGN COMPONENT 'DELETEOPERATION' OF STRUCTURE cs_result TO <lv_delete_operation>.
    IF <lv_delete_operation> IS ASSIGNED.
      <lv_delete_operation> = zcl_zodata_contract_constants=>c_op_delete.
    ENDIF.

    ASSIGN COMPONENT 'GRANTEDOPERATIONS' OF STRUCTURE cs_result TO <lv_granted_operations>.
    IF <lv_granted_operations> IS ASSIGNED.
      <lv_granted_operations> = lv_granted_operations.
    ENDIF.

    ASSIGN COMPONENT 'CANCREATE' OF STRUCTURE cs_result TO <lv_can_create>.
    IF <lv_can_create> IS ASSIGNED.
      <lv_can_create> = iv_can_create.
    ENDIF.

    ASSIGN COMPONENT 'CANVIEW' OF STRUCTURE cs_result TO <lv_can_view>.
    IF <lv_can_view> IS ASSIGNED.
      <lv_can_view> = iv_can_view.
    ENDIF.

    ASSIGN COMPONENT 'CANEDIT' OF STRUCTURE cs_result TO <lv_can_edit>.
    IF <lv_can_edit> IS ASSIGNED.
      <lv_can_edit> = iv_can_edit.
    ENDIF.

    ASSIGN COMPONENT 'CANDELETE' OF STRUCTURE cs_result TO <lv_can_delete>.
    IF <lv_can_delete> IS ASSIGNED.
      <lv_can_delete> = iv_can_delete.
    ENDIF.

    ASSIGN COMPONENT 'REASONCODE' OF STRUCTURE cs_result TO <lv_reason_code>.
    IF <lv_reason_code> IS ASSIGNED.
      <lv_reason_code> = iv_reason_code.
    ENDIF.

    ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE cs_result TO <lv_message>.
    IF <lv_message> IS ASSIGNED.
      <lv_message> = iv_message.
    ENDIF.
  ENDMETHOD.

  METHOD fill_current_user.
    FIELD-SYMBOLS:
      <lv_key>               TYPE any,
      <lv_full_name>         TYPE any,
      <lv_permissions_csv>   TYPE any,
      <lv_permission_rules>  TYPE any,
      <lv_can_view>          TYPE any,
      <lv_can_edit>          TYPE any,
      <lv_can_delete>        TYPE any,
      <lv_summary_text>      TYPE any.

    ASSIGN COMPONENT 'KEY' OF STRUCTURE cs_result TO <lv_key>.
    IF <lv_key> IS ASSIGNED.
      <lv_key> = 'CURRENT'.
    ENDIF.

    ASSIGN COMPONENT 'FULLNAME' OF STRUCTURE cs_result TO <lv_full_name>.
    IF <lv_full_name> IS ASSIGNED.
      <lv_full_name> = iv_full_name.
    ENDIF.

    ASSIGN COMPONENT 'PERMISSIONSCSV' OF STRUCTURE cs_result TO <lv_permissions_csv>.
    IF <lv_permissions_csv> IS ASSIGNED.
      <lv_permissions_csv> = iv_permissions_csv.
    ENDIF.

    ASSIGN COMPONENT 'PERMISSIONRULESJSON' OF STRUCTURE cs_result TO <lv_permission_rules>.
    IF <lv_permission_rules> IS ASSIGNED.
      <lv_permission_rules> = iv_permission_rules.
    ENDIF.

    ASSIGN COMPONENT 'CANVIEW' OF STRUCTURE cs_result TO <lv_can_view>.
    IF <lv_can_view> IS ASSIGNED.
      <lv_can_view> = iv_can_view.
    ENDIF.

    ASSIGN COMPONENT 'CANEDIT' OF STRUCTURE cs_result TO <lv_can_edit>.
    IF <lv_can_edit> IS ASSIGNED.
      <lv_can_edit> = iv_can_edit.
    ENDIF.

    ASSIGN COMPONENT 'CANDELETE' OF STRUCTURE cs_result TO <lv_can_delete>.
    IF <lv_can_delete> IS ASSIGNED.
      <lv_can_delete> = iv_can_delete.
    ENDIF.

    ASSIGN COMPONENT 'SUMMARYTEXT' OF STRUCTURE cs_result TO <lv_summary_text>.
    IF <lv_summary_text> IS ASSIGNED.
      <lv_summary_text> = iv_summary_text.
    ENDIF.
  ENDMETHOD.

  METHOD fill_runtime_settings.
    FIELD-SYMBOLS:
      <lv_key>                       TYPE any,
      <lv_environment>               TYPE any,
      <lv_heartbeat_ms>              TYPE any,
      <lv_idle_ms>                   TYPE any,
      <lv_autosave_ms>               TYPE any,
      <lv_lock_refresh_cooldown_ms>  TYPE any,
      <lv_analytics_refresh_ms>      TYPE any,
      <lv_gcd_interval_ms>           TYPE any,
      <lv_network_grace_ms>          TYPE any,
      <lv_cache_tolerance_ms>        TYPE any.

    ASSIGN COMPONENT 'KEY' OF STRUCTURE cs_result TO <lv_key>.
    IF <lv_key> IS ASSIGNED.
      <lv_key> = 'GLOBAL'.
    ENDIF.

    ASSIGN COMPONENT 'ENVIRONMENT' OF STRUCTURE cs_result TO <lv_environment>.
    IF <lv_environment> IS ASSIGNED.
      <lv_environment> = iv_environment.
    ENDIF.

    ASSIGN COMPONENT 'HEARTBEATMS' OF STRUCTURE cs_result TO <lv_heartbeat_ms>.
    IF <lv_heartbeat_ms> IS ASSIGNED.
      <lv_heartbeat_ms> = zcl_zodata_contract_constants=>c_heartbeat_ms.
    ENDIF.

    ASSIGN COMPONENT 'IDLEMS' OF STRUCTURE cs_result TO <lv_idle_ms>.
    IF <lv_idle_ms> IS ASSIGNED.
      <lv_idle_ms> = zcl_zodata_contract_constants=>c_idle_ms.
    ENDIF.

    ASSIGN COMPONENT 'AUTOSAVEINTERVALMS' OF STRUCTURE cs_result TO <lv_autosave_ms>.
    IF <lv_autosave_ms> IS ASSIGNED.
      <lv_autosave_ms> = zcl_zodata_contract_constants=>c_autosave_ms.
    ENDIF.

    ASSIGN COMPONENT 'LOCKREFRESHCOOLDOWNMS' OF STRUCTURE cs_result TO <lv_lock_refresh_cooldown_ms>.
    IF <lv_lock_refresh_cooldown_ms> IS ASSIGNED.
      <lv_lock_refresh_cooldown_ms> = zcl_zodata_contract_constants=>c_lock_refresh_cooldown_ms.
    ENDIF.

    ASSIGN COMPONENT 'ANALYTICSREFRESHMS' OF STRUCTURE cs_result TO <lv_analytics_refresh_ms>.
    IF <lv_analytics_refresh_ms> IS ASSIGNED.
      <lv_analytics_refresh_ms> = zcl_zodata_contract_constants=>c_analytics_refresh_ms.
    ENDIF.

    ASSIGN COMPONENT 'GCDINTERVALMS' OF STRUCTURE cs_result TO <lv_gcd_interval_ms>.
    IF <lv_gcd_interval_ms> IS ASSIGNED.
      <lv_gcd_interval_ms> = zcl_zodata_contract_constants=>c_gcd_interval_ms.
    ENDIF.

    ASSIGN COMPONENT 'NETWORKGRACEMS' OF STRUCTURE cs_result TO <lv_network_grace_ms>.
    IF <lv_network_grace_ms> IS ASSIGNED.
      <lv_network_grace_ms> = zcl_zodata_contract_constants=>c_network_grace_ms.
    ENDIF.

    ASSIGN COMPONENT 'CACHETOLERANCEMS' OF STRUCTURE cs_result TO <lv_cache_tolerance_ms>.
    IF <lv_cache_tolerance_ms> IS ASSIGNED.
      <lv_cache_tolerance_ms> = zcl_zodata_contract_constants=>c_cache_tolerance_ms.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
