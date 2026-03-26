CLASS zcl_zodata_frontend_context_svc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_contract TYPE REF TO zcl_zodata_contract_service OPTIONAL.

    METHODS build_permission_result
      IMPORTING
        iv_root_key    TYPE sysuuid_x16
        iv_bukrs       TYPE bukrs
      RETURNING
        VALUE(rs_result) TYPE zstr_pcct_permission_rs.

    METHODS build_current_user_result
      IMPORTING
        iv_permission_rules_json TYPE string
      RETURNING
        VALUE(rs_result) TYPE zstr_pcct_current_user_rs.

    METHODS build_runtime_settings_result
      IMPORTING
        iv_environment            TYPE string
        iv_heartbeat_ms           TYPE int4
        iv_idle_ms                TYPE int4
        iv_autosave_interval_ms   TYPE int4
        iv_lock_refresh_ms        TYPE int4
        iv_analytics_refresh_ms   TYPE int4
        iv_gcd_interval_ms        TYPE int4
        iv_network_grace_ms       TYPE int4
        iv_cache_tolerance_ms     TYPE int4
      RETURNING
        VALUE(rs_result) TYPE zstr_pcct_runtime_settings_rs.

  PRIVATE SECTION.
    DATA mo_contract TYPE REF TO zcl_zodata_contract_service.

    METHODS ensure_contract.
ENDCLASS.

CLASS zcl_zodata_frontend_context_svc IMPLEMENTATION.
  METHOD constructor.
    mo_contract = io_contract.
  ENDMETHOD.

  METHOD ensure_contract.
    IF mo_contract IS INITIAL.
      mo_contract = NEW zcl_zodata_contract_service( ).
    ENDIF.
  ENDMETHOD.

  METHOD build_permission_result.
    DATA lv_can_view TYPE abap_bool VALUE abap_true.
    DATA lv_can_edit TYPE abap_bool VALUE abap_true.
    DATA lv_can_delete TYPE abap_bool VALUE abap_true.
    DATA lv_can_create TYPE abap_bool VALUE abap_true.
    DATA lv_reason TYPE string VALUE zif_zodata_message_codes=>authorized.
    DATA lv_message TYPE string VALUE zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_permission_authorized ).

    ensure_contract( ).

    IF iv_bukrs IS NOT INITIAL.
      AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_view ID 'BUKRS' FIELD iv_bukrs.
      lv_can_view = xsdbool( sy-subrc = 0 ).
      AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_change ID 'BUKRS' FIELD iv_bukrs.
      lv_can_edit = xsdbool( sy-subrc = 0 ).
      AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_delete ID 'BUKRS' FIELD iv_bukrs.
      lv_can_delete = xsdbool( sy-subrc = 0 ).
      AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_create ID 'BUKRS' FIELD iv_bukrs.
      lv_can_create = xsdbool( sy-subrc = 0 ).
    ENDIF.

    IF lv_can_view = abap_false.
      lv_reason = zif_zodata_message_codes=>no_view_auth.
      lv_message = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_permission_no_view ).
    ELSEIF lv_can_edit = abap_false AND lv_can_delete = abap_false.
      lv_reason = zif_zodata_message_codes=>read_only_auth.
      lv_message = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_permission_read_only ).
    ELSEIF lv_can_edit = abap_false.
      lv_reason = zif_zodata_message_codes=>no_edit_auth.
      lv_message = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_permission_no_edit ).
    ELSEIF lv_can_delete = abap_false.
      lv_reason = zif_zodata_message_codes=>no_delete_auth.
      lv_message = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_permission_no_delete ).
    ENDIF.

    mo_contract->fill_permission_result(
      EXPORTING
        iv_root_key    = iv_root_key
        iv_user_id     = sy-uname
        iv_can_create  = lv_can_create
        iv_can_view    = lv_can_view
        iv_can_edit    = lv_can_edit
        iv_can_delete  = lv_can_delete
        iv_reason_code = lv_reason
        iv_message     = lv_message
      CHANGING
        cs_result      = rs_result ).
  ENDMETHOD.

  METHOD build_current_user_result.
    DATA lv_full_name TYPE string.
    DATA lv_summary TYPE string.
    DATA lv_rules TYPE string.
    DATA lv_csv TYPE string.
    DATA lv_can_view TYPE abap_bool VALUE abap_false.
    DATA lv_can_edit TYPE abap_bool VALUE abap_false.
    DATA lv_can_delete TYPE abap_bool VALUE abap_false.

    ensure_contract( ).

    lv_full_name = |{ sy-uname }|.
    AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_view ID 'BUKRS' DUMMY.
    lv_can_view = xsdbool( sy-subrc = 0 ).
    AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_change ID 'BUKRS' DUMMY.
    lv_can_edit = xsdbool( sy-subrc = 0 ).
    AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_delete ID 'BUKRS' DUMMY.
    lv_can_delete = xsdbool( sy-subrc = 0 ).

    IF lv_can_view = abap_true.
      lv_csv = zif_zodata_contract_constants=>c_op_view.
    ENDIF.
    IF lv_can_edit = abap_true.
      lv_csv = COND string( WHEN lv_csv IS INITIAL THEN zif_zodata_contract_constants=>c_op_change ELSE |{ lv_csv },{ zif_zodata_contract_constants=>c_op_change }| ).
    ENDIF.
    IF lv_can_delete = abap_true.
      lv_csv = COND string( WHEN lv_csv IS INITIAL THEN zif_zodata_contract_constants=>c_op_delete ELSE |{ lv_csv },{ zif_zodata_contract_constants=>c_op_delete }| ).
    ENDIF.

    lv_rules = iv_permission_rules_json.
    lv_summary = |{ zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_permission_summary_prefix ) } { lv_csv }|.

    mo_contract->fill_current_user(
      EXPORTING
        iv_full_name        = lv_full_name
        iv_permissions_csv  = lv_csv
        iv_permission_rules = lv_rules
        iv_can_view         = lv_can_view
        iv_can_edit         = lv_can_edit
        iv_can_delete       = lv_can_delete
        iv_summary_text     = lv_summary
      CHANGING
        cs_result           = rs_result ).
  ENDMETHOD.

  METHOD build_runtime_settings_result.
    ensure_contract( ).
    mo_contract->fill_runtime_settings(
      EXPORTING
        iv_environment          = iv_environment
        iv_heartbeat_ms         = iv_heartbeat_ms
        iv_idle_ms              = iv_idle_ms
        iv_autosave_interval_ms = iv_autosave_interval_ms
        iv_lock_refresh_ms      = iv_lock_refresh_ms
        iv_analytics_refresh_ms = iv_analytics_refresh_ms
        iv_gcd_interval_ms      = iv_gcd_interval_ms
        iv_network_grace_ms     = iv_network_grace_ms
        iv_cache_tolerance_ms   = iv_cache_tolerance_ms
      CHANGING
        cs_result               = rs_result ).
  ENDMETHOD.
ENDCLASS.
