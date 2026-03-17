CLASS zcl_zodata_lock_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_zodata_lock_manager.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_contract TYPE REF TO zcl_zodata_contract_service.

    METHODS ensure_contract.

    METHODS call_lock_fm
      IMPORTING
        !iv_mode TYPE c
        !is_key  TYPE zif_zodata_lock_manager=>ty_key
        !iv_session_guid TYPE string OPTIONAL
        !iv_owner        TYPE syuname OPTIONAL
        !iv_tab_session_id TYPE string OPTIONAL
      CHANGING
        !cs_result TYPE any OPTIONAL
      RAISING
        zcx_zodata_error.
ENDCLASS.

CLASS zcl_zodata_lock_manager IMPLEMENTATION.

  METHOD ensure_contract.
    IF mo_contract IS INITIAL.
      mo_contract = NEW zcl_zodata_contract_service( ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~acquire.
    call_lock_fm(
      EXPORTING
        iv_mode           = 'A'
        is_key            = is_key
        iv_session_guid   = is_owner-session_guid
        iv_owner          = is_owner-uname
        iv_tab_session_id = is_owner-tab_session_id
      CHANGING
        cs_result         = cs_result ).
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~heartbeat.
    call_lock_fm(
      EXPORTING
        iv_mode         = 'H'
        is_key          = is_key
        iv_session_guid = iv_session_guid
      CHANGING
        cs_result       = cs_result ).
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~release.
    call_lock_fm(
      EXPORTING
        iv_mode         = 'R'
        is_key          = is_key
        iv_session_guid = iv_session_guid
      CHANGING
        cs_result       = cs_result ).
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~status.
    call_lock_fm(
      EXPORTING
        iv_mode         = 'S'
        is_key          = is_key
        iv_session_guid = iv_session_guid
      CHANGING
        cs_result       = cs_result ).
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~ensure_session_lock.
    DATA ls_dummy TYPE string.
    call_lock_fm(
      EXPORTING
        iv_mode         = 'V'
        is_key          = is_key
        iv_session_guid = iv_session_guid ).
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~lock.
    call_lock_fm(
      iv_mode = 'A'
      is_key  = is_key ).
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~unlock.
    call_lock_fm(
      iv_mode = 'R'
      is_key  = is_key ).
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~update_last_touch.
    call_lock_fm(
      iv_mode = 'T'
      is_key  = is_key ).
  ENDMETHOD.

  METHOD call_lock_fm.
    DATA lv_now TYPE timestampl.
    DATA lv_expires TYPE timestampl.

    ensure_contract( ).

    CALL FUNCTION 'ZODATA_LOCK_CONTROL'
      EXPORTING
        iv_mode           = iv_mode
        iv_bo_key         = is_key-bo_key
        iv_object_id      = is_key-object_id
        iv_session_guid   = iv_session_guid
        iv_owner          = iv_owner
        iv_tab_session_id = iv_tab_session_id
      EXCEPTIONS
        lock_error   = 1
        update_error = 2
        OTHERS       = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_zodata_error
        EXPORTING
          iv_msg = |Lock manager failed. MODE={ iv_mode } BO_KEY={ is_key-bo_key } SUBRC={ sy-subrc }|.
    ENDIF.

    IF cs_result IS SUPPLIED.
      GET TIME STAMP FIELD lv_now.
      lv_expires = cl_abap_tstmp=>add(
        tstmp = lv_now
        secs  = zcl_zodata_contract_constants=>c_lock_ttl_seconds ).

      DATA(lv_action) = SWITCH string(
        iv_mode
        WHEN 'A' THEN zcl_zodata_contract_constants=>c_action_acquired
        WHEN 'H' THEN zcl_zodata_contract_constants=>c_action_heartbeat
        WHEN 'R' THEN zcl_zodata_contract_constants=>c_action_released
        WHEN 'S' THEN zcl_zodata_contract_constants=>c_action_owned
        ELSE zcl_zodata_contract_constants=>c_action_none ).

      mo_contract->fill_lock_result(
        EXPORTING
          iv_ok                  = abap_true
          iv_code                = zcl_zodata_contract_constants=>c_code_lock_ok
          iv_action              = lv_action
          iv_owner               = iv_owner
          iv_owner_session       = iv_session_guid
          iv_tab_session_id      = iv_tab_session_id
          iv_object_uuid         = is_key-object_id
          iv_lock_expires        = lv_expires
          iv_server_now          = lv_now
          iv_lock_refreshed      = xsdbool( iv_mode = 'H' OR iv_mode = 'A' )
          iv_owner_session_match = xsdbool( iv_session_guid IS NOT INITIAL )
        CHANGING
          cs_result              = cs_result ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
