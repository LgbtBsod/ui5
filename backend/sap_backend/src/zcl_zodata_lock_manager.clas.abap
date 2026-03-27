CLASS zcl_zodata_lock_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_zodata_lock_manager.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_contract TYPE REF TO zcl_zodata_contract_service.

    TYPES: BEGIN OF ty_lock_snapshot,
             lock_owner      TYPE string,
             lock_session    TYPE string,
             tab_session_id  TYPE string,
             lock_expires_at TYPE timestampl,
           END OF ty_lock_snapshot.

    METHODS ensure_contract.
    METHODS read_lock_snapshot
      IMPORTING
        !is_key TYPE zif_zodata_lock_manager=>ty_key
      RETURNING
        VALUE(rs_snapshot) TYPE ty_lock_snapshot.
    METHODS resolve_status_code
      IMPORTING
        !is_snapshot     TYPE ty_lock_snapshot
        !iv_session_guid TYPE string OPTIONAL
        !iv_now          TYPE timestampl
      RETURNING
        VALUE(rv_code)   TYPE string.
    METHODS resolve_status_action
      IMPORTING
        !iv_code TYPE string
      RETURNING
        VALUE(rv_action) TYPE string.
    METHODS fill_status_result
      IMPORTING
        !is_key          TYPE zif_zodata_lock_manager=>ty_key
        !is_snapshot     TYPE ty_lock_snapshot
        !iv_session_guid TYPE string OPTIONAL
        !iv_now          TYPE timestampl
      CHANGING
        !cs_result       TYPE zstr_pcct_lock_acquire_rs.
    METHODS resolve_lock_failure_code
      IMPORTING
        !iv_mode         TYPE c
        !is_key          TYPE zif_zodata_lock_manager=>ty_key
        !iv_session_guid TYPE string OPTIONAL
      RETURNING
        VALUE(rv_code)   TYPE string.

    METHODS call_lock_fm
      IMPORTING
        !iv_mode TYPE c
        !is_key  TYPE zif_zodata_lock_manager=>ty_key
        !iv_session_guid TYPE string OPTIONAL
        !iv_owner        TYPE syuname OPTIONAL
        !iv_tab_session_id TYPE string OPTIONAL
        !iv_force_takeover TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_zodata_error.
ENDCLASS.

CLASS zcl_zodata_lock_manager IMPLEMENTATION.

  METHOD ensure_contract.
    IF mo_contract IS INITIAL.
      mo_contract = NEW zcl_zodata_contract_service( ).
    ENDIF.
  ENDMETHOD.

  METHOD read_lock_snapshot.
    SELECT SINGLE lock_owner lock_session tab_session_id lock_expires_at
      FROM ztodata_hdr
      INTO CORRESPONDING FIELDS OF @rs_snapshot
      WHERE bo_key = @is_key-bo_key
        AND object_id = @is_key-object_id.
  ENDMETHOD.

  METHOD resolve_status_code.
    IF is_snapshot-lock_session IS INITIAL.
      rv_code = zif_zodata_message_codes=>lock_missing.
      RETURN.
    ENDIF.

    IF is_snapshot-lock_expires_at IS INITIAL OR is_snapshot-lock_expires_at <= iv_now.
      rv_code = zif_zodata_message_codes=>lock_expired.
      RETURN.
    ENDIF.

    IF iv_session_guid IS INITIAL OR is_snapshot-lock_session = iv_session_guid.
      rv_code = zif_zodata_message_codes=>lock_ok.
      RETURN.
    ENDIF.

    rv_code = zif_zodata_message_codes=>lock_not_owned_by_session.
  ENDMETHOD.

  METHOD resolve_status_action.
    rv_action = COND string(
      WHEN iv_code = zif_zodata_message_codes=>lock_ok THEN 'OWNED'
      WHEN iv_code = zif_zodata_message_codes=>lock_expired THEN 'EXPIRED'
      WHEN iv_code = zif_zodata_message_codes=>lock_missing THEN 'MISSING'
      ELSE 'FAILED' ).
  ENDMETHOD.

  METHOD fill_status_result.
    DATA(lv_code) = resolve_status_code(
      is_snapshot     = is_snapshot
      iv_session_guid = iv_session_guid
      iv_now          = iv_now ).

    mo_contract->fill_lock_acquire_result(
      EXPORTING
        iv_ok                  = xsdbool( lv_code = zif_zodata_message_codes=>lock_ok )
        iv_code                = lv_code
        iv_owner               = is_snapshot-lock_owner
        iv_owner_session       = is_snapshot-lock_session
        iv_tab_session_id      = is_snapshot-tab_session_id
        iv_db_key              = is_key-object_id
        iv_lock_expires        = is_snapshot-lock_expires_at
        iv_server_now          = iv_now
        iv_lock_refreshed      = abap_false
        iv_owner_session_match = xsdbool( is_snapshot-lock_session = iv_session_guid AND iv_session_guid IS NOT INITIAL )
      CHANGING
        cs_result              = cs_result ).

    ASSIGN COMPONENT 'SESSION_GUID' OF STRUCTURE cs_result TO FIELD-SYMBOL(<lv_session_guid>).
    IF sy-subrc = 0.
      <lv_session_guid> = iv_session_guid.
    ENDIF.
    ASSIGN COMPONENT 'STATUS' OF STRUCTURE cs_result TO FIELD-SYMBOL(<lv_status>).
    IF sy-subrc = 0.
      <lv_status> = lv_code.
    ENDIF.
    ASSIGN COMPONENT 'ACTION' OF STRUCTURE cs_result TO FIELD-SYMBOL(<lv_action>).
    IF sy-subrc = 0.
      <lv_action> = resolve_status_action( lv_code ).
    ENDIF.
  ENDMETHOD.

  METHOD resolve_lock_failure_code.
    DATA(ls_snapshot) = read_lock_snapshot( is_key ).
    DATA lv_now TYPE timestampl.

    GET TIME STAMP FIELD lv_now.

    CASE iv_mode.
      WHEN 'S' OR 'V'.
        rv_code = resolve_status_code(
          is_snapshot     = ls_snapshot
          iv_session_guid = iv_session_guid
          iv_now          = lv_now ).
      WHEN 'H' OR 'R'.
        rv_code = COND string(
          WHEN resolve_status_code(
                 is_snapshot     = ls_snapshot
                 iv_session_guid = iv_session_guid
                 iv_now          = lv_now ) = zif_zodata_message_codes=>lock_expired
          THEN zif_zodata_message_codes=>lock_expired
          ELSE zif_zodata_message_codes=>lock_not_owned_by_session ).
      WHEN OTHERS.
        rv_code = zif_zodata_message_codes=>technical_error.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~acquire.
    call_lock_fm(
      EXPORTING
        iv_mode           = 'A'
        is_key            = is_key
        iv_session_guid   = is_owner-session_guid
        iv_owner          = is_owner-uname
        iv_tab_session_id = is_owner-tab_session_id
        iv_force_takeover = iv_force_takeover ).
    DATA lv_now TYPE timestampl.
    DATA lv_expires TYPE timestampl.
    GET TIME STAMP FIELD lv_now.
    lv_expires = cl_abap_tstmp=>add(
      tstmp = lv_now
      secs  = zif_zodata_contract_constants=>c_lock_ttl_seconds ).
    mo_contract->fill_lock_acquire_result(
      EXPORTING
        iv_ok                  = abap_true
        iv_code                = zif_zodata_message_codes=>lock_ok
        iv_owner               = is_owner-uname
        iv_owner_session       = is_owner-session_guid
        iv_tab_session_id      = is_owner-tab_session_id
        iv_db_key              = is_key-object_id
        iv_lock_expires        = lv_expires
        iv_server_now          = lv_now
        iv_lock_refreshed      = abap_true
        iv_owner_session_match = xsdbool( is_owner-session_guid IS NOT INITIAL )
      CHANGING
        cs_result              = cs_result ).
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~heartbeat.
    call_lock_fm(
      EXPORTING
        iv_mode         = 'H'
        is_key          = is_key
        iv_session_guid = iv_session_guid ).
    DATA lv_now TYPE timestampl.
    DATA lv_expires TYPE timestampl.
    GET TIME STAMP FIELD lv_now.
    lv_expires = cl_abap_tstmp=>add(
      tstmp = lv_now
      secs  = zif_zodata_contract_constants=>c_lock_ttl_seconds ).
    mo_contract->fill_lock_heartbeat_result(
      EXPORTING
        iv_ok                  = abap_true
        iv_code                = zif_zodata_message_codes=>lock_ok
        iv_owner_session       = iv_session_guid
        iv_db_key              = is_key-object_id
        iv_lock_expires        = lv_expires
        iv_server_now          = lv_now
        iv_lock_refreshed      = abap_true
        iv_owner_session_match = xsdbool( iv_session_guid IS NOT INITIAL )
      CHANGING
        cs_result              = cs_result ).
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~release.
    call_lock_fm(
      EXPORTING
        iv_mode         = 'R'
        is_key          = is_key
        iv_session_guid = iv_session_guid ).
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~status.
    DATA ls_snapshot TYPE ty_lock_snapshot.
    DATA lv_now TYPE timestampl.

    GET TIME STAMP FIELD lv_now.

    ls_snapshot = read_lock_snapshot( is_key ).
    fill_status_result(
      EXPORTING
        is_key          = is_key
        is_snapshot     = ls_snapshot
        iv_session_guid = iv_session_guid
        iv_now          = lv_now
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
      iv_mode         = 'R'
      is_key          = is_key
      iv_session_guid = iv_session_guid ).
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~update_last_touch.
    call_lock_fm(
      iv_mode = 'T'
      is_key  = is_key ).
  ENDMETHOD.

  METHOD call_lock_fm.
    DATA lv_error_code TYPE string.
    DATA lv_error_text TYPE string.

    ensure_contract( ).

    CALL FUNCTION 'ZODATA_LOCK_CONTROL'
      EXPORTING
        iv_mode           = iv_mode
        iv_bo_key         = is_key-bo_key
        iv_object_id      = is_key-object_id
        iv_session_guid   = iv_session_guid
        iv_owner          = iv_owner
        iv_tab_session_id = iv_tab_session_id
        iv_force_takeover = iv_force_takeover
      EXCEPTIONS
        lock_error   = 1
        update_error = 2
        OTHERS       = 3.

    IF sy-subrc <> 0.
      lv_error_code = resolve_lock_failure_code(
        iv_mode         = iv_mode
        is_key          = is_key
        iv_session_guid = iv_session_guid ).
      lv_error_text = lv_error_code.
      RAISE EXCEPTION TYPE zcx_zodata_error
        EXPORTING
          iv_code = lv_error_code
          iv_msg  = lv_error_text.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
