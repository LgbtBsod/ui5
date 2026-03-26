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
        iv_object_uuid         = is_key-object_id
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
        iv_object_uuid         = is_key-object_id
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
    DATA ls_root TYPE zcl_zodata_read_service=>ty_root_row.
    DATA lv_now TYPE timestampl.
    GET TIME STAMP FIELD lv_now.
    SELECT SINGLE lock_owner lock_session tab_session_id lock_expires_at
      FROM ztodata_hdr
      INTO CORRESPONDING FIELDS OF @ls_root
      WHERE bo_key = @is_key-bo_key
        AND object_id = @is_key-object_id.
    IF sy-subrc <> 0 OR ls_root-lock_session IS INITIAL.
      mo_contract->fill_lock_acquire_result(
        EXPORTING
          iv_ok                  = abap_false
          iv_code                = zif_zodata_message_codes=>lock_missing
          iv_owner               = ls_root-lock_owner
          iv_owner_session       = ls_root-lock_session
          iv_tab_session_id      = ls_root-tab_session_id
          iv_object_uuid         = is_key-object_id
          iv_lock_expires        = ls_root-lock_expires_at
          iv_server_now          = lv_now
          iv_lock_refreshed      = abap_false
          iv_owner_session_match = abap_false
        CHANGING
          cs_result              = cs_result ).
      RETURN.
    ENDIF.
    IF ls_root-lock_expires_at IS INITIAL OR ls_root-lock_expires_at <= lv_now.
      mo_contract->fill_lock_acquire_result(
        EXPORTING
          iv_ok                  = abap_false
          iv_code                = zif_zodata_message_codes=>lock_expired
          iv_owner               = ls_root-lock_owner
          iv_owner_session       = ls_root-lock_session
          iv_tab_session_id      = ls_root-tab_session_id
          iv_object_uuid         = is_key-object_id
          iv_lock_expires        = ls_root-lock_expires_at
          iv_server_now          = lv_now
          iv_lock_refreshed      = abap_false
          iv_owner_session_match = xsdbool( ls_root-lock_session = iv_session_guid )
        CHANGING
          cs_result              = cs_result ).
      RETURN.
    ENDIF.
    mo_contract->fill_lock_acquire_result(
      EXPORTING
        iv_ok                  = xsdbool( ls_root-lock_session = iv_session_guid )
        iv_code                = COND string(
                                    WHEN ls_root-lock_session = iv_session_guid
                                    THEN zif_zodata_message_codes=>lock_ok
                                    ELSE zif_zodata_message_codes=>lock_not_owned_by_session )
        iv_owner               = ls_root-lock_owner
        iv_owner_session       = ls_root-lock_session
        iv_tab_session_id      = ls_root-tab_session_id
        iv_object_uuid         = is_key-object_id
        iv_lock_expires        = ls_root-lock_expires_at
        iv_server_now          = lv_now
        iv_lock_refreshed      = abap_false
        iv_owner_session_match = xsdbool( ls_root-lock_session = iv_session_guid )
      CHANGING
        cs_result              = cs_result ).
    ASSIGN COMPONENT 'SESSION_GUID' OF STRUCTURE cs_result TO FIELD-SYMBOL(<lv_session_guid>).
    IF sy-subrc = 0.
      <lv_session_guid> = iv_session_guid.
    ENDIF.
    ASSIGN COMPONENT 'STATUS' OF STRUCTURE cs_result TO FIELD-SYMBOL(<lv_status>).
    IF sy-subrc = 0.
      <lv_status> = COND string(
        WHEN ls_root-lock_session = iv_session_guid THEN zif_zodata_message_codes=>lock_ok
        WHEN ls_root-lock_expires_at IS INITIAL OR ls_root-lock_expires_at <= lv_now THEN zif_zodata_message_codes=>lock_expired
        ELSE zif_zodata_message_codes=>lock_not_owned_by_session ).
    ENDIF.
    ASSIGN COMPONENT 'ACTION' OF STRUCTURE cs_result TO FIELD-SYMBOL(<lv_action>).
    IF sy-subrc = 0.
      <lv_action> = COND string(
        WHEN ls_root-lock_session = iv_session_guid THEN 'OWNED'
        WHEN ls_root-lock_expires_at IS INITIAL OR ls_root-lock_expires_at <= lv_now THEN 'EXPIRED'
        ELSE 'FAILED' ).
    ENDIF.
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
      CASE iv_mode.
        WHEN 'H' OR 'V'.
          lv_error_code = zif_zodata_message_codes=>lock_not_owned_by_session.
          lv_error_text = zif_zodata_message_codes=>lock_not_owned_by_session.
        WHEN 'R'.
          lv_error_code = zif_zodata_message_codes=>lock_not_owned_by_session.
          lv_error_text = zif_zodata_message_codes=>lock_not_owned_by_session.
        WHEN 'S'.
          lv_error_code = zif_zodata_message_codes=>lock_missing.
          lv_error_text = zif_zodata_message_codes=>lock_missing.
        WHEN OTHERS.
          lv_error_code = zif_zodata_message_codes=>technical_error.
          lv_error_text = zif_zodata_message_codes=>technical_error.
      ENDCASE.
      RAISE EXCEPTION TYPE zcx_zodata_error
        EXPORTING
          iv_code = lv_error_code
          iv_msg  = lv_error_text.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
