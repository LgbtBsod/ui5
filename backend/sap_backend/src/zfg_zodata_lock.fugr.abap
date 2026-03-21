" Canonical lock function group for Gateway runtime.
" Productive target contract:
" - TTL = 600 sec
" - validity is determined by technical last_refresh_at timestamp
" - acquire/heartbeat/status/save/autosave validate active ownership directly
" - cleanup is retention support only and must not define runtime truth
" - session_guid is mandatory for heartbeat/status/save/autosave semantics

FUNCTION zodata_lock_control.
"----------------------------------------------------------------------
"*"Local interface:
"*  IMPORTING
"*     VALUE(IV_MODE) TYPE  C " A=acquire, H=heartbeat, R=release, S=status, V=validate, T=touch
"*     VALUE(IV_BO_KEY) TYPE /BOBF/CONF_KEY
"*     VALUE(IV_OBJECT_ID) TYPE SYSUUID_X16
"*     VALUE(IV_SESSION_GUID) TYPE STRING OPTIONAL
"*     VALUE(IV_OWNER) TYPE SYUNAME OPTIONAL
"*     VALUE(IV_TAB_SESSION_ID) TYPE STRING OPTIONAL
"*  EXCEPTIONS
"*      LOCK_ERROR
"*      UPDATE_ERROR
"----------------------------------------------------------------------

  DATA lv_now TYPE timestampl.

  GET TIME STAMP FIELD lv_now.

  IF iv_bo_key IS INITIAL OR iv_object_id IS INITIAL.
    RAISE update_error.
  ENDIF.

  CASE iv_mode.
    WHEN 'A'. " acquire
      CALL FUNCTION 'ENQUEUE_EZODATA_LCK'
        EXPORTING
          mode_ztodata_lck = 'E'
          mandt            = sy-mandt
          bo_key           = iv_bo_key
          object_id        = iv_object_id
        EXCEPTIONS
          foreign_lock     = 1
          system_failure   = 2
          OTHERS           = 3.
      IF sy-subrc <> 0.
        RAISE lock_error.
      ENDIF.

      " Productive persistence truth:
      " - one row per BO/object
      " - physical column last_touch_at acts as canonical last_refresh_at timestamp
      " - owner/session fields define ownership
      UPDATE ztodata_hdr
         SET lock_owner      = iv_owner
             lock_session    = iv_session_guid
             tab_session_id  = iv_tab_session_id
             last_touch_at   = lv_now
             last_touch_by   = sy-uname
             lock_expires_at = cl_abap_tstmp=>add(
                                 tstmp = lv_now
                                 secs  = zif_zodata_contract_constants=>c_lock_ttl_seconds )
       WHERE bo_key          = iv_bo_key
         AND object_id       = iv_object_id.

      IF sy-subrc <> 0.
        RAISE update_error.
      ENDIF.

    WHEN 'R'. " release
      IF iv_session_guid IS INITIAL.
        RAISE update_error.
      ENDIF.

      SELECT SINGLE lock_session
        FROM ztodata_hdr
        WHERE bo_key    = @iv_bo_key
          AND object_id = @iv_object_id
        INTO @DATA(lv_release_session).

      IF sy-subrc <> 0 OR lv_release_session IS INITIAL OR lv_release_session <> iv_session_guid.
        RAISE lock_error.
      ENDIF.

      CALL FUNCTION 'DEQUEUE_EZODATA_LCK'
        EXPORTING
          mode_ztodata_lck = 'E'
          mandt            = sy-mandt
          bo_key           = iv_bo_key
          object_id        = iv_object_id.

      UPDATE ztodata_hdr
         SET lock_owner      = ''
             lock_session    = ''
             tab_session_id  = ''
             lock_expires_at = '00000000000000'
        WHERE bo_key          = iv_bo_key
          AND object_id       = iv_object_id.

      IF sy-subrc <> 0.
        RAISE update_error.
      ENDIF.

    WHEN 'H' OR 'T'. " heartbeat / technical touch
      IF iv_mode = 'H' AND iv_session_guid IS INITIAL.
        RAISE update_error.
      ENDIF.

      UPDATE ztodata_hdr
         SET last_touch_at = sy-datum && sy-uzeit
             last_touch_by = sy-uname
             lock_expires_at = cl_abap_tstmp=>add(
                                 tstmp = lv_now
                                 secs  = zif_zodata_contract_constants=>c_lock_ttl_seconds )
       WHERE bo_key        = iv_bo_key
         AND object_id     = iv_object_id
         AND ( lock_session = iv_session_guid OR iv_mode = 'T' ).
      IF sy-subrc <> 0.
        RAISE update_error.
      ENDIF.

    WHEN 'S' OR 'V'. " status / validate
      " Canonical runtime truth must be checked against:
      " 1. lock row exists
      " 2. physical last_touch_at/lock_expires_at represent the last_refresh_at truth
      " 3. session_guid matches active owner for validate
      SELECT SINGLE last_touch_at
                    lock_expires_at
                    lock_session
        FROM ztodata_hdr
        WHERE bo_key    = @iv_bo_key
          AND object_id = @iv_object_id
        INTO @DATA(ls_lock_state).

      IF sy-subrc <> 0.
        RAISE lock_error.
      ENDIF.

      IF ls_lock_state-lock_expires_at IS INITIAL OR ls_lock_state-lock_expires_at <= lv_now.
        RAISE lock_error.
      ENDIF.

      IF iv_mode = 'V' AND ls_lock_state-lock_session <> iv_session_guid.
        RAISE lock_error.
      ENDIF.

    WHEN OTHERS.
      RAISE update_error.
  ENDCASE.

ENDFUNCTION.
