" Function group template for application-level lock service
" Expected productive split: Z_PCCT_LOCK_ACQUIRE / HEARTBEAT / RELEASE / CLEANUP

FUNCTION zodata_lock_control.
"----------------------------------------------------------------------
"*"Local interface:
"*  IMPORTING
"*     VALUE(IV_MODE) TYPE  C " A=acquire, H=heartbeat, R=release, T=touch
"*     VALUE(IV_BO_KEY) TYPE /BOBF/CONF_KEY
"*     VALUE(IV_OBJECT_ID) TYPE SYSUUID_X16
"*  EXCEPTIONS
"*      LOCK_ERROR
"*      UPDATE_ERROR
"----------------------------------------------------------------------

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

    WHEN 'R'. " release
      CALL FUNCTION 'DEQUEUE_EZODATA_LCK'
        EXPORTING
          mode_ztodata_lck = 'E'
          mandt            = sy-mandt
          bo_key           = iv_bo_key
          object_id        = iv_object_id.

    WHEN 'H' OR 'T'. " heartbeat / technical touch
      UPDATE ztodata_hdr
         SET last_touch_at = sy-datum && sy-uzeit
             last_touch_by = sy-uname
       WHERE bo_key        = iv_bo_key
         AND object_id     = iv_object_id.
      IF sy-subrc <> 0.
        RAISE update_error.
      ENDIF.

    WHEN OTHERS.
      RAISE update_error.
  ENDCASE.

ENDFUNCTION.
