CLASS zcl_lock_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_lock_manager.

    METHODS lock_object
      IMPORTING
        !iv_guid TYPE sysuuid_x16
        !iv_user TYPE syuname
      RAISING
        zcx_lock_error.

    METHODS unlock_on_commit
      IMPORTING
        !iv_guid TYPE sysuuid_x16
        !iv_user TYPE syuname.

    METHODS unlock_immediately
      IMPORTING
        !iv_guid TYPE sysuuid_x16
        !iv_user TYPE syuname.

  PRIVATE SECTION.
    CLASS-DATA go_instance TYPE REF TO zcl_lock_manager.

    METHODS get_timeout_sec
      RETURNING
        VALUE(rv_sec) TYPE i.

    METHODS get_user_info
      IMPORTING
        !iv_user TYPE syuname
      EXPORTING
        !ev_name TYPE string
        !ev_pernr TYPE p_pernr.
ENDCLASS.

CLASS zcl_lock_manager IMPLEMENTATION.

  METHOD get_instance.
    go_instance = COND #( WHEN go_instance IS BOUND THEN go_instance ELSE NEW #( ) ).
    ro_obj = go_instance.
  ENDMETHOD.

  METHOD get_timeout_sec.
    SELECT SINGLE param_value
      FROM zlock_config
      INTO @DATA(lv_val)
      WHERE param_name = 'LOCK_TIMEOUT'.

    rv_sec = COND #( WHEN sy-subrc = 0 THEN lv_val ELSE 600 ).
  ENDMETHOD.

  METHOD get_user_info.
    SELECT SINGLE adrp~name_text, pa0001~pernr
      FROM usr21
      LEFT JOIN adrp ON adrp~persnumber = usr21~persnumber
      LEFT JOIN pa0001 ON pa0001~uname = usr21~bname
      INTO (@ev_name, @ev_pernr)
      WHERE usr21~bname = @iv_user.

    IF sy-subrc <> 0 OR ev_name IS INITIAL.
      ev_name = iv_user.
    ENDIF.
  ENDMETHOD.

  METHOD lock_object.
    DATA: ls_lock TYPE zlock_regs,
          lv_now  TYPE timestampl.

    GET TIME STAMP FIELD lv_now.

    SELECT SINGLE *
      FROM zlock_regs
      INTO @ls_lock
      WHERE object_guid = @iv_guid.

    IF sy-subrc = 0.
      DATA(lv_diff) = cl_abap_tstmp=>subtract( tstmp1 = lv_now tstmp2 = ls_lock-timestamp ).
      IF ls_lock-uname <> iv_user AND lv_diff < get_timeout_sec( ).
        get_user_info(
          EXPORTING iv_user = ls_lock-uname
          IMPORTING ev_name = DATA(lv_name)
                    ev_pernr = DATA(lv_pernr) ).

        RAISE EXCEPTION TYPE zcx_lock_error
          EXPORTING
            textid        = VALUE #( msgid = 'ZLOCK_MSG' msgno = '001' attr1 = 'USER_FULLNAME' attr2 = 'PERNR' )
            user_fullname = lv_name
            pernr         = lv_pernr.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'Z_LOCK_REGS_ASYNC'
      STARTING NEW TASK 'ZLOCK'
      EXPORTING
        iv_guid = iv_guid
        iv_user = iv_user.
  ENDMETHOD.

  METHOD unlock_on_commit.
    CALL FUNCTION 'Z_UNLOCK_REGS_UPDATE' IN UPDATE TASK
      EXPORTING
        iv_guid = iv_guid
        iv_user = iv_user.
  ENDMETHOD.

  METHOD unlock_immediately.
    DELETE FROM zlock_regs
      WHERE object_guid = @iv_guid
        AND uname       = @iv_user.
    COMMIT WORK.
  ENDMETHOD.

ENDCLASS.
