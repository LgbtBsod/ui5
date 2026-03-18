FUNCTION z_lock_regs_async.
"----------------------------------------------------------------------
"*"Local interface:
"*  IMPORTING
"*     VALUE(IV_GUID) TYPE  SYSUUID_X16
"*     VALUE(IV_USER) TYPE  SYUNAME
"----------------------------------------------------------------------

  DATA ls_lock TYPE zlock_regs.
  GET TIME STAMP FIELD DATA(lv_now).

  SELECT SINGLE FOR UPDATE *
    FROM zlock_regs
    WHERE object_guid = @iv_guid
    INTO @ls_lock.

  IF sy-subrc = 0.
    DATA(lv_diff) = cl_abap_tstmp=>subtract( tstmp1 = lv_now tstmp2 = ls_lock-timestamp ).
    IF ls_lock-uname = iv_user OR lv_diff > 600.
      ls_lock-uname     = iv_user.
      ls_lock-timestamp = lv_now.
      MODIFY zlock_regs FROM ls_lock.
    ENDIF.
  ELSE.
    ls_lock = VALUE #( object_guid = iv_guid uname = iv_user timestamp = lv_now ).
    INSERT zlock_regs FROM ls_lock.
  ENDIF.

  COMMIT WORK.
ENDFUNCTION.
