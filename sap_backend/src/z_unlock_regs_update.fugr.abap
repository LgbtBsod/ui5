FUNCTION z_unlock_regs_update.
"----------------------------------------------------------------------
"*"Local interface:
"*  IMPORTING
"*     VALUE(IV_GUID) TYPE  SYSUUID_X16
"*     VALUE(IV_USER) TYPE  SYUNAME
"----------------------------------------------------------------------

  DELETE FROM zlock_regs
    WHERE object_guid = @iv_guid
      AND uname       = @iv_user.

ENDFUNCTION.
