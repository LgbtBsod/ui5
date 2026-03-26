INTERFACE zif_zodata_contract_constants PUBLIC.

  CONSTANTS c_auth_object_checklist TYPE xubname VALUE 'Z_UI5_CHKL'.

  CONSTANTS c_op_create TYPE c LENGTH 2 VALUE '01'.
  CONSTANTS c_op_change TYPE c LENGTH 2 VALUE '02'.
  CONSTANTS c_op_view TYPE c LENGTH 2 VALUE '03'.
  CONSTANTS c_op_delete TYPE c LENGTH 2 VALUE '06'.
  CONSTANTS c_edit_mode_create TYPE c LENGTH 1 VALUE 'C'.
  CONSTANTS c_edit_mode_update TYPE c LENGTH 1 VALUE 'U'.
  CONSTANTS c_edit_mode_delete TYPE c LENGTH 1 VALUE 'D'.

  CONSTANTS c_lock_ttl_seconds TYPE i VALUE 600.

  CONSTANTS c_action_acquired TYPE string VALUE 'ACQUIRED'.
  CONSTANTS c_action_heartbeat TYPE string VALUE 'HEARTBEAT'.
  CONSTANTS c_action_released TYPE string VALUE 'RELEASED'.
  CONSTANTS c_action_owned TYPE string VALUE 'OWNED'.
  CONSTANTS c_action_none TYPE string VALUE 'NONE'.

  CONSTANTS c_reason_saved TYPE string VALUE 'SAVED'.
  CONSTANTS c_reason_no_changes TYPE string VALUE 'NO_CHANGES'.
  CONSTANTS c_request_id_prefix_save TYPE string VALUE 'SAVE'.

ENDINTERFACE.
