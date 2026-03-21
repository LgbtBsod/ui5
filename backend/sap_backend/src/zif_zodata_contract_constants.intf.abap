INTERFACE zif_zodata_contract_constants PUBLIC.

  CONSTANTS c_auth_object_checklist TYPE xubname VALUE 'Z_UI5_CHKL'.

  CONSTANTS c_op_create TYPE c LENGTH 2 VALUE '01'.
  CONSTANTS c_op_change TYPE c LENGTH 2 VALUE '02'.
  CONSTANTS c_op_view TYPE c LENGTH 2 VALUE '03'.
  CONSTANTS c_op_delete TYPE c LENGTH 2 VALUE '06'.

  CONSTANTS c_lock_ttl_seconds TYPE i VALUE 600.

  CONSTANTS c_code_lock_ok TYPE string VALUE 'LOCK_OK'.
  CONSTANTS c_code_validation_error TYPE string VALUE 'VALIDATION_ERROR'.
  CONSTANTS c_code_technical_error TYPE string VALUE 'TECHNICAL_ERROR'.
  CONSTANTS c_code_lock_missing TYPE string VALUE 'LOCK_MISSING'.
  CONSTANTS c_code_lock_expired TYPE string VALUE 'LOCK_EXPIRED'.
  CONSTANTS c_code_lock_stolen TYPE string VALUE 'LOCK_STOLEN'.
  CONSTANTS c_code_lock_not_owned_by_session TYPE string VALUE 'LOCK_NOT_OWNED_BY_SESSION'.
  CONSTANTS c_code_permission_denied TYPE string VALUE 'PERMISSION_DENIED'.
  CONSTANTS c_code_authorized TYPE string VALUE 'AUTHORIZED'.
  CONSTANTS c_code_read_only_auth TYPE string VALUE 'READ_ONLY_AUTH'.
  CONSTANTS c_code_no_view_auth TYPE string VALUE 'NO_VIEW_AUTH'.
  CONSTANTS c_code_no_edit_auth TYPE string VALUE 'NO_EDIT_AUTH'.
  CONSTANTS c_code_no_delete_auth TYPE string VALUE 'NO_DELETE_AUTH'.
  CONSTANTS c_code_no_create_auth TYPE string VALUE 'NO_CREATE_AUTH'.
  CONSTANTS c_code_release_save_failed TYPE string VALUE 'RELEASE_SAVE_FAILED'.

  CONSTANTS c_action_acquired TYPE string VALUE 'ACQUIRED'.
  CONSTANTS c_action_heartbeat TYPE string VALUE 'HEARTBEAT'.
  CONSTANTS c_action_released TYPE string VALUE 'RELEASED'.
  CONSTANTS c_action_owned TYPE string VALUE 'OWNED'.
  CONSTANTS c_action_none TYPE string VALUE 'NONE'.

  CONSTANTS c_reason_saved TYPE string VALUE 'SAVED'.
  CONSTANTS c_reason_no_changes TYPE string VALUE 'NO_CHANGES'.
  CONSTANTS c_request_id_prefix_save TYPE string VALUE 'SAVE'.

  CONSTANTS c_msg_lock_acquire_required TYPE string VALUE 'object_uuid and session_guid are required for lock acquire'.
  CONSTANTS c_msg_lock_heartbeat_required TYPE string VALUE 'object_uuid and session_guid are required for lock heartbeat'.
  CONSTANTS c_msg_lock_release_required TYPE string VALUE 'object_uuid and session_guid are required for lock release'.
  CONSTANTS c_msg_lock_session_required TYPE string VALUE 'Active session lock is required'.
  CONSTANTS c_msg_release_save_failed_prefix TYPE string VALUE 'Save succeeded but lock release failed:'.
  CONSTANTS c_msg_save_root_required TYPE string VALUE 'root.pcct_uuid is required'.
  CONSTANTS c_msg_save_session_required TYPE string VALUE 'session_guid is required'.
  CONSTANTS c_msg_save_root_mode_invalid TYPE string VALUE 'root.edit_mode must be C, U or D when provided'.
  CONSTANTS c_msg_save_checks_mode_required TYPE string VALUE 'checks.edit_mode is required'.
  CONSTANTS c_msg_save_checks_mode_invalid TYPE string VALUE 'checks.edit_mode must be C, U or D'.
  CONSTANTS c_msg_save_barriers_mode_required TYPE string VALUE 'barriers.edit_mode is required'.
  CONSTANTS c_msg_save_barriers_mode_invalid TYPE string VALUE 'barriers.edit_mode must be C, U or D'.
  CONSTANTS c_msg_save_participants_mode_required TYPE string VALUE 'participants.edit_mode is required'.
  CONSTANTS c_msg_save_participants_mode_invalid TYPE string VALUE 'participants.edit_mode must be C, U or D'.
  CONSTANTS c_msg_save_attachments_mode_required TYPE string VALUE 'attachments.edit_mode is required'.
  CONSTANTS c_msg_save_attachments_mode_invalid TYPE string VALUE 'attachments.edit_mode must be C, U or D'.
  CONSTANTS c_msg_permission_authorized TYPE string VALUE 'Authorized'.
  CONSTANTS c_msg_permission_no_view TYPE string VALUE 'No display authorization'.
  CONSTANTS c_msg_permission_read_only TYPE string VALUE 'Read-only authorization'.
  CONSTANTS c_msg_permission_no_edit TYPE string VALUE 'No edit authorization'.
  CONSTANTS c_msg_permission_no_delete TYPE string VALUE 'No delete authorization'.
  CONSTANTS c_msg_permission_summary_prefix TYPE string VALUE 'Granted operations:'.
  CONSTANTS c_msg_crud_not_allowed TYPE string VALUE 'Aggregate writes use function imports CreateChecklist, SaveChanges, AutoSave or CopyChecklist.'.
  CONSTANTS c_msg_no_create_auth TYPE string VALUE 'No create authorization'.
  CONSTANTS c_msg_no_create_auth_copy TYPE string VALUE 'No create authorization for copy'.
  CONSTANTS c_msg_no_edit_auth TYPE string VALUE 'No edit authorization'.
  CONSTANTS c_msg_no_edit_auth_save TYPE string VALUE 'No edit authorization for save'.
  CONSTANTS c_msg_mpl_tree_read_failed_prefix TYPE string VALUE 'MPL tree read failed for date'.
  CONSTANTS c_msg_analytics_triggered TYPE string VALUE 'TRIGGERED'.

ENDINTERFACE.
