INTERFACE zif_zodata_message_texts PUBLIC.

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

ENDINTERFACE.
