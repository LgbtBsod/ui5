CLASS zcl_zodata_message_texts DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS c_key_lock_acquire_required TYPE string VALUE 'LOCK_ACQUIRE_REQUIRED'.
    CONSTANTS c_key_lock_heartbeat_required TYPE string VALUE 'LOCK_HEARTBEAT_REQUIRED'.
    CONSTANTS c_key_lock_release_required TYPE string VALUE 'LOCK_RELEASE_REQUIRED'.
    CONSTANTS c_key_lock_session_required TYPE string VALUE 'LOCK_SESSION_REQUIRED'.
    CONSTANTS c_key_release_save_failed_prefix TYPE string VALUE 'RELEASE_SAVE_FAILED_PREFIX'.
    CONSTANTS c_key_save_root_required TYPE string VALUE 'SAVE_ROOT_REQUIRED'.
    CONSTANTS c_key_save_session_required TYPE string VALUE 'SAVE_SESSION_REQUIRED'.
    CONSTANTS c_key_save_root_mode_invalid TYPE string VALUE 'SAVE_ROOT_MODE_INVALID'.
    CONSTANTS c_key_save_checks_mode_required TYPE string VALUE 'SAVE_CHECKS_MODE_REQUIRED'.
    CONSTANTS c_key_save_checks_mode_invalid TYPE string VALUE 'SAVE_CHECKS_MODE_INVALID'.
    CONSTANTS c_key_save_barriers_mode_required TYPE string VALUE 'SAVE_BARRIERS_MODE_REQUIRED'.
    CONSTANTS c_key_save_barriers_mode_invalid TYPE string VALUE 'SAVE_BARRIERS_MODE_INVALID'.
    CONSTANTS c_key_save_participants_mode_required TYPE string VALUE 'SAVE_PARTICIPANTS_MODE_REQUIRED'.
    CONSTANTS c_key_save_participants_mode_invalid TYPE string VALUE 'SAVE_PARTICIPANTS_MODE_INVALID'.
    CONSTANTS c_key_save_attachments_mode_required TYPE string VALUE 'SAVE_ATTACHMENTS_MODE_REQUIRED'.
    CONSTANTS c_key_save_attachments_mode_invalid TYPE string VALUE 'SAVE_ATTACHMENTS_MODE_INVALID'.
    CONSTANTS c_key_permission_authorized TYPE string VALUE 'PERMISSION_AUTHORIZED'.
    CONSTANTS c_key_permission_no_view TYPE string VALUE 'PERMISSION_NO_VIEW'.
    CONSTANTS c_key_permission_read_only TYPE string VALUE 'PERMISSION_READ_ONLY'.
    CONSTANTS c_key_permission_no_edit TYPE string VALUE 'PERMISSION_NO_EDIT'.
    CONSTANTS c_key_permission_no_delete TYPE string VALUE 'PERMISSION_NO_DELETE'.
    CONSTANTS c_key_permission_summary_prefix TYPE string VALUE 'PERMISSION_SUMMARY_PREFIX'.
    CONSTANTS c_key_crud_not_allowed TYPE string VALUE 'CRUD_NOT_ALLOWED'.
    CONSTANTS c_key_no_create_auth TYPE string VALUE 'NO_CREATE_AUTH'.
    CONSTANTS c_key_no_create_auth_copy TYPE string VALUE 'NO_CREATE_AUTH_COPY'.
    CONSTANTS c_key_no_edit_auth TYPE string VALUE 'NO_EDIT_AUTH'.
    CONSTANTS c_key_no_edit_auth_save TYPE string VALUE 'NO_EDIT_AUTH_SAVE'.
    CONSTANTS c_key_mpl_tree_read_failed_prefix TYPE string VALUE 'MPL_TREE_READ_FAILED_PREFIX'.

    CLASS-METHODS get_text
      IMPORTING
        iv_key TYPE string
      RETURNING
        VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_zodata_message_texts IMPLEMENTATION.
  METHOD get_text.
    CASE iv_key.
      WHEN c_key_lock_acquire_required.
        rv_text = 'db_key and session_guid are required for lock acquire'.
      WHEN c_key_lock_heartbeat_required.
        rv_text = 'db_key and session_guid are required for lock heartbeat'.
      WHEN c_key_lock_release_required.
        rv_text = 'db_key and session_guid are required for lock release'.
      WHEN c_key_lock_session_required.
        rv_text = 'Active session lock is required'.
      WHEN c_key_release_save_failed_prefix.
        rv_text = 'Save succeeded but lock release failed:'.
      WHEN c_key_save_root_required.
        rv_text = 'root.pcct_uuid is required'.
      WHEN c_key_save_session_required.
        rv_text = 'session_guid is required'.
      WHEN c_key_save_root_mode_invalid.
        rv_text = 'root.edit_mode must be C, U or D when provided'.
      WHEN c_key_save_checks_mode_required.
        rv_text = 'checks.edit_mode is required'.
      WHEN c_key_save_checks_mode_invalid.
        rv_text = 'checks.edit_mode must be C, U or D'.
      WHEN c_key_save_barriers_mode_required.
        rv_text = 'barriers.edit_mode is required'.
      WHEN c_key_save_barriers_mode_invalid.
        rv_text = 'barriers.edit_mode must be C, U or D'.
      WHEN c_key_save_participants_mode_required.
        rv_text = 'participants.edit_mode is required'.
      WHEN c_key_save_participants_mode_invalid.
        rv_text = 'participants.edit_mode must be C, U or D'.
      WHEN c_key_save_attachments_mode_required.
        rv_text = 'attachments.edit_mode is required'.
      WHEN c_key_save_attachments_mode_invalid.
        rv_text = 'attachments.edit_mode must be C, U or D'.
      WHEN c_key_permission_authorized.
        rv_text = 'Authorized'.
      WHEN c_key_permission_no_view.
        rv_text = 'No display authorization'.
      WHEN c_key_permission_read_only.
        rv_text = 'Read-only authorization'.
      WHEN c_key_permission_no_edit.
        rv_text = 'No edit authorization'.
      WHEN c_key_permission_no_delete.
        rv_text = 'No delete authorization'.
      WHEN c_key_permission_summary_prefix.
        rv_text = 'Granted operations:'.
      WHEN c_key_crud_not_allowed.
        rv_text = 'Aggregate writes use function imports CreateChecklist, SaveChanges, AutoSave or CopyChecklist.'.
      WHEN c_key_no_create_auth.
        rv_text = 'No create authorization'.
      WHEN c_key_no_create_auth_copy.
        rv_text = 'No create authorization for copy'.
      WHEN c_key_no_edit_auth.
        rv_text = 'No edit authorization'.
      WHEN c_key_no_edit_auth_save.
        rv_text = 'No edit authorization for save'.
      WHEN c_key_mpl_tree_read_failed_prefix.
        rv_text = 'MPL tree read failed for date'.
      WHEN OTHERS.
        rv_text = iv_key.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
