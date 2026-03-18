CLASS zcl_zodata_contract_constants DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS c_auth_object_checklist TYPE string VALUE 'Z_UI5_CHKL'.

    CONSTANTS c_op_create TYPE string VALUE '01'.
    CONSTANTS c_op_change TYPE string VALUE '02'.
    CONSTANTS c_op_view   TYPE string VALUE '03'.
    CONSTANTS c_op_delete TYPE string VALUE '06'.

    CONSTANTS c_lock_ttl_seconds         TYPE i VALUE 600.
    CONSTANTS c_heartbeat_ms             TYPE i VALUE 270000.
    CONSTANTS c_idle_ms                  TYPE i VALUE 570000.
    CONSTANTS c_autosave_ms              TYPE i VALUE 150000.
    CONSTANTS c_lock_refresh_cooldown_ms TYPE i VALUE 150000.
    CONSTANTS c_analytics_refresh_ms     TYPE i VALUE 900000.
    CONSTANTS c_gcd_interval_ms          TYPE i VALUE 30000.
    CONSTANTS c_network_grace_ms         TYPE i VALUE 15000.
    CONSTANTS c_cache_tolerance_ms       TYPE i VALUE 5500.

    CONSTANTS c_code_lock_ok                   TYPE string VALUE 'LOCK_OK'.
    CONSTANTS c_code_lock_missing              TYPE string VALUE 'LOCK_MISSING'.
    CONSTANTS c_code_lock_expired              TYPE string VALUE 'LOCK_EXPIRED'.
    CONSTANTS c_code_lock_stolen               TYPE string VALUE 'LOCK_STOLEN'.
    CONSTANTS c_code_lock_not_owned_by_session TYPE string VALUE 'LOCK_NOT_OWNED_BY_SESSION'.
    CONSTANTS c_code_permission_denied         TYPE string VALUE 'PERMISSION_DENIED'.
    CONSTANTS c_code_authorized                TYPE string VALUE 'AUTHORIZED'.
    CONSTANTS c_code_read_only_auth            TYPE string VALUE 'READ_ONLY_AUTH'.
    CONSTANTS c_code_no_view_auth              TYPE string VALUE 'NO_VIEW_AUTH'.
    CONSTANTS c_code_no_edit_auth              TYPE string VALUE 'NO_EDIT_AUTH'.
    CONSTANTS c_code_no_delete_auth            TYPE string VALUE 'NO_DELETE_AUTH'.
    CONSTANTS c_code_no_create_auth            TYPE string VALUE 'NO_CREATE_AUTH'.
    CONSTANTS c_code_release_save_failed       TYPE string VALUE 'RELEASE_OK_BUT_TRY_SAVE_FAILED'.

    CONSTANTS c_action_acquired  TYPE string VALUE 'ACQUIRED'.
    CONSTANTS c_action_heartbeat TYPE string VALUE 'HEARTBEAT'.
    CONSTANTS c_action_released  TYPE string VALUE 'RELEASED'.
    CONSTANTS c_action_owned     TYPE string VALUE 'OWNED_BY_YOU'.
    CONSTANTS c_action_none      TYPE string VALUE 'NONE'.

    CONSTANTS c_reason_saved                TYPE string VALUE 'SAVED'.
    CONSTANTS c_reason_no_changes           TYPE string VALUE 'NO_CHANGES'.
    CONSTANTS c_environment_production      TYPE string VALUE 'production'.
    CONSTANTS c_permission_rules_empty_json TYPE string VALUE '[]'.
    CONSTANTS c_request_id_prefix_save      TYPE string VALUE 'SAVE'.

    CONSTANTS c_msg_lock_acquire_required      TYPE string VALUE 'LockAcquire: ObjectUuid and SessionGuid are required.'.
    CONSTANTS c_msg_lock_heartbeat_required    TYPE string VALUE 'LockHeartbeat: ObjectUuid and SessionGuid are required.'.
    CONSTANTS c_msg_lock_release_required      TYPE string VALUE 'LockRelease: ObjectUuid is required.'.
    CONSTANTS c_msg_lock_session_required      TYPE string VALUE 'SaveChanges: active lock for SessionGuid is required.'.
    CONSTANTS c_msg_release_save_failed_prefix TYPE string VALUE 'Final save failed during release:'.
    CONSTANTS c_msg_save_root_required         TYPE string VALUE 'SaveChanges: root.pcct_uuid is required.'.
    CONSTANTS c_msg_save_session_required      TYPE string VALUE 'SaveChanges: SessionGuid is required.'.
    CONSTANTS c_msg_save_root_mode_invalid     TYPE string VALUE 'SaveChanges: root.edit_mode must be C, U or D.'.
    CONSTANTS c_msg_save_checks_mode_required  TYPE string VALUE 'SaveChanges: checks[].edit_mode is required.'.
    CONSTANTS c_msg_save_checks_mode_invalid   TYPE string VALUE 'SaveChanges: checks[].edit_mode must be C, U or D.'.
    CONSTANTS c_msg_save_barriers_mode_required TYPE string VALUE 'SaveChanges: barriers[].edit_mode is required.'.
    CONSTANTS c_msg_save_barriers_mode_invalid  TYPE string VALUE 'SaveChanges: barriers[].edit_mode must be C, U or D.'.
    CONSTANTS c_msg_save_participants_mode_required TYPE string VALUE 'SaveChanges: participants[].edit_mode is required.'.
    CONSTANTS c_msg_save_participants_mode_invalid  TYPE string VALUE 'SaveChanges: participants[].edit_mode must be C, U or D.'.
    CONSTANTS c_msg_save_attachments_mode_required TYPE string VALUE 'SaveChanges: attachments[].edit_mode is required.'.
    CONSTANTS c_msg_save_attachments_mode_invalid  TYPE string VALUE 'SaveChanges: attachments[].edit_mode must be C, U or D.'.
    CONSTANTS c_msg_permission_authorized     TYPE string VALUE 'Authorized by productive SAP seam'.
    CONSTANTS c_msg_permission_no_view        TYPE string VALUE 'User has no display authorization for checklist scope'.
    CONSTANTS c_msg_permission_read_only      TYPE string VALUE 'User is authorized in read-only mode'.
    CONSTANTS c_msg_permission_no_edit        TYPE string VALUE 'User has no change authorization'.
    CONSTANTS c_msg_permission_no_delete      TYPE string VALUE 'User has no delete authorization'.
    CONSTANTS c_msg_permission_summary_prefix TYPE string VALUE 'Permissions:'.
    CONSTANTS c_msg_crud_not_allowed          TYPE string VALUE 'Standard CRUD is disabled for this service. Use function imports AutoSave or SaveChanges instead.'.
ENDCLASS.

CLASS zcl_zodata_contract_constants IMPLEMENTATION.
ENDCLASS.
