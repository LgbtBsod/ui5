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

    CONSTANTS c_action_acquired  TYPE string VALUE 'ACQUIRED'.
    CONSTANTS c_action_heartbeat TYPE string VALUE 'HEARTBEAT'.
    CONSTANTS c_action_released  TYPE string VALUE 'RELEASED'.
    CONSTANTS c_action_owned     TYPE string VALUE 'OWNED_BY_YOU'.
    CONSTANTS c_action_none      TYPE string VALUE 'NONE'.
ENDCLASS.

CLASS zcl_zodata_contract_constants IMPLEMENTATION.
ENDCLASS.
