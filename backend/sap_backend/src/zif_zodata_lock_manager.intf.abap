INTERFACE zif_zodata_lock_manager PUBLIC.

  TYPES: BEGIN OF ty_key,
           bo_key    TYPE /bobf/conf_key,
           object_id TYPE sysuuid_x16,
         END OF ty_key,
         tt_key TYPE STANDARD TABLE OF ty_key WITH DEFAULT KEY,
         BEGIN OF ty_owner,
           uname          TYPE syuname,
           session_guid   TYPE string,
           tab_session_id TYPE string,
         END OF ty_owner.

  METHODS acquire
    IMPORTING
      !is_key          TYPE ty_key
      !is_owner        TYPE ty_owner
      !iv_force_takeover TYPE abap_bool DEFAULT abap_false
    CHANGING
      !cs_result       TYPE zstr_pcct_lock_acquire_rs
    RAISING
      zcx_zodata_error.

  METHODS heartbeat
    IMPORTING
      !is_key          TYPE ty_key
      !iv_session_guid TYPE string
    CHANGING
      !cs_result       TYPE zstr_pcct_lock_heartbeat_rs
    RAISING
      zcx_zodata_error.

  METHODS release
    IMPORTING
      !is_key          TYPE ty_key
      !iv_session_guid TYPE string OPTIONAL
    RAISING
      zcx_zodata_error.

  METHODS status
    IMPORTING
      !is_key          TYPE ty_key
      !iv_session_guid TYPE string OPTIONAL
    CHANGING
      !cs_result       TYPE zstr_pcct_lock_acquire_rs
    RAISING
      zcx_zodata_error.

  METHODS ensure_session_lock
    IMPORTING
      !is_key          TYPE ty_key
      !iv_session_guid TYPE string
    RAISING
      zcx_zodata_error.

  "! Legacy wrapper kept for compatibility with older callers.
  "! Prefer ACQUIRE in new code.
  METHODS lock
    IMPORTING
      !is_key TYPE ty_key
    RAISING
      zcx_zodata_error.

  "! Legacy wrapper kept for compatibility with older callers.
  "! Prefer RELEASE in new code.
  METHODS unlock
  IMPORTING
      !is_key TYPE ty_key
      !iv_session_guid TYPE string OPTIONAL
  RAISING
      zcx_zodata_error.

  METHODS update_last_touch
    IMPORTING
      !is_key TYPE ty_key
    RAISING
      zcx_zodata_error.

ENDINTERFACE.
