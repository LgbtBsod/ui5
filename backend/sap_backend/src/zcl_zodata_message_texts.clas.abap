CLASS zcl_zodata_message_texts DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS c_message_id TYPE symsgid VALUE 'ZPCCT_API'.
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
    CONSTANTS c_key_checklist_root_not_found_prefix TYPE string VALUE 'CHECKLIST_ROOT_NOT_FOUND_PREFIX'.

    CLASS-METHODS get_text
      IMPORTING
        iv_key TYPE string
      RETURNING
        VALUE(rv_text) TYPE string.

  PRIVATE SECTION.
    CLASS-METHODS get_message_number
      IMPORTING
        iv_key TYPE string
      RETURNING
        VALUE(rv_msgno) TYPE symsgno.

    CLASS-METHODS resolve_message_text
      IMPORTING
        iv_msgno TYPE symsgno
      RETURNING
        VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_zodata_message_texts IMPLEMENTATION.
  METHOD get_message_number.
    CASE iv_key.
      WHEN c_key_lock_acquire_required.
        rv_msgno = '001'.
      WHEN c_key_lock_heartbeat_required.
        rv_msgno = '002'.
      WHEN c_key_lock_release_required.
        rv_msgno = '003'.
      WHEN c_key_lock_session_required.
        rv_msgno = '004'.
      WHEN c_key_release_save_failed_prefix.
        rv_msgno = '005'.
      WHEN c_key_save_root_required.
        rv_msgno = '006'.
      WHEN c_key_save_session_required.
        rv_msgno = '007'.
      WHEN c_key_save_root_mode_invalid.
        rv_msgno = '008'.
      WHEN c_key_save_checks_mode_required.
        rv_msgno = '009'.
      WHEN c_key_save_checks_mode_invalid.
        rv_msgno = '010'.
      WHEN c_key_save_barriers_mode_required.
        rv_msgno = '011'.
      WHEN c_key_save_barriers_mode_invalid.
        rv_msgno = '012'.
      WHEN c_key_save_participants_mode_required.
        rv_msgno = '013'.
      WHEN c_key_save_participants_mode_invalid.
        rv_msgno = '014'.
      WHEN c_key_save_attachments_mode_required.
        rv_msgno = '015'.
      WHEN c_key_save_attachments_mode_invalid.
        rv_msgno = '016'.
      WHEN c_key_permission_authorized.
        rv_msgno = '017'.
      WHEN c_key_permission_no_view.
        rv_msgno = '018'.
      WHEN c_key_permission_read_only.
        rv_msgno = '019'.
      WHEN c_key_permission_no_edit.
        rv_msgno = '020'.
      WHEN c_key_permission_no_delete.
        rv_msgno = '021'.
      WHEN c_key_permission_summary_prefix.
        rv_msgno = '022'.
      WHEN c_key_crud_not_allowed.
        rv_msgno = '023'.
      WHEN c_key_no_create_auth.
        rv_msgno = '024'.
      WHEN c_key_no_create_auth_copy.
        rv_msgno = '025'.
      WHEN c_key_no_edit_auth.
        rv_msgno = '026'.
      WHEN c_key_no_edit_auth_save.
        rv_msgno = '027'.
      WHEN c_key_mpl_tree_read_failed_prefix.
        rv_msgno = '028'.
      WHEN c_key_checklist_root_not_found_prefix.
        rv_msgno = '029'.
      WHEN OTHERS.
        CLEAR rv_msgno.
    ENDCASE.
  ENDMETHOD.

  METHOD resolve_message_text.
    MESSAGE ID c_message_id TYPE 'S' NUMBER iv_msgno INTO rv_text.
  ENDMETHOD.

  METHOD get_text.
    DATA(lv_msgno) = get_message_number( iv_key ).
    IF lv_msgno IS INITIAL.
      rv_text = iv_key.
      RETURN.
    ENDIF.
    rv_text = resolve_message_text( lv_msgno ).
  ENDMETHOD.
ENDCLASS.
