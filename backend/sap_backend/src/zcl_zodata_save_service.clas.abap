CLASS zcl_zodata_save_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_mapper       TYPE REF TO zif_zodata_bopf_mapper
        io_lock_manager TYPE REF TO zif_zodata_lock_manager
        io_contract     TYPE REF TO zcl_zodata_contract_service
        io_srv_mgr      TYPE REF TO /bobf/if_tra_service_manager.

    METHODS execute_save
      IMPORTING
        is_request        TYPE zstr_pcct_savechanges_rq
        iv_is_autosave    TYPE abap_bool
        iv_bukrs          TYPE bukrs
        iv_changed_on     TYPE timestampl
        iv_version_number TYPE int4
        iv_lock_expires   TYPE timestampl
      RETURNING
        VALUE(rs_response) TYPE zstr_pcct_savechanges_rs
      RAISING
        /iwbep/cx_mgw_busi_exception
        zcx_zodata_error.

    METHODS validate_save_request
      IMPORTING
        is_request TYPE zstr_pcct_savechanges_rq
      RAISING
        zcx_zodata_error.

    METHODS build_copy_request
      IMPORTING
        is_request     TYPE zstr_pcct_savechanges_rq
        is_source_root TYPE zcl_zodata_read_service=>ty_root_row
        it_checks      TYPE zcl_zodata_read_service=>tt_check_row
        it_barriers    TYPE zcl_zodata_read_service=>tt_barrier_row
      RETURNING
        VALUE(rs_request) TYPE zstr_pcct_savechanges_rq.

  PRIVATE SECTION.
    DATA mo_mapper TYPE REF TO zif_zodata_bopf_mapper.
    DATA mo_lock_manager TYPE REF TO zif_zodata_lock_manager.
    DATA mo_contract TYPE REF TO zcl_zodata_contract_service.
    DATA mo_srv_mgr TYPE REF TO /bobf/if_tra_service_manager.

    METHODS raise_busi_exception
      IMPORTING
        iv_text TYPE string
        iv_code TYPE string
      RAISING
        zcx_zodata_error.
    METHODS validate_edit_mode
      IMPORTING
        iv_edit_mode TYPE csequence
        iv_required_text TYPE string
        iv_invalid_text TYPE string
      RAISING
        zcx_zodata_error.
    METHODS refresh_session_lock
      IMPORTING
        is_request TYPE zstr_pcct_savechanges_rq
      RETURNING
        VALUE(rs_lock_hb) TYPE zstr_pcct_lock_heartbeat_rs
      RAISING
        zcx_zodata_error.
    METHODS finalize_save_transaction
      IMPORTING
        iv_is_autosave TYPE abap_bool
      RAISING
        zcx_zodata_error.
    METHODS resolve_lock_expires
      IMPORTING
        is_lock_hb      TYPE zstr_pcct_lock_heartbeat_rs
        iv_lock_expires TYPE timestampl
      RETURNING
        VALUE(rv_lock_expires) TYPE timestampl.
    METHODS resolve_server_now
      IMPORTING
        is_lock_hb   TYPE zstr_pcct_lock_heartbeat_rs
        iv_server_now TYPE timestampl
      RETURNING
        VALUE(rv_server_now) TYPE timestampl.
ENDCLASS.

CLASS zcl_zodata_save_service IMPLEMENTATION.
  METHOD constructor.
    mo_mapper = io_mapper.
    mo_lock_manager = io_lock_manager.
    mo_contract = io_contract.
    mo_srv_mgr = io_srv_mgr.
  ENDMETHOD.

  METHOD raise_busi_exception.
    RAISE EXCEPTION TYPE zcx_zodata_error
      EXPORTING
        iv_code = iv_code
        iv_msg  = iv_text.
  ENDMETHOD.

  METHOD validate_edit_mode.
    IF iv_edit_mode IS INITIAL.
      raise_busi_exception(
        iv_text = iv_required_text
        iv_code = zif_zodata_message_codes=>validation_error ).
    ENDIF.

    IF iv_edit_mode <> zif_zodata_contract_constants=>c_edit_mode_create
       AND iv_edit_mode <> zif_zodata_contract_constants=>c_edit_mode_update
       AND iv_edit_mode <> zif_zodata_contract_constants=>c_edit_mode_delete.
      raise_busi_exception(
        iv_text = iv_invalid_text
        iv_code = zif_zodata_message_codes=>validation_error ).
    ENDIF.
  ENDMETHOD.

  METHOD validate_save_request.
    IF is_request-root-key IS INITIAL.
      raise_busi_exception( iv_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_save_root_required ) iv_code = zif_zodata_message_codes=>validation_error ).
    ENDIF.
    IF is_request-session_guid IS INITIAL.
      raise_busi_exception( iv_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_save_session_required ) iv_code = zif_zodata_message_codes=>validation_error ).
    ENDIF.
    IF is_request-root-edit_mode IS NOT INITIAL
       AND is_request-root-edit_mode <> zif_zodata_contract_constants=>c_edit_mode_create
       AND is_request-root-edit_mode <> zif_zodata_contract_constants=>c_edit_mode_update
       AND is_request-root-edit_mode <> zif_zodata_contract_constants=>c_edit_mode_delete.
      raise_busi_exception( iv_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_save_root_mode_invalid ) iv_code = zif_zodata_message_codes=>validation_error ).
    ENDIF.
    LOOP AT is_request-checks ASSIGNING FIELD-SYMBOL(<ls_check>).
      validate_edit_mode(
        iv_edit_mode = <ls_check>-edit_mode
        iv_required_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_save_checks_mode_required )
        iv_invalid_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_save_checks_mode_invalid ) ).
    ENDLOOP.
    LOOP AT is_request-barriers ASSIGNING FIELD-SYMBOL(<ls_barrier>).
      validate_edit_mode(
        iv_edit_mode = <ls_barrier>-edit_mode
        iv_required_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_save_barriers_mode_required )
        iv_invalid_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_save_barriers_mode_invalid ) ).
    ENDLOOP.
    LOOP AT is_request-participants ASSIGNING FIELD-SYMBOL(<ls_participant>).
      validate_edit_mode(
        iv_edit_mode = <ls_participant>-edit_mode
        iv_required_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_save_participants_mode_required )
        iv_invalid_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_save_participants_mode_invalid ) ).
    ENDLOOP.
    LOOP AT is_request-attachments ASSIGNING FIELD-SYMBOL(<ls_attachment>).
      validate_edit_mode(
        iv_edit_mode = <ls_attachment>-edit_mode
        iv_required_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_save_attachments_mode_required )
        iv_invalid_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_save_attachments_mode_invalid ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD refresh_session_lock.
    TRY.
        mo_lock_manager->heartbeat(
          EXPORTING
            is_key          = VALUE zif_zodata_lock_manager=>ty_key(
                                bo_key    = zif_i_bo_c=>sc_bo_key
                                object_id = is_request-root-key )
            iv_session_guid = is_request-session_guid
          CHANGING
            cs_result       = rs_lock_hb ).
      CATCH zcx_zodata_error INTO DATA(lx_lock_hb_error).
        RAISE EXCEPTION lx_lock_hb_error.
    ENDTRY.

    IF rs_lock_hb-success <> abap_true AND rs_lock_hb-ok <> abap_true.
      raise_busi_exception(
        iv_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_lock_session_required )
        iv_code = zif_zodata_message_codes=>lock_not_owned_by_session ).
    ENDIF.
  ENDMETHOD.

  METHOD finalize_save_transaction.
    DATA(lo_txn_mgr) = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).
    DATA lo_message TYPE REF TO /bobf/if_frw_message.
    DATA lv_rejected TYPE boole_d.

    lo_txn_mgr->save(
      IMPORTING
        eo_message  = lo_message
        ev_rejected = lv_rejected ).

    IF lv_rejected = abap_true.
      RAISE EXCEPTION TYPE zcx_zodata_error
        EXPORTING iv_code = 'TECHNICAL_ERROR'
                  iv_msg  = 'BOPF transaction manager rejected the save request.'.
    ENDIF.
  ENDMETHOD.

  METHOD resolve_lock_expires.
    rv_lock_expires = iv_lock_expires.

    ASSIGN COMPONENT 'LOCK_EXPIRES_AT' OF STRUCTURE is_lock_hb TO FIELD-SYMBOL(<lv_lock_expires_at>).
    IF sy-subrc = 0 AND <lv_lock_expires_at> IS ASSIGNED AND <lv_lock_expires_at> IS NOT INITIAL.
      rv_lock_expires = <lv_lock_expires_at>.
      RETURN.
    ENDIF.

    ASSIGN COMPONENT 'LOCK_EXPIRES' OF STRUCTURE is_lock_hb TO FIELD-SYMBOL(<lv_lock_expires>).
    IF sy-subrc = 0 AND <lv_lock_expires> IS ASSIGNED AND <lv_lock_expires> IS NOT INITIAL.
      rv_lock_expires = <lv_lock_expires>.
    ENDIF.
  ENDMETHOD.

  METHOD resolve_server_now.
    rv_server_now = iv_server_now.

    ASSIGN COMPONENT 'SERVER_NOW' OF STRUCTURE is_lock_hb TO FIELD-SYMBOL(<lv_server_now>).
    IF sy-subrc = 0 AND <lv_server_now> IS ASSIGNED AND <lv_server_now> IS NOT INITIAL.
      rv_server_now = <lv_server_now>.
    ENDIF.
  ENDMETHOD.

  METHOD execute_save.
    DATA lt_change TYPE zif_zodata_bopf_mapper=>tt_change.
    DATA lt_mod TYPE /bobf/t_frw_modification.
    DATA lt_failed TYPE /bobf/t_frw_key.
    DATA lt_msg TYPE /bobf/t_frw_message_k.
    DATA ls_lock_hb TYPE zstr_pcct_lock_heartbeat_rs.
    DATA lv_now_ts TYPE timestampl.
    DATA lv_request_id TYPE string.
    DATA lv_effective_lock_expires TYPE timestampl.
    DATA lv_effective_server_now TYPE timestampl.

    validate_save_request( is_request ).

    AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist
      ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_change
      ID 'BUKRS' FIELD iv_bukrs.
    IF sy-subrc <> 0.
      raise_busi_exception( iv_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_no_edit_auth_save ) iv_code = zif_zodata_message_codes=>no_edit_auth ).
    ENDIF.

    ls_lock_hb = refresh_session_lock( is_request ).

    lt_change = mo_mapper->build_change_list( is_request ).
    GET TIME STAMP FIELD lv_now_ts.
    lv_effective_lock_expires = resolve_lock_expires(
      is_lock_hb      = ls_lock_hb
      iv_lock_expires = iv_lock_expires ).
    lv_effective_server_now = resolve_server_now(
      is_lock_hb    = ls_lock_hb
      iv_server_now = lv_now_ts ).
    lv_request_id = |{ zif_zodata_contract_constants=>c_request_id_prefix_save }-{ sy-datum }{ sy-uzeit }|.

    IF lt_change IS INITIAL.
      mo_contract->fill_save_response(
        EXPORTING
          iv_pcct_uuid      = is_request-root-key
          iv_changed_on     = COND #( WHEN iv_changed_on IS INITIAL THEN lv_effective_server_now ELSE iv_changed_on )
          iv_version_number = iv_version_number
          iv_is_autosave    = iv_is_autosave
          iv_no_changes     = abap_true
          iv_code           = zif_zodata_message_codes=>lock_ok
          iv_reason_code    = zif_zodata_contract_constants=>c_reason_no_changes
          iv_lock_refreshed = abap_true
          iv_lock_expires   = lv_effective_lock_expires
          iv_server_now     = lv_effective_server_now
          iv_request_id     = lv_request_id
        CHANGING
          cs_result         = rs_response ).
      RETURN.
    ENDIF.

    lt_mod = mo_mapper->map_to_modification( lt_change ).
    mo_srv_mgr->modify( EXPORTING it_modification = lt_mod IMPORTING et_failed_key = lt_failed et_message = lt_msg ).
    zcl_zodata_bopf_msg_helper=>raise_on_failed_keys( it_failed_key = lt_failed it_message = lt_msg ).

    finalize_save_transaction( iv_is_autosave ).

    mo_contract->fill_save_response(
      EXPORTING
        iv_pcct_uuid      = is_request-root-key
        iv_changed_on     = COND #( WHEN iv_changed_on IS INITIAL THEN lv_effective_server_now ELSE iv_changed_on )
        iv_version_number = iv_version_number
        iv_is_autosave    = iv_is_autosave
        iv_no_changes     = abap_false
        iv_code           = zif_zodata_message_codes=>lock_ok
        iv_reason_code    = zif_zodata_contract_constants=>c_reason_saved
        iv_lock_refreshed = abap_true
        iv_lock_expires   = lv_effective_lock_expires
        iv_server_now     = lv_effective_server_now
        iv_request_id     = lv_request_id
      CHANGING
        cs_result         = rs_response ).

    LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).
      DATA(lv_msg_text) = <ls_msg>-message->if_message~get_text( ).
      APPEND VALUE #( msg_type = <ls_msg>-message->if_message~get_message_attributes( )-msg_type msg_text = lv_msg_text ) TO rs_response-messages.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_copy_request.
    rs_request = is_request.
    rs_request-root = CORRESPONDING #( is_source_root ).
    CLEAR: rs_request-root-key,
      rs_request-root-created_on,
      rs_request-root-created_by,
      rs_request-root-changed_on,
      rs_request-root-changed_by,
      rs_request-root-version_number,
      rs_request-root-lock_owner,
      rs_request-root-lock_session,
      rs_request-root-tab_session_id,
      rs_request-root-lock_expires_at.
    rs_request-checks = CORRESPONDING #( it_checks ).
    rs_request-barriers = CORRESPONDING #( it_barriers ).
    rs_request-root-edit_mode = zif_zodata_contract_constants=>c_edit_mode_create.
    LOOP AT rs_request-checks ASSIGNING FIELD-SYMBOL(<ls_copy_check>).
      CLEAR <ls_copy_check>-key.
      <ls_copy_check>-edit_mode = zif_zodata_contract_constants=>c_edit_mode_create.
    ENDLOOP.
    LOOP AT rs_request-barriers ASSIGNING FIELD-SYMBOL(<ls_copy_barrier>).
      CLEAR <ls_copy_barrier>-key.
      <ls_copy_barrier>-edit_mode = zif_zodata_contract_constants=>c_edit_mode_create.
    ENDLOOP.
    LOOP AT rs_request-participants ASSIGNING FIELD-SYMBOL(<ls_copy_participant>).
      CLEAR <ls_copy_participant>-key.
      <ls_copy_participant>-edit_mode = zif_zodata_contract_constants=>c_edit_mode_create.
    ENDLOOP.
    LOOP AT rs_request-attachments ASSIGNING FIELD-SYMBOL(<ls_copy_attachment>).
      CLEAR <ls_copy_attachment>-key.
      <ls_copy_attachment>-edit_mode = zif_zodata_contract_constants=>c_edit_mode_create.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
