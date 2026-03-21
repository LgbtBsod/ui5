CLASS zcl_zodata_save_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES ty_root_reader TYPE REF TO object.

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
        /iwbep/cx_mgw_busi_exception.

    METHODS build_copy_request
      IMPORTING
        is_request     TYPE zstr_pcct_savechanges_rq
        is_source_root TYPE any
        it_checks      TYPE STANDARD TABLE
        it_barriers    TYPE STANDARD TABLE
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
        /iwbep/cx_mgw_busi_exception.
ENDCLASS.

CLASS zcl_zodata_save_service IMPLEMENTATION.
  METHOD constructor.
    mo_mapper = io_mapper.
    mo_lock_manager = io_lock_manager.
    mo_contract = io_contract.
    mo_srv_mgr = io_srv_mgr.
  ENDMETHOD.

  METHOD raise_busi_exception.
    DATA lo_container TYPE REF TO /iwbep/if_message_container.
    lo_container = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).
    lo_container->add_message_text_only(
      iv_msg_type = /iwbep/if_mgw_defines=>gcs_msg_type-error
      iv_msg_text = |{ iv_code }: { iv_text }| ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_container.
  ENDMETHOD.

  METHOD validate_save_request.
    IF is_request-root-pcct_uuid IS INITIAL.
      raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_root_required iv_code = zif_zodata_contract_constants=>c_code_validation_error ).
    ENDIF.
    IF is_request-session_guid IS INITIAL.
      raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_session_required iv_code = zif_zodata_contract_constants=>c_code_validation_error ).
    ENDIF.
    IF is_request-root-edit_mode IS NOT INITIAL AND is_request-root-edit_mode <> 'C' AND is_request-root-edit_mode <> 'U' AND is_request-root-edit_mode <> 'D'.
      raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_root_mode_invalid iv_code = zif_zodata_contract_constants=>c_code_validation_error ).
    ENDIF.
    LOOP AT is_request-checks ASSIGNING FIELD-SYMBOL(<ls_check>).
      IF <ls_check>-edit_mode IS INITIAL.
        raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_checks_mode_required iv_code = zif_zodata_contract_constants=>c_code_validation_error ).
      ENDIF.
      IF <ls_check>-edit_mode <> 'C' AND <ls_check>-edit_mode <> 'U' AND <ls_check>-edit_mode <> 'D'.
        raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_checks_mode_invalid iv_code = zif_zodata_contract_constants=>c_code_validation_error ).
      ENDIF.
    ENDLOOP.
    LOOP AT is_request-barriers ASSIGNING FIELD-SYMBOL(<ls_barrier>).
      IF <ls_barrier>-edit_mode IS INITIAL.
        raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_barriers_mode_required iv_code = zif_zodata_contract_constants=>c_code_validation_error ).
      ENDIF.
      IF <ls_barrier>-edit_mode <> 'C' AND <ls_barrier>-edit_mode <> 'U' AND <ls_barrier>-edit_mode <> 'D'.
        raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_barriers_mode_invalid iv_code = zif_zodata_contract_constants=>c_code_validation_error ).
      ENDIF.
    ENDLOOP.
    LOOP AT is_request-participants ASSIGNING FIELD-SYMBOL(<ls_participant>).
      IF <ls_participant>-edit_mode IS INITIAL.
        raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_participants_mode_required iv_code = zif_zodata_contract_constants=>c_code_validation_error ).
      ENDIF.
      IF <ls_participant>-edit_mode <> 'C' AND <ls_participant>-edit_mode <> 'U' AND <ls_participant>-edit_mode <> 'D'.
        raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_participants_mode_invalid iv_code = zif_zodata_contract_constants=>c_code_validation_error ).
      ENDIF.
    ENDLOOP.
    LOOP AT is_request-attachments ASSIGNING FIELD-SYMBOL(<ls_attachment>).
      IF <ls_attachment>-edit_mode IS INITIAL.
        raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_attachments_mode_required iv_code = zif_zodata_contract_constants=>c_code_validation_error ).
      ENDIF.
      IF <ls_attachment>-edit_mode <> 'C' AND <ls_attachment>-edit_mode <> 'U' AND <ls_attachment>-edit_mode <> 'D'.
        raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_attachments_mode_invalid iv_code = zif_zodata_contract_constants=>c_code_validation_error ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD execute_save.
    DATA lt_change TYPE zif_zodata_bopf_mapper=>tt_change.
    DATA lt_mod TYPE /bobf/t_frw_modification.
    DATA lt_failed TYPE /bobf/t_frw_key.
    DATA lt_msg TYPE /bobf/t_frw_message_k.
    DATA ls_lock_hb TYPE zstr_pcct_lock_heartbeat_rs.
    DATA lv_now_ts TYPE timestampl.
    DATA lv_request_id TYPE string.

    validate_save_request( is_request ).

    AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist
      ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_change
      ID 'BUKRS' FIELD iv_bukrs.
    IF sy-subrc <> 0.
      raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_no_edit_auth_save iv_code = zif_zodata_contract_constants=>c_code_no_edit_auth ).
    ENDIF.

    TRY.
        mo_lock_manager->heartbeat(
          EXPORTING
            is_key          = VALUE zif_zodata_lock_manager=>ty_key( bo_key = zif_i_bo_c=>sc_bo_key object_id = is_request-root-pcct_uuid )
            iv_session_guid = is_request-session_guid
          CHANGING
            cs_result       = ls_lock_hb ).
      CATCH zcx_zodata_error INTO DATA(lx_lock_hb_error).
        RAISE EXCEPTION lx_lock_hb_error.
    ENDTRY.

    IF ls_lock_hb-success <> abap_true AND ls_lock_hb-ok <> abap_true.
      raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_lock_session_required iv_code = zif_zodata_contract_constants=>c_code_lock_not_owned_by_session ).
    ENDIF.

    lt_change = mo_mapper->build_change_list( is_request ).
    GET TIME STAMP FIELD lv_now_ts.
    lv_request_id = |{ zif_zodata_contract_constants=>c_request_id_prefix_save }-{ sy-datum }{ sy-uzeit }|.

    IF lt_change IS INITIAL.
      mo_contract->fill_save_response(
        EXPORTING
          iv_pcct_uuid      = is_request-root-pcct_uuid
          iv_changed_on     = COND #( WHEN iv_changed_on IS INITIAL THEN lv_now_ts ELSE iv_changed_on )
          iv_version_number = iv_version_number
          iv_is_autosave    = iv_is_autosave
          iv_no_changes     = abap_true
          iv_code           = zif_zodata_contract_constants=>c_code_lock_ok
          iv_reason_code    = zif_zodata_contract_constants=>c_reason_no_changes
          iv_lock_refreshed = abap_true
          iv_lock_expires   = iv_lock_expires
          iv_server_now     = lv_now_ts
          iv_request_id     = lv_request_id
        CHANGING
          cs_result         = rs_response ).
      RETURN.
    ENDIF.

    lt_mod = mo_mapper->map_to_modification( lt_change ).
    mo_srv_mgr->modify( EXPORTING it_modification = lt_mod IMPORTING et_failed_key = lt_failed et_message = lt_msg ).
    zcl_zodata_bopf_msg_helper=>raise_on_failed_keys( it_failed_key = lt_failed it_message = lt_msg ).

    IF iv_is_autosave <> abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

    mo_contract->fill_save_response(
      EXPORTING
        iv_pcct_uuid      = is_request-root-pcct_uuid
        iv_changed_on     = COND #( WHEN iv_changed_on IS INITIAL THEN lv_now_ts ELSE iv_changed_on )
        iv_version_number = iv_version_number
        iv_is_autosave    = iv_is_autosave
        iv_no_changes     = abap_false
        iv_code           = zif_zodata_contract_constants=>c_code_lock_ok
        iv_reason_code    = zif_zodata_contract_constants=>c_reason_saved
        iv_lock_refreshed = abap_true
        iv_lock_expires   = iv_lock_expires
        iv_server_now     = lv_now_ts
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
    CLEAR: rs_request-root-pcct_uuid,
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
    rs_request-root-edit_mode = 'C'.
    LOOP AT rs_request-checks ASSIGNING FIELD-SYMBOL(<ls_copy_check>).
      CLEAR <ls_copy_check>-key_uuid.
      <ls_copy_check>-edit_mode = 'C'.
    ENDLOOP.
    LOOP AT rs_request-barriers ASSIGNING FIELD-SYMBOL(<ls_copy_barrier>).
      CLEAR <ls_copy_barrier>-key_uuid.
      <ls_copy_barrier>-edit_mode = 'C'.
    ENDLOOP.
    LOOP AT rs_request-participants ASSIGNING FIELD-SYMBOL(<ls_copy_participant>).
      CLEAR <ls_copy_participant>-key_uuid.
      <ls_copy_participant>-edit_mode = 'C'.
    ENDLOOP.
    LOOP AT rs_request-attachments ASSIGNING FIELD-SYMBOL(<ls_copy_attachment>).
      CLEAR <ls_copy_attachment>-key_uuid.
      <ls_copy_attachment>-edit_mode = 'C'.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
