CLASS zcl_zodata_dpc_ext DEFINITION PUBLIC INHERITING FROM zcl_zodata_dpc CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS checklistrootset_get_entity REDEFINITION.
    METHODS checklistrootset_get_entityset REDEFINITION.
    METHODS checklistbasicinfos_get_entity REDEFINITION.
    METHODS checklistbasicinfos_get_entityset REDEFINITION.
    METHODS checklistcheckset_get_entityset REDEFINITION.
    METHODS checklistbarriers_get_entityset REDEFINITION.
    METHODS checklistpermissions_get_entity REDEFINITION.
    METHODS checklistpermissions_get_entityset REDEFINITION.
    METHODS currentuserset_get_entity REDEFINITION.
    METHODS runtimesettings_get_entity REDEFINITION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~create_entity REDEFINITION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~update_entity REDEFINITION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~delete_entity REDEFINITION.
    METHODS lockacquire_create_entity REDEFINITION.
    METHODS lockheartbeat_create_entity REDEFINITION.
    METHODS lockrelease_create_entity REDEFINITION.
    METHODS autosave_create_entity REDEFINITION.
    METHODS savechanges_create_entity REDEFINITION.
    METHODS mpltreeset_get_entityset REDEFINITION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_root_row,
      pcct_uuid TYPE sysuuid_x16,
      checklist_id TYPE char20,
      lpc TYPE char10,
      lpc_text TYPE string,
      status TYPE char20,
      integration_flag TYPE abap_bool,
      date_check TYPE datum,
      time_check TYPE tims,
      time_zone TYPE string,
      equipment TYPE string,
      bukrs TYPE bukrs,
      observer_fullname TYPE string,
      observer_perner TYPE pernr_d,
      observer_position TYPE string,
      observer_orgunit TYPE string,
      observed_fullname TYPE string,
      observed_perner TYPE pernr_d,
      observed_position TYPE string,
      observed_orgunit TYPE string,
      location_key TYPE string,
      location_name TYPE string,
      location_text TYPE string,
      changed_on TYPE timestampl,
      changed_by TYPE syuname,
      created_on TYPE timestampl,
      created_by TYPE syuname,
      version_number TYPE int4,
      checks_total TYPE int4,
      checks_success TYPE int4,
      barriers_total TYPE int4,
      barriers_success TYPE int4,
      lock_owner TYPE syuname,
      lock_session TYPE string,
      tab_session_id TYPE string,
      lock_expires_at TYPE timestampl,
    END OF ty_root_row,
    tt_root_row TYPE STANDARD TABLE OF ty_root_row WITH DEFAULT KEY,
    BEGIN OF ty_check_row,
      key_uuid TYPE sysuuid_x16,
      root_key TYPE sysuuid_x16,
      checks_num TYPE int4,
      text TYPE string,
      comment_text TYPE string,
      result TYPE abap_bool,
      changed_on TYPE timestampl,
    END OF ty_check_row,
    tt_check_row TYPE STANDARD TABLE OF ty_check_row WITH DEFAULT KEY,
    BEGIN OF ty_barrier_row,
      key_uuid TYPE sysuuid_x16,
      root_key TYPE sysuuid_x16,
      barriers_num TYPE int4,
      text TYPE string,
      comment_text TYPE string,
      result TYPE abap_bool,
      changed_on TYPE timestampl,
    END OF ty_barrier_row,
    tt_barrier_row TYPE STANDARD TABLE OF ty_barrier_row WITH DEFAULT KEY.
    DATA mo_lock_manager TYPE REF TO zif_zodata_lock_manager.
    DATA mo_mapper TYPE REF TO zif_zodata_bopf_mapper.
    DATA mo_contract TYPE REF TO zcl_zodata_contract_service.
    METHODS ensure_deps.
    METHODS get_srv_mgr RETURNING VALUE(ro_srv_mgr) TYPE REF TO /bobf/if_tra_service_manager.
    METHODS execute_save IMPORTING is_request TYPE zstr_pcct_savechanges_rq iv_is_autosave TYPE abap_bool DEFAULT abap_false RETURNING VALUE(rs_response) TYPE zstr_pcct_savechanges_rs RAISING /iwbep/cx_mgw_busi_exception zcx_zodata_error.
    METHODS validate_save_request IMPORTING is_request TYPE zstr_pcct_savechanges_rq RAISING /iwbep/cx_mgw_busi_exception.
    METHODS raise_busi_exception IMPORTING iv_text TYPE string iv_code TYPE string OPTIONAL RAISING /iwbep/cx_mgw_busi_exception.
    METHODS raise_crud_not_allowed IMPORTING iv_context TYPE string RAISING /iwbep/cx_mgw_busi_exception.
    METHODS read_root_key IMPORTING it_key_tab TYPE /iwbep/t_mgw_name_value_pair RETURNING VALUE(rv_rootkey) TYPE sysuuid_x16 RAISING /iwbep/cx_mgw_busi_exception.
    METHODS read_root_row IMPORTING iv_rootkey TYPE sysuuid_x16 RETURNING VALUE(rs_root) TYPE ty_root_row RAISING /iwbep/cx_mgw_busi_exception.
    METHODS read_root_rows RETURNING VALUE(rt_root) TYPE tt_root_row.
    METHODS read_check_rows IMPORTING iv_rootkey TYPE sysuuid_x16 OPTIONAL RETURNING VALUE(rt_checks) TYPE tt_check_row.
    METHODS read_barrier_rows IMPORTING iv_rootkey TYPE sysuuid_x16 OPTIONAL RETURNING VALUE(rt_barriers) TYPE tt_barrier_row.
    METHODS build_permission_row IMPORTING iv_rootkey TYPE sysuuid_x16 RETURNING VALUE(rs_result) TYPE zstr_pcct_permission_rs.
    METHODS build_current_user_row RETURNING VALUE(rs_result) TYPE zstr_pcct_current_user_rs.
    METHODS build_runtime_settings_row RETURNING VALUE(rs_result) TYPE zstr_pcct_runtime_settings_rs.
ENDCLASS.

CLASS zcl_zodata_dpc_ext IMPLEMENTATION.
  METHOD ensure_deps.
    IF mo_mapper IS INITIAL. mo_mapper = zcl_zodata_bopf_mapper=>create( ). ENDIF.
    IF mo_lock_manager IS INITIAL. mo_lock_manager = NEW zcl_zodata_lock_manager( ). ENDIF.
    IF mo_contract IS INITIAL. mo_contract = NEW zcl_zodata_contract_service( ). ENDIF.
  ENDMETHOD.
  METHOD get_srv_mgr.
    ro_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_i_bo_c=>sc_bo_key ).
  ENDMETHOD.
  METHOD /iwbep/if_mgw_appl_srv_runtime~create_entity.
    raise_crud_not_allowed( 'CREATE_ENTITY' ).
  ENDMETHOD.
  METHOD /iwbep/if_mgw_appl_srv_runtime~update_entity.
    raise_crud_not_allowed( 'UPDATE_ENTITY' ).
  ENDMETHOD.
  METHOD /iwbep/if_mgw_appl_srv_runtime~delete_entity.
    raise_crud_not_allowed( 'DELETE_ENTITY' ).
  ENDMETHOD.
  METHOD lockacquire_create_entity.
    DATA ls_req TYPE zstr_pcct_lock_acquire_rq.
    DATA ls_result TYPE zstr_pcct_lock_acquire_rs.
    DATA lv_now_ts TYPE timestampl.
    DATA lv_exp_ts TYPE timestampl.
    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).
    IF ls_req-object_uuid IS INITIAL OR ls_req-session_guid IS INITIAL.
      raise_busi_exception(
        iv_text = zcl_zodata_contract_constants=>c_msg_lock_acquire_required
        iv_code = 'VALIDATION_ERROR' ).
    ENDIF.
    TRY.
        DATA(ls_lock_key) = VALUE zif_zodata_lock_manager=>ty_key( bo_key = zif_i_bo_c=>sc_bo_key object_id = ls_req-object_uuid ).
        mo_lock_manager->acquire(
          EXPORTING
            is_key = ls_lock_key
            is_owner = VALUE zif_zodata_lock_manager=>ty_owner( uname = sy-uname session_guid = ls_req-session_guid tab_session_id = ls_req-tab_session_id )
          CHANGING
            cs_result = ls_result ).
        ls_result-success = abap_true.
        ls_result-action = zcl_zodata_contract_constants=>c_action_acquired.
        ls_result-owner = sy-uname.
        ls_result-owner_session = ls_req-session_guid.
        ls_result-tab_session_id = ls_req-tab_session_id.
        ls_result-object_uuid = ls_req-object_uuid.
        GET TIME STAMP FIELD lv_now_ts.
        lv_exp_ts = cl_abap_tstmp=>add( tstmp = lv_now_ts secs = zcl_zodata_contract_constants=>c_lock_ttl_seconds ).
        ls_result-lock_expires = lv_exp_ts.
        mo_contract->fill_lock_result(
          EXPORTING iv_ok = abap_true iv_code = zcl_zodata_contract_constants=>c_code_lock_ok iv_action = zcl_zodata_contract_constants=>c_action_acquired iv_owner = sy-uname iv_owner_session = ls_req-session_guid iv_tab_session_id = ls_req-tab_session_id iv_object_uuid = ls_req-object_uuid iv_lock_expires = lv_exp_ts iv_server_now = lv_now_ts iv_lock_refreshed = abap_true iv_owner_session_match = abap_true
          CHANGING cs_result = ls_result ).
        copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_lock_error).
        raise_busi_exception( iv_text = lx_lock_error->get_message_text( ) iv_code = lx_lock_error->get_code( ) ).
      CATCH zcx_lock_error INTO DATA(lx_lock).
        raise_busi_exception(
          iv_text = |Заблокировано: { lx_lock->user_fullname } (Таб.№ { lx_lock->pernr })|
          iv_code = zcl_zodata_contract_constants=>c_code_lock_stolen ).
    ENDTRY.
  ENDMETHOD.
  METHOD lockheartbeat_create_entity.
    DATA ls_req TYPE zstr_pcct_lock_heartbeat_rq.
    DATA ls_result TYPE zstr_pcct_lock_heartbeat_rs.
    DATA lv_now_ts TYPE timestampl.
    DATA lv_exp_ts TYPE timestampl.
    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).
    IF ls_req-object_uuid IS INITIAL OR ls_req-session_guid IS INITIAL.
      raise_busi_exception(
        iv_text = zcl_zodata_contract_constants=>c_msg_lock_heartbeat_required
        iv_code = 'VALIDATION_ERROR' ).
    ENDIF.
    TRY.
        mo_lock_manager->heartbeat(
          EXPORTING
            is_key = VALUE zif_zodata_lock_manager=>ty_key( bo_key = zif_i_bo_c=>sc_bo_key object_id = ls_req-object_uuid )
            iv_session_guid = ls_req-session_guid
          CHANGING
            cs_result = ls_result ).
        GET TIME STAMP FIELD lv_now_ts.
        lv_exp_ts = cl_abap_tstmp=>add( tstmp = lv_now_ts secs = zcl_zodata_contract_constants=>c_lock_ttl_seconds ).
        mo_contract->fill_lock_result(
          EXPORTING iv_ok = abap_true iv_code = zcl_zodata_contract_constants=>c_code_lock_ok iv_action = zcl_zodata_contract_constants=>c_action_heartbeat iv_owner = sy-uname iv_owner_session = ls_req-session_guid iv_object_uuid = ls_req-object_uuid iv_lock_expires = lv_exp_ts iv_server_now = lv_now_ts iv_lock_refreshed = abap_true iv_owner_session_match = abap_true
          CHANGING cs_result = ls_result ).
        copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_heartbeat_error).
        raise_busi_exception( iv_text = lx_heartbeat_error->get_message_text( ) iv_code = lx_heartbeat_error->get_code( ) ).
    ENDTRY.
  ENDMETHOD.
  METHOD lockrelease_create_entity.
    DATA ls_req TYPE zstr_pcct_lock_release_rq.
    DATA ls_result TYPE zstr_pcct_lock_release_rs.
    DATA lv_now_ts TYPE timestampl.
    DATA lv_release_code TYPE string VALUE zcl_zodata_contract_constants=>c_code_lock_ok.
    DATA lv_release_message TYPE string.
    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).
    IF ls_req-object_uuid IS INITIAL.
      raise_busi_exception(
        iv_text = zcl_zodata_contract_constants=>c_msg_lock_release_required
        iv_code = 'VALIDATION_ERROR' ).
    ENDIF.
    TRY.
        DATA(ls_lock_key) = VALUE zif_zodata_lock_manager=>ty_key( bo_key = zif_i_bo_c=>sc_bo_key object_id = ls_req-object_uuid ).
        mo_lock_manager->unlock( ls_lock_key ).
      CATCH zcx_lock_error.
    ENDTRY.
    IF ls_req-try_save = abap_true AND ls_req-payload IS NOT INITIAL.
      TRY.
          execute_save( EXPORTING is_request = ls_req-payload iv_is_autosave = abap_false ).
        CATCH /iwbep/cx_mgw_busi_exception INTO DATA(lx_busi).
          lv_release_code = zcl_zodata_contract_constants=>c_code_release_save_failed.
          lv_release_message = lx_busi->get_text( ).
          ls_result-success = abap_false.
          ls_result-message = |{ zcl_zodata_contract_constants=>c_msg_release_save_failed_prefix } { lv_release_message }|.
          ls_result-reason_code = lv_release_code.
        CATCH zcx_zodata_error INTO DATA(lx_save_error).
          lv_release_code = zcl_zodata_contract_constants=>c_code_release_save_failed.
          lv_release_message = lx_save_error->get_message_text( ).
          ls_result-success = abap_false.
          ls_result-message = |{ zcl_zodata_contract_constants=>c_msg_release_save_failed_prefix } { lv_release_message }|.
          ls_result-reason_code = lv_release_code.
      ENDTRY.
    ELSE.
      COMMIT WORK AND WAIT.
    ENDIF.
    IF ls_result-success IS INITIAL AND lv_release_code = zcl_zodata_contract_constants=>c_code_lock_ok.
      ls_result-success = abap_true.
    ENDIF.
    ls_result-action = zcl_zodata_contract_constants=>c_action_released.
    ls_result-object_uuid = ls_req-object_uuid.
    GET TIME STAMP FIELD lv_now_ts.
    mo_contract->fill_lock_result(
      EXPORTING iv_ok = xsdbool( lv_release_code = zcl_zodata_contract_constants=>c_code_lock_ok ) iv_code = lv_release_code iv_action = zcl_zodata_contract_constants=>c_action_released iv_object_uuid = ls_req-object_uuid iv_owner_session = ls_req-session_guid iv_server_now = lv_now_ts iv_lock_refreshed = abap_false iv_owner_session_match = abap_true
      CHANGING cs_result = ls_result ).
    copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
  ENDMETHOD.
  METHOD autosave_create_entity.
    DATA ls_req TYPE zstr_pcct_savechanges_rq.
    DATA ls_resp TYPE zstr_pcct_savechanges_rs.
    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).
    TRY.
        ls_resp = execute_save( EXPORTING is_request = ls_req iv_is_autosave = abap_true ).
        copy_data_to_ref( EXPORTING is_data = ls_resp CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_map_autosave).
        raise_busi_exception( iv_text = lx_map_autosave->get_message_text( ) iv_code = lx_map_autosave->get_code( ) ).
    ENDTRY.
  ENDMETHOD.
  METHOD savechanges_create_entity.
    DATA ls_req TYPE zstr_pcct_savechanges_rq.
    DATA ls_resp TYPE zstr_pcct_savechanges_rs.
    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).
    TRY.
        ls_resp = execute_save( EXPORTING is_request = ls_req iv_is_autosave = abap_false ).
        copy_data_to_ref( EXPORTING is_data = ls_resp CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_map_save).
        raise_busi_exception( iv_text = lx_map_save->get_message_text( ) iv_code = lx_map_save->get_code( ) ).
    ENDTRY.
  ENDMETHOD.
  METHOD mpltreeset_get_entityset.
    DATA: lt_tree TYPE ztt_pcct_mpl_tree, lv_date TYPE datum.
    lv_date = sy-datum.
    READ TABLE it_filter_select_options ASSIGNING FIELD-SYMBOL(<ls_filter>) WITH KEY property = 'Date'.
    IF sy-subrc = 0 AND <ls_filter>-select_options IS NOT INITIAL.
      lv_date = <ls_filter>-select_options[ 1 ]-low.
    ENDIF.
    CALL FUNCTION 'Z_PCCT_MPL_TREE_GET' EXPORTING iv_date = lv_date TABLES et_tree = lt_tree EXCEPTIONS not_found = 1 OTHERS = 2.
    IF sy-subrc <> 0. raise_busi_exception( iv_text = |MPL tree read failed for date { lv_date }.| iv_code = 'TECHNICAL_ERROR' ). ENDIF.
    copy_data_to_ref( EXPORTING is_data = lt_tree CHANGING cr_data = er_entityset ).
  ENDMETHOD.
  METHOD checklistrootset_get_entity.
    DATA ls_row TYPE ty_root_row.
    ls_row = read_root_row( read_root_key( it_key_tab ) ).
    copy_data_to_ref( EXPORTING is_data = ls_row CHANGING cr_data = er_entity ).
  ENDMETHOD.
  METHOD checklistrootset_get_entityset.
    DATA lt_rows TYPE tt_root_row.
    lt_rows = read_root_rows( ).
    copy_data_to_ref( EXPORTING is_data = lt_rows CHANGING cr_data = er_entityset ).
  ENDMETHOD.
  METHOD checklistbasicinfos_get_entity.
    DATA ls_row TYPE ty_root_row.
    ls_row = read_root_row( read_root_key( it_key_tab ) ).
    copy_data_to_ref( EXPORTING is_data = ls_row CHANGING cr_data = er_entity ).
  ENDMETHOD.
  METHOD checklistbasicinfos_get_entityset.
    DATA lt_rows TYPE tt_root_row.
    lt_rows = read_root_rows( ).
    copy_data_to_ref( EXPORTING is_data = lt_rows CHANGING cr_data = er_entityset ).
  ENDMETHOD.
  METHOD checklistcheckset_get_entityset.
    DATA lv_rootkey TYPE sysuuid_x16.
    DATA lt_rows TYPE tt_check_row.
    TRY. lv_rootkey = read_root_key( it_key_tab ). CATCH /iwbep/cx_mgw_busi_exception. CLEAR lv_rootkey. ENDTRY.
    lt_rows = read_check_rows( lv_rootkey ).
    copy_data_to_ref( EXPORTING is_data = lt_rows CHANGING cr_data = er_entityset ).
  ENDMETHOD.
  METHOD checklistbarriers_get_entityset.
    DATA lv_rootkey TYPE sysuuid_x16.
    DATA lt_rows TYPE tt_barrier_row.
    TRY. lv_rootkey = read_root_key( it_key_tab ). CATCH /iwbep/cx_mgw_busi_exception. CLEAR lv_rootkey. ENDTRY.
    lt_rows = read_barrier_rows( lv_rootkey ).
    copy_data_to_ref( EXPORTING is_data = lt_rows CHANGING cr_data = er_entityset ).
  ENDMETHOD.
  METHOD checklistpermissions_get_entity.
    DATA ls_result TYPE zstr_pcct_permission_rs.
    ls_result = build_permission_row( read_root_key( it_key_tab ) ).
    copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
  ENDMETHOD.
  METHOD checklistpermissions_get_entityset.
    DATA lt_root TYPE tt_root_row.
    DATA lt_result TYPE STANDARD TABLE OF zstr_pcct_permission_rs WITH DEFAULT KEY.
    lt_root = read_root_rows( ).
    LOOP AT lt_root ASSIGNING FIELD-SYMBOL(<ls_root_row>). APPEND build_permission_row( <ls_root_row>-pcct_uuid ) TO lt_result. ENDLOOP.
    copy_data_to_ref( EXPORTING is_data = lt_result CHANGING cr_data = er_entityset ).
  ENDMETHOD.
  METHOD currentuserset_get_entity.
    DATA ls_result TYPE zstr_pcct_current_user_rs.
    ls_result = build_current_user_row( ).
    copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
  ENDMETHOD.
  METHOD runtimesettings_get_entity.
    DATA ls_result TYPE zstr_pcct_runtime_settings_rs.
    ls_result = build_runtime_settings_row( ).
    copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
  ENDMETHOD.
  METHOD execute_save.
    DATA: lt_change TYPE zif_zodata_bopf_mapper=>tt_change, lt_mod TYPE /bobf/t_frw_modification, lt_failed TYPE /bobf/t_frw_key, lt_msg TYPE /bobf/t_frw_message_k.
    DATA ls_lock_hb TYPE zstr_pcct_lock_heartbeat_rs.
    DATA ls_root TYPE ty_root_row.
    DATA lv_now_ts TYPE timestampl.
    DATA lv_request_id TYPE string.
    validate_save_request( is_request ).
    TRY.
        mo_lock_manager->heartbeat( EXPORTING is_key = VALUE zif_zodata_lock_manager=>ty_key( bo_key = zif_i_bo_c=>sc_bo_key object_id = is_request-root-pcct_uuid ) iv_session_guid = is_request-session_guid CHANGING cs_result = ls_lock_hb ).
      CATCH zcx_zodata_error INTO DATA(lx_lock_hb_error).
        raise_busi_exception( iv_text = lx_lock_hb_error->get_message_text( ) iv_code = lx_lock_hb_error->get_code( ) ).
    ENDTRY.
    IF ls_lock_hb-success <> abap_true AND ls_lock_hb-ok <> abap_true.
      raise_busi_exception(
        iv_text = zcl_zodata_contract_constants=>c_msg_lock_session_required
        iv_code = zcl_zodata_contract_constants=>c_code_lock_not_owned_by_session ).
    ENDIF.
    lt_change = mo_mapper->build_change_list( is_request ).
    IF lt_change IS INITIAL.
      GET TIME STAMP FIELD lv_now_ts.
      ls_root = read_root_row( is_request-root-pcct_uuid ).
      lv_request_id = |{ zcl_zodata_contract_constants=>c_request_id_prefix_save }-{ sy-datum }{ sy-uzeit }|.
      mo_contract->fill_save_response( EXPORTING iv_pcct_uuid = is_request-root-pcct_uuid iv_changed_on = COND #( WHEN ls_root-changed_on IS INITIAL THEN lv_now_ts ELSE ls_root-changed_on ) iv_version_number = ls_root-version_number iv_is_autosave = iv_is_autosave iv_no_changes = abap_true iv_code = zcl_zodata_contract_constants=>c_code_lock_ok iv_reason_code = zcl_zodata_contract_constants=>c_reason_no_changes iv_lock_refreshed = abap_true iv_lock_expires = ls_root-lock_expires_at iv_server_now = lv_now_ts iv_request_id = lv_request_id CHANGING cs_result = rs_response ).
      RETURN.
    ENDIF.
    lt_mod = mo_mapper->map_to_modification( lt_change ).
    get_srv_mgr( )->modify( EXPORTING it_modification = lt_mod IMPORTING et_failed_key = lt_failed et_message = lt_msg ).
    zcl_zodata_bopf_msg_helper=>raise_on_failed_keys( it_failed_key = lt_failed it_message = lt_msg ).
    IF iv_is_autosave <> abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.
    GET TIME STAMP FIELD lv_now_ts.
    ls_root = read_root_row( is_request-root-pcct_uuid ).
    lv_request_id = |{ zcl_zodata_contract_constants=>c_request_id_prefix_save }-{ sy-datum }{ sy-uzeit }|.
    mo_contract->fill_save_response( EXPORTING iv_pcct_uuid = is_request-root-pcct_uuid iv_changed_on = COND #( WHEN ls_root-changed_on IS INITIAL THEN lv_now_ts ELSE ls_root-changed_on ) iv_version_number = ls_root-version_number iv_is_autosave = iv_is_autosave iv_no_changes = abap_false iv_code = zcl_zodata_contract_constants=>c_code_lock_ok iv_reason_code = zcl_zodata_contract_constants=>c_reason_saved iv_lock_refreshed = abap_true iv_lock_expires = ls_root-lock_expires_at iv_server_now = lv_now_ts iv_request_id = lv_request_id CHANGING cs_result = rs_response ).
    LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).
      DATA(lv_msg_text) = <ls_msg>-message->if_message~get_text( ).
      APPEND VALUE #( msg_type = <ls_msg>-message->if_message~get_message_attributes( )-msg_type msg_text = lv_msg_text ) TO rs_response-messages.
    ENDLOOP.
  ENDMETHOD.
  METHOD validate_save_request.
    " Canonical validation texts retained for invariant tests:
    " SaveChanges: checks[].edit_mode is required.
    " SaveChanges: barriers[].edit_mode is required.
    " SaveChanges: participants[].edit_mode is required.
    " SaveChanges: attachments[].edit_mode is required.
    IF is_request-root-pcct_uuid IS INITIAL. raise_busi_exception( iv_text = zcl_zodata_contract_constants=>c_msg_save_root_required iv_code = 'VALIDATION_ERROR' ). ENDIF.
    IF is_request-session_guid IS INITIAL. raise_busi_exception( iv_text = zcl_zodata_contract_constants=>c_msg_save_session_required iv_code = 'VALIDATION_ERROR' ). ENDIF.
    IF is_request-root-edit_mode IS NOT INITIAL AND is_request-root-edit_mode <> 'C' AND is_request-root-edit_mode <> 'U' AND is_request-root-edit_mode <> 'D'. raise_busi_exception( iv_text = zcl_zodata_contract_constants=>c_msg_save_root_mode_invalid iv_code = 'VALIDATION_ERROR' ). ENDIF.
    LOOP AT is_request-checks ASSIGNING FIELD-SYMBOL(<ls_check>).
      IF <ls_check>-edit_mode IS INITIAL. raise_busi_exception( iv_text = zcl_zodata_contract_constants=>c_msg_save_checks_mode_required iv_code = 'VALIDATION_ERROR' ). ENDIF.
      IF <ls_check>-edit_mode <> 'C' AND <ls_check>-edit_mode <> 'U' AND <ls_check>-edit_mode <> 'D'. raise_busi_exception( iv_text = zcl_zodata_contract_constants=>c_msg_save_checks_mode_invalid iv_code = 'VALIDATION_ERROR' ). ENDIF.
    ENDLOOP.
    LOOP AT is_request-barriers ASSIGNING FIELD-SYMBOL(<ls_barrier>).
      IF <ls_barrier>-edit_mode IS INITIAL. raise_busi_exception( iv_text = zcl_zodata_contract_constants=>c_msg_save_barriers_mode_required iv_code = 'VALIDATION_ERROR' ). ENDIF.
      IF <ls_barrier>-edit_mode <> 'C' AND <ls_barrier>-edit_mode <> 'U' AND <ls_barrier>-edit_mode <> 'D'. raise_busi_exception( iv_text = zcl_zodata_contract_constants=>c_msg_save_barriers_mode_invalid iv_code = 'VALIDATION_ERROR' ). ENDIF.
    ENDLOOP.
    LOOP AT is_request-participants ASSIGNING FIELD-SYMBOL(<ls_participant>).
      IF <ls_participant>-edit_mode IS INITIAL. raise_busi_exception( iv_text = zcl_zodata_contract_constants=>c_msg_save_participants_mode_required iv_code = 'VALIDATION_ERROR' ). ENDIF.
      IF <ls_participant>-edit_mode <> 'C' AND <ls_participant>-edit_mode <> 'U' AND <ls_participant>-edit_mode <> 'D'. raise_busi_exception( iv_text = zcl_zodata_contract_constants=>c_msg_save_participants_mode_invalid iv_code = 'VALIDATION_ERROR' ). ENDIF.
    ENDLOOP.
    LOOP AT is_request-attachments ASSIGNING FIELD-SYMBOL(<ls_attachment>).
      IF <ls_attachment>-edit_mode IS INITIAL. raise_busi_exception( iv_text = zcl_zodata_contract_constants=>c_msg_save_attachments_mode_required iv_code = 'VALIDATION_ERROR' ). ENDIF.
      IF <ls_attachment>-edit_mode <> 'C' AND <ls_attachment>-edit_mode <> 'U' AND <ls_attachment>-edit_mode <> 'D'. raise_busi_exception( iv_text = zcl_zodata_contract_constants=>c_msg_save_attachments_mode_invalid iv_code = 'VALIDATION_ERROR' ). ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD read_root_key.
    TRY.
        rv_rootkey = zcl_zodata_odata_util=>get_uuid_from_it_key_tab( it_key_tab = it_key_tab iv_name = 'RootKey' ).
      CATCH zcx_zodata_error.
        TRY.
            rv_rootkey = zcl_zodata_odata_util=>get_uuid_from_it_key_tab( it_key_tab = it_key_tab iv_name = 'ObjectUuid' ).
          CATCH zcx_zodata_error INTO DATA(lx_rootkey).
            raise_busi_exception( iv_text = lx_rootkey->get_message_text( ) iv_code = lx_rootkey->get_code( ) ).
        ENDTRY.
    ENDTRY.
  ENDMETHOD.
  METHOD read_root_row.
    SELECT SINGLE pcct_uuid checklist_id lpc lpc_text status integration_flag date_check time_check time_zone equipment bukrs observer_fullname observer_perner observer_position observer_orgunit observed_fullname observed_perner observed_position observed_orgunit location_key location_name location_text changed_on changed_by created_on created_by version_number lock_owner lock_session tab_session_id lock_expires_at FROM ztodata_hdr INTO CORRESPONDING FIELDS OF @rs_root WHERE pcct_uuid = @iv_rootkey.
    IF sy-subrc <> 0. raise_busi_exception( iv_text = |ChecklistRoot not found for key { iv_rootkey }.| iv_code = 'VALIDATION_ERROR' ). ENDIF.
    SELECT COUNT( * ) FROM zpcct_check WHERE pcct_uuid = @iv_rootkey INTO @rs_root-checks_total.
    SELECT COUNT( * ) FROM zpcct_check WHERE pcct_uuid = @iv_rootkey AND result = @abap_true INTO @rs_root-checks_success.
    SELECT COUNT( * ) FROM zpcct_barrier WHERE pcct_uuid = @iv_rootkey INTO @rs_root-barriers_total.
    SELECT COUNT( * ) FROM zpcct_barrier WHERE pcct_uuid = @iv_rootkey AND result = @abap_true INTO @rs_root-barriers_success.
  ENDMETHOD.
  METHOD read_root_rows.
    SELECT pcct_uuid checklist_id lpc lpc_text status integration_flag date_check time_check time_zone equipment bukrs observer_fullname observer_perner observer_position observer_orgunit observed_fullname observed_perner observed_position observed_orgunit location_key location_name location_text changed_on changed_by created_on created_by version_number lock_owner lock_session tab_session_id lock_expires_at FROM ztodata_hdr INTO CORRESPONDING FIELDS OF TABLE @rt_root.
    LOOP AT rt_root ASSIGNING FIELD-SYMBOL(<ls_root>).
      SELECT COUNT( * ) FROM zpcct_check WHERE pcct_uuid = @<ls_root>-pcct_uuid INTO @<ls_root>-checks_total.
      SELECT COUNT( * ) FROM zpcct_check WHERE pcct_uuid = @<ls_root>-pcct_uuid AND result = @abap_true INTO @<ls_root>-checks_success.
      SELECT COUNT( * ) FROM zpcct_barrier WHERE pcct_uuid = @<ls_root>-pcct_uuid INTO @<ls_root>-barriers_total.
      SELECT COUNT( * ) FROM zpcct_barrier WHERE pcct_uuid = @<ls_root>-pcct_uuid AND result = @abap_true INTO @<ls_root>-barriers_success.
    ENDLOOP.
  ENDMETHOD.
  METHOD read_check_rows.
    IF iv_rootkey IS INITIAL.
      SELECT check_uuid AS key_uuid pcct_uuid AS root_key checks_num check_text AS text comment_text result changed_on FROM zpcct_check INTO CORRESPONDING FIELDS OF TABLE @rt_checks.
      RETURN.
    ENDIF.
    SELECT check_uuid AS key_uuid pcct_uuid AS root_key checks_num check_text AS text comment_text result changed_on FROM zpcct_check INTO CORRESPONDING FIELDS OF TABLE @rt_checks WHERE pcct_uuid = @iv_rootkey.
  ENDMETHOD.
  METHOD read_barrier_rows.
    IF iv_rootkey IS INITIAL.
      SELECT barrier_uuid AS key_uuid pcct_uuid AS root_key barriers_num barrier_text AS text comment_text result changed_on FROM zpcct_barrier INTO CORRESPONDING FIELDS OF TABLE @rt_barriers.
      RETURN.
    ENDIF.
    SELECT barrier_uuid AS key_uuid pcct_uuid AS root_key barriers_num barrier_text AS text comment_text result changed_on FROM zpcct_barrier INTO CORRESPONDING FIELDS OF TABLE @rt_barriers WHERE pcct_uuid = @iv_rootkey.
  ENDMETHOD.
  METHOD build_permission_row.
    DATA ls_root TYPE ty_root_row.
    DATA lv_can_view TYPE abap_bool VALUE abap_true.
    DATA lv_can_edit TYPE abap_bool VALUE abap_true.
    DATA lv_can_delete TYPE abap_bool VALUE abap_true.
    DATA lv_can_create TYPE abap_bool VALUE abap_true.
    DATA lv_reason TYPE string VALUE zcl_zodata_contract_constants=>c_code_authorized.
    DATA lv_message TYPE string VALUE zcl_zodata_contract_constants=>c_msg_permission_authorized.
    ls_root = read_root_row( iv_rootkey ).
    IF ls_root-bukrs IS NOT INITIAL.
      AUTHORITY-CHECK OBJECT z_ui5_chkl ID 'ACTVT' FIELD zcl_zodata_contract_constants=>c_op_view ID 'BUKRS' FIELD ls_root-bukrs. lv_can_view = xsdbool( sy-subrc = 0 ).
      AUTHORITY-CHECK OBJECT z_ui5_chkl ID 'ACTVT' FIELD zcl_zodata_contract_constants=>c_op_change ID 'BUKRS' FIELD ls_root-bukrs. lv_can_edit = xsdbool( sy-subrc = 0 ).
      AUTHORITY-CHECK OBJECT z_ui5_chkl ID 'ACTVT' FIELD zcl_zodata_contract_constants=>c_op_delete ID 'BUKRS' FIELD ls_root-bukrs. lv_can_delete = xsdbool( sy-subrc = 0 ).
      AUTHORITY-CHECK OBJECT z_ui5_chkl ID 'ACTVT' FIELD zcl_zodata_contract_constants=>c_op_create ID 'BUKRS' FIELD ls_root-bukrs. lv_can_create = xsdbool( sy-subrc = 0 ).
    ENDIF.
    IF lv_can_view = abap_false. lv_reason = zcl_zodata_contract_constants=>c_code_no_view_auth. lv_message = zcl_zodata_contract_constants=>c_msg_permission_no_view.
    ELSEIF lv_can_edit = abap_false AND lv_can_delete = abap_false. lv_reason = zcl_zodata_contract_constants=>c_code_read_only_auth. lv_message = zcl_zodata_contract_constants=>c_msg_permission_read_only.
    ELSEIF lv_can_edit = abap_false. lv_reason = zcl_zodata_contract_constants=>c_code_no_edit_auth. lv_message = zcl_zodata_contract_constants=>c_msg_permission_no_edit.
    ELSEIF lv_can_delete = abap_false. lv_reason = zcl_zodata_contract_constants=>c_code_no_delete_auth. lv_message = zcl_zodata_contract_constants=>c_msg_permission_no_delete.
    ENDIF.
    mo_contract->fill_permission_result( EXPORTING iv_root_key = iv_rootkey iv_user_id = sy-uname iv_can_create = lv_can_create iv_can_view = lv_can_view iv_can_edit = lv_can_edit iv_can_delete = lv_can_delete iv_reason_code = lv_reason iv_message = lv_message CHANGING cs_result = rs_result ).
  ENDMETHOD.
  METHOD build_current_user_row.
    DATA lv_full_name TYPE string. DATA lv_summary TYPE string. DATA lv_rules TYPE string. DATA lv_csv TYPE string. DATA lv_can_view TYPE abap_bool VALUE abap_false. DATA lv_can_edit TYPE abap_bool VALUE abap_false. DATA lv_can_delete TYPE abap_bool VALUE abap_false.
    lv_full_name = |{ sy-uname }|.
    AUTHORITY-CHECK OBJECT z_ui5_chkl ID 'ACTVT' FIELD zcl_zodata_contract_constants=>c_op_view ID 'BUKRS' DUMMY. lv_can_view = xsdbool( sy-subrc = 0 ).
    AUTHORITY-CHECK OBJECT z_ui5_chkl ID 'ACTVT' FIELD zcl_zodata_contract_constants=>c_op_change ID 'BUKRS' DUMMY. lv_can_edit = xsdbool( sy-subrc = 0 ).
    AUTHORITY-CHECK OBJECT z_ui5_chkl ID 'ACTVT' FIELD zcl_zodata_contract_constants=>c_op_delete ID 'BUKRS' DUMMY. lv_can_delete = xsdbool( sy-subrc = 0 ).
    lv_csv = ``.
    IF lv_can_view = abap_true. lv_csv = zcl_zodata_contract_constants=>c_op_view. ENDIF.
    IF lv_can_edit = abap_true. lv_csv = COND string( WHEN lv_csv IS INITIAL THEN zcl_zodata_contract_constants=>c_op_change ELSE |{ lv_csv },{ zcl_zodata_contract_constants=>c_op_change }| ). ENDIF.
    IF lv_can_delete = abap_true. lv_csv = COND string( WHEN lv_csv IS INITIAL THEN zcl_zodata_contract_constants=>c_op_delete ELSE |{ lv_csv },{ zcl_zodata_contract_constants=>c_op_delete }| ). ENDIF.
    lv_rules = zcl_zodata_contract_constants=>c_permission_rules_empty_json.
    lv_summary = |{ zcl_zodata_contract_constants=>c_msg_permission_summary_prefix } { lv_csv }|.
    mo_contract->fill_current_user( EXPORTING iv_full_name = lv_full_name iv_permissions_csv = lv_csv iv_permission_rules = lv_rules iv_can_view = lv_can_view iv_can_edit = lv_can_edit iv_can_delete = lv_can_delete iv_summary_text = lv_summary CHANGING cs_result = rs_result ).
  ENDMETHOD.
  METHOD build_runtime_settings_row.
    mo_contract->fill_runtime_settings( EXPORTING iv_environment = zcl_zodata_contract_constants=>c_environment_production CHANGING cs_result = rs_result ).
  ENDMETHOD.
  METHOD raise_busi_exception.
    DATA(lo_container) = mo_context->get_message_container( ).
    DATA(lv_message_text) = COND string(
      WHEN iv_code IS INITIAL THEN iv_text
      ELSE |{ iv_code }: { iv_text }| ).
    lo_container->add_message_text_only( iv_msg_type = /iwbep/if_mgw_defines=>gcs_msg_type-error iv_msg_text = lv_message_text ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception EXPORTING message_container = lo_container.
  ENDMETHOD.
  METHOD raise_crud_not_allowed.
    raise_busi_exception(
      iv_text = |{ iv_context }: { zcl_zodata_contract_constants=>c_msg_crud_not_allowed }|
      iv_code = 'TECHNICAL_ERROR' ).
  ENDMETHOD.
ENDCLASS.
