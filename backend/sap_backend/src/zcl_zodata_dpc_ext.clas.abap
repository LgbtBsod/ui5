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
    METHODS createchecklist_create_entity REDEFINITION.
    METHODS copychecklist_create_entity REDEFINITION.
    METHODS analyticsrefreshtrigger_create_entity REDEFINITION.
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
    DATA mo_frontend_context TYPE REF TO zcl_zodata_frontend_context_svc.
    DATA mo_mpl_service TYPE REF TO zcl_zodata_mpl_service.
    METHODS ensure_deps.
    METHODS get_srv_mgr RETURNING VALUE(ro_srv_mgr) TYPE REF TO /bobf/if_tra_service_manager.
    METHODS execute_save IMPORTING is_request TYPE zstr_pcct_savechanges_rq iv_is_autosave TYPE abap_bool DEFAULT abap_false RETURNING VALUE(rs_response) TYPE zstr_pcct_savechanges_rs RAISING /iwbep/cx_mgw_busi_exception zcx_zodata_error.
    METHODS read_save_request IMPORTING io_data_provider TYPE REF TO /iwbep/if_mgw_entry_provider RETURNING VALUE(rs_request) TYPE zstr_pcct_savechanges_rq.
    METHODS execute_function_save IMPORTING io_data_provider TYPE REF TO /iwbep/if_mgw_entry_provider iv_is_autosave TYPE abap_bool DEFAULT abap_false RETURNING VALUE(rs_response) TYPE zstr_pcct_savechanges_rs RAISING /iwbep/cx_mgw_busi_exception zcx_zodata_error.
    METHODS build_copy_request IMPORTING is_request TYPE zstr_pcct_savechanges_rq RETURNING VALUE(rs_request) TYPE zstr_pcct_savechanges_rq RAISING /iwbep/cx_mgw_busi_exception.
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

  METHOD createchecklist_create_entity.
    DATA ls_req TYPE zstr_pcct_savechanges_rq.
    DATA ls_resp TYPE zstr_pcct_savechanges_rs.
    AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist
      ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_create
      ID 'BUKRS' FIELD ''.
    IF sy-subrc <> 0.
      raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_no_create_auth iv_code = zif_zodata_contract_constants=>c_code_no_create_auth ).
    ENDIF.
    ensure_deps( ).
    ls_req = read_save_request( io_data_provider ).
    ls_req-root-edit_mode = 'C'.
    TRY.
        ls_resp = execute_save( EXPORTING is_request = ls_req iv_is_autosave = abap_false ).
        copy_data_to_ref( EXPORTING is_data = ls_resp CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_create).
        raise_busi_exception( iv_text = lx_create->get_message_text( ) iv_code = lx_create->get_code( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD copychecklist_create_entity.
    DATA ls_req TYPE zstr_pcct_savechanges_rq.
    DATA ls_resp TYPE zstr_pcct_savechanges_rs.
    ls_req = build_copy_request( read_save_request( io_data_provider ) ).
    TRY.
        ls_resp = execute_save( EXPORTING is_request = ls_req iv_is_autosave = abap_false ).
        copy_data_to_ref( EXPORTING is_data = ls_resp CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_copy).
        raise_busi_exception( iv_text = lx_copy->get_message_text( ) iv_code = lx_copy->get_code( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD analyticsrefreshtrigger_create_entity.
    " AnalyticsRefreshTrigger: enqueue background refresh job
    " Implementation: insert/update ZTODATA_ANALYTICS_REFRESH task row
    DATA lv_now TYPE timestampl.
    GET TIME STAMP FIELD lv_now.
    UPDATE ztodata_hdr SET last_touch_at = lv_now last_touch_by = sy-uname
      WHERE bo_key = zif_i_bo_c=>sc_bo_key AND object_id = sy-uname.
    " Return a minimal FunctionResult acknowledging the trigger
    DATA ls_result TYPE zstr_pcct_savechanges_rs.
    ls_result-ok = abap_true.
    ls_result-success = abap_true.
    ls_result-reason_code = zif_zodata_contract_constants=>c_msg_analytics_triggered.
    copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
  ENDMETHOD.

ENDCLASS.

CLASS zcl_zodata_dpc_ext IMPLEMENTATION.
  METHOD ensure_deps.
    IF mo_mapper IS INITIAL. mo_mapper = zcl_zodata_bopf_mapper=>create( ). ENDIF.
    IF mo_lock_manager IS INITIAL. mo_lock_manager = NEW zcl_zodata_lock_manager( ). ENDIF.
    IF mo_contract IS INITIAL. mo_contract = NEW zcl_zodata_contract_service( ). ENDIF.
    IF mo_frontend_context IS INITIAL. mo_frontend_context = NEW zcl_zodata_frontend_context_svc( mo_contract ). ENDIF.
    IF mo_mpl_service IS INITIAL. mo_mpl_service = NEW zcl_zodata_mpl_service( ). ENDIF.
  ENDMETHOD.
  METHOD get_srv_mgr.
    ro_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_i_bo_c=>sc_bo_key ).
  ENDMETHOD.
  METHOD read_save_request.
    io_data_provider->read_entry_data( IMPORTING es_data = rs_request ).
  ENDMETHOD.
  METHOD execute_function_save.
    rs_response = execute_save(
      EXPORTING
        is_request     = read_save_request( io_data_provider )
        iv_is_autosave = iv_is_autosave ).
  ENDMETHOD.
  METHOD build_copy_request.
    DATA lv_src_uuid TYPE sysuuid_x16.
    DATA ls_src TYPE ty_root_row.

    ensure_deps( ).
    rs_request = is_request.
    lv_src_uuid = rs_request-root-pcct_uuid.
    ls_src = read_root_row( lv_src_uuid ).
    AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist
      ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_create
      ID 'BUKRS' FIELD ls_src-bukrs.
    IF sy-subrc <> 0.
      raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_no_create_auth_copy iv_code = zif_zodata_contract_constants=>c_code_no_create_auth ).
    ENDIF.
    rs_request-root = CORRESPONDING #( ls_src ).
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
    rs_request-checks = CORRESPONDING #( read_check_rows( lv_src_uuid ) ).
    rs_request-barriers = CORRESPONDING #( read_barrier_rows( lv_src_uuid ) ).
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
    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).
    IF ls_req-object_uuid IS INITIAL OR ls_req-session_guid IS INITIAL.
      raise_busi_exception(
        iv_text = zif_zodata_contract_constants=>c_msg_lock_acquire_required
        iv_code = zif_zodata_contract_constants=>c_code_validation_error ).
    ENDIF.
    " AB-03: authorize EDIT before allowing lock
    DATA(ls_auth_lock) = read_root_row( ls_req-object_uuid ).
    AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist
      ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_change
      ID 'BUKRS' FIELD ls_auth_lock-bukrs.
    IF sy-subrc <> 0.
      raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_no_edit_auth iv_code = zif_zodata_contract_constants=>c_code_no_edit_auth ).
    ENDIF.
    TRY.
        DATA(ls_lock_key) = VALUE zif_zodata_lock_manager=>ty_key( bo_key = zif_i_bo_c=>sc_bo_key object_id = ls_req-object_uuid ).
        mo_lock_manager->acquire(
          EXPORTING
            is_key = ls_lock_key
            is_owner = VALUE zif_zodata_lock_manager=>ty_owner( uname = sy-uname session_guid = ls_req-session_guid tab_session_id = ls_req-tab_session_id )
          CHANGING
            cs_result = ls_result ).
        copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_lock_error).
        raise_busi_exception( iv_text = lx_lock_error->get_message_text( ) iv_code = lx_lock_error->get_code( ) ).
      CATCH zcx_lock_error INTO DATA(lx_lock).
        raise_busi_exception(
          iv_text = |Заблокировано: { lx_lock->user_fullname } (Таб.№ { lx_lock->pernr })|
          iv_code = zif_zodata_contract_constants=>c_code_lock_stolen ).
    ENDTRY.
  ENDMETHOD.
  METHOD lockheartbeat_create_entity.
    DATA ls_req TYPE zstr_pcct_lock_heartbeat_rq.
    DATA ls_result TYPE zstr_pcct_lock_heartbeat_rs.
    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).
    IF ls_req-object_uuid IS INITIAL OR ls_req-session_guid IS INITIAL.
      raise_busi_exception(
        iv_text = zif_zodata_contract_constants=>c_msg_lock_heartbeat_required
        iv_code = zif_zodata_contract_constants=>c_code_validation_error ).
    ENDIF.
    TRY.
        mo_lock_manager->heartbeat(
          EXPORTING
            is_key = VALUE zif_zodata_lock_manager=>ty_key( bo_key = zif_i_bo_c=>sc_bo_key object_id = ls_req-object_uuid )
            iv_session_guid = ls_req-session_guid
          CHANGING
            cs_result = ls_result ).
        copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_heartbeat_error).
        raise_busi_exception( iv_text = lx_heartbeat_error->get_message_text( ) iv_code = lx_heartbeat_error->get_code( ) ).
    ENDTRY.
  ENDMETHOD.
  METHOD lockrelease_create_entity.
    DATA ls_req TYPE zstr_pcct_lock_release_rq.
    DATA ls_result TYPE zstr_pcct_lock_release_rs.
    DATA lv_now_ts TYPE timestampl.
    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).
    IF ls_req-object_uuid IS INITIAL OR ls_req-session_guid IS INITIAL.
      raise_busi_exception(
        iv_text = zif_zodata_contract_constants=>c_msg_lock_release_required
        iv_code = zif_zodata_contract_constants=>c_code_validation_error ).
    ENDIF.
    TRY.
        DATA(ls_lock_key) = VALUE zif_zodata_lock_manager=>ty_key( bo_key = zif_i_bo_c=>sc_bo_key object_id = ls_req-object_uuid ).
        mo_lock_manager->unlock(
          EXPORTING
            is_key          = ls_lock_key
            iv_session_guid = ls_req-session_guid ).
      CATCH zcx_lock_error INTO DATA(lx_release_lock).
        raise_busi_exception(
          iv_text = lx_release_lock->get_text( )
          iv_code = zif_zodata_contract_constants=>c_code_lock_not_owned_by_session ).
      CATCH zcx_zodata_error INTO DATA(lx_release_error).
        raise_busi_exception( iv_text = lx_release_error->get_message_text( ) iv_code = lx_release_error->get_code( ) ).
    ENDTRY.
    COMMIT WORK AND WAIT.
    GET TIME STAMP FIELD lv_now_ts.
    mo_contract->fill_lock_release_result(
      EXPORTING iv_ok = abap_true iv_code = zif_zodata_contract_constants=>c_code_lock_ok iv_object_uuid = ls_req-object_uuid iv_owner_session = ls_req-session_guid iv_server_now = lv_now_ts iv_lock_refreshed = abap_false iv_owner_session_match = abap_true
      CHANGING cs_result = ls_result ).
    copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
  ENDMETHOD.
  METHOD autosave_create_entity.
    DATA ls_resp TYPE zstr_pcct_savechanges_rs.
    ensure_deps( ).
    TRY.
        ls_resp = execute_function_save( io_data_provider = io_data_provider iv_is_autosave = abap_true ).
        copy_data_to_ref( EXPORTING is_data = ls_resp CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_map_autosave).
        raise_busi_exception( iv_text = lx_map_autosave->get_message_text( ) iv_code = lx_map_autosave->get_code( ) ).
    ENDTRY.
  ENDMETHOD.
  METHOD savechanges_create_entity.
    DATA ls_resp TYPE zstr_pcct_savechanges_rs.
    ensure_deps( ).
    TRY.
        ls_resp = execute_function_save( io_data_provider = io_data_provider iv_is_autosave = abap_false ).
        copy_data_to_ref( EXPORTING is_data = ls_resp CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_map_save).
        raise_busi_exception( iv_text = lx_map_save->get_message_text( ) iv_code = lx_map_save->get_code( ) ).
    ENDTRY.
  ENDMETHOD.
  METHOD mpltreeset_get_entityset.
    DATA: lt_tree TYPE ztt_pcct_mpl_tree, lv_date TYPE datum.
    ensure_deps( ).
    lv_date = sy-datum.
    READ TABLE it_filter_select_options ASSIGNING FIELD-SYMBOL(<ls_filter>) WITH KEY property = 'Date'.
    IF sy-subrc = 0 AND <ls_filter>-select_options IS NOT INITIAL.
      lv_date = <ls_filter>-select_options[ 1 ]-low.
    ENDIF.
    TRY.
        lt_tree = mo_mpl_service->read_tree( lv_date ).
      CATCH zcx_zodata_error INTO DATA(lx_mpl_error).
        raise_busi_exception( iv_text = lx_mpl_error->get_message_text( ) iv_code = lx_mpl_error->get_code( ) ).
    ENDTRY.
    copy_data_to_ref( EXPORTING is_data = lt_tree CHANGING cr_data = er_entityset ).
  ENDMETHOD.
  METHOD checklistrootset_get_entity.
    DATA ls_row TYPE ty_root_row.
    ls_row = read_root_row( read_root_key( it_key_tab ) ).
    AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist
      ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_view
      ID 'BUKRS' FIELD ls_row-bukrs.
    IF sy-subrc <> 0.
      raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_permission_no_view iv_code = zif_zodata_contract_constants=>c_code_no_view_auth ).
    ENDIF.
    copy_data_to_ref( EXPORTING is_data = ls_row CHANGING cr_data = er_entity ).
  ENDMETHOD.
  METHOD checklistrootset_get_entityset.
    DATA lt_rows TYPE tt_root_row.
    DATA lt_allowed TYPE tt_root_row.
    lt_rows = read_root_rows( ).
    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<ls_root_row>).
      AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist
        ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_view
        ID 'BUKRS' FIELD <ls_root_row>-bukrs.
      IF sy-subrc = 0.
        APPEND <ls_root_row> TO lt_allowed.
      ENDIF.
    ENDLOOP.
    copy_data_to_ref( EXPORTING is_data = lt_allowed CHANGING cr_data = er_entityset ).
  ENDMETHOD.
  METHOD checklistbasicinfos_get_entity.
    DATA ls_row TYPE ty_root_row.
    ls_row = read_root_row( read_root_key( it_key_tab ) ).
    AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist
      ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_view
      ID 'BUKRS' FIELD ls_row-bukrs.
    IF sy-subrc <> 0.
      raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_permission_no_view iv_code = zif_zodata_contract_constants=>c_code_no_view_auth ).
    ENDIF.
    copy_data_to_ref( EXPORTING is_data = ls_row CHANGING cr_data = er_entity ).
  ENDMETHOD.
  METHOD checklistbasicinfos_get_entityset.
    DATA lt_rows TYPE tt_root_row.
    DATA lt_allowed TYPE tt_root_row.
    lt_rows = read_root_rows( ).
    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<ls_basic_root_row>).
      AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist
        ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_view
        ID 'BUKRS' FIELD <ls_basic_root_row>-bukrs.
      IF sy-subrc = 0.
        APPEND <ls_basic_root_row> TO lt_allowed.
      ENDIF.
    ENDLOOP.
    copy_data_to_ref( EXPORTING is_data = lt_allowed CHANGING cr_data = er_entityset ).
  ENDMETHOD.
  METHOD checklistcheckset_get_entityset.
    DATA lv_rootkey TYPE sysuuid_x16.
    DATA lt_rows TYPE tt_check_row.
    DATA ls_root_auth TYPE ty_root_row.
    TRY. lv_rootkey = read_root_key( it_key_tab ). CATCH /iwbep/cx_mgw_busi_exception. CLEAR lv_rootkey. ENDTRY.
    IF lv_rootkey IS NOT INITIAL.
      ls_root_auth = read_root_row( lv_rootkey ).
      AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist
        ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_view
        ID 'BUKRS' FIELD ls_root_auth-bukrs.
      IF sy-subrc <> 0.
        raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_permission_no_view iv_code = zif_zodata_contract_constants=>c_code_no_view_auth ).
      ENDIF.
    ENDIF.
    lt_rows = read_check_rows( lv_rootkey ).
    copy_data_to_ref( EXPORTING is_data = lt_rows CHANGING cr_data = er_entityset ).
  ENDMETHOD.
  METHOD checklistbarriers_get_entityset.
    DATA lv_rootkey TYPE sysuuid_x16.
    DATA lt_rows TYPE tt_barrier_row.
    DATA ls_root_auth TYPE ty_root_row.
    TRY. lv_rootkey = read_root_key( it_key_tab ). CATCH /iwbep/cx_mgw_busi_exception. CLEAR lv_rootkey. ENDTRY.
    IF lv_rootkey IS NOT INITIAL.
      ls_root_auth = read_root_row( lv_rootkey ).
      AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist
        ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_view
        ID 'BUKRS' FIELD ls_root_auth-bukrs.
      IF sy-subrc <> 0.
        raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_permission_no_view iv_code = zif_zodata_contract_constants=>c_code_no_view_auth ).
      ENDIF.
    ENDIF.
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
    " AB-03: authorize EDIT for every save/autosave
    DATA(ls_auth_save) = read_root_row( is_request-root-pcct_uuid ).
    AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist
      ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_change
      ID 'BUKRS' FIELD ls_auth_save-bukrs.
    IF sy-subrc <> 0.
      raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_no_edit_auth_save iv_code = zif_zodata_contract_constants=>c_code_no_edit_auth ).
    ENDIF.
    TRY.
        mo_lock_manager->heartbeat( EXPORTING is_key = VALUE zif_zodata_lock_manager=>ty_key( bo_key = zif_i_bo_c=>sc_bo_key object_id = is_request-root-pcct_uuid ) iv_session_guid = is_request-session_guid CHANGING cs_result = ls_lock_hb ).
      CATCH zcx_zodata_error INTO DATA(lx_lock_hb_error).
        raise_busi_exception( iv_text = lx_lock_hb_error->get_message_text( ) iv_code = lx_lock_hb_error->get_code( ) ).
    ENDTRY.
    IF ls_lock_hb-success <> abap_true AND ls_lock_hb-ok <> abap_true.
      raise_busi_exception(
        iv_text = zif_zodata_contract_constants=>c_msg_lock_session_required
        iv_code = zif_zodata_contract_constants=>c_code_lock_not_owned_by_session ).
    ENDIF.
    lt_change = mo_mapper->build_change_list( is_request ).
    IF lt_change IS INITIAL.
      GET TIME STAMP FIELD lv_now_ts.
      ls_root = read_root_row( is_request-root-pcct_uuid ).
      lv_request_id = |{ zif_zodata_contract_constants=>c_request_id_prefix_save }-{ sy-datum }{ sy-uzeit }|.
      mo_contract->fill_save_response( EXPORTING iv_pcct_uuid = is_request-root-pcct_uuid iv_changed_on = COND #( WHEN ls_root-changed_on IS INITIAL THEN lv_now_ts ELSE ls_root-changed_on ) iv_version_number = ls_root-version_number iv_is_autosave = iv_is_autosave iv_no_changes = abap_true iv_code = zif_zodata_contract_constants=>c_code_lock_ok iv_reason_code = zif_zodata_contract_constants=>c_reason_no_changes iv_lock_refreshed = abap_true iv_lock_expires = ls_root-lock_expires_at iv_server_now = lv_now_ts iv_request_id = lv_request_id CHANGING cs_result = rs_response ).
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
    lv_request_id = |{ zif_zodata_contract_constants=>c_request_id_prefix_save }-{ sy-datum }{ sy-uzeit }|.
    mo_contract->fill_save_response( EXPORTING iv_pcct_uuid = is_request-root-pcct_uuid iv_changed_on = COND #( WHEN ls_root-changed_on IS INITIAL THEN lv_now_ts ELSE ls_root-changed_on ) iv_version_number = ls_root-version_number iv_is_autosave = iv_is_autosave iv_no_changes = abap_false iv_code = zif_zodata_contract_constants=>c_code_lock_ok iv_reason_code = zif_zodata_contract_constants=>c_reason_saved iv_lock_refreshed = abap_true iv_lock_expires = ls_root-lock_expires_at iv_server_now = lv_now_ts iv_request_id = lv_request_id CHANGING cs_result = rs_response ).
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
    IF is_request-root-pcct_uuid IS INITIAL. raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_root_required iv_code = zif_zodata_contract_constants=>c_code_validation_error ). ENDIF.
    IF is_request-session_guid IS INITIAL. raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_session_required iv_code = zif_zodata_contract_constants=>c_code_validation_error ). ENDIF.
    IF is_request-root-edit_mode IS NOT INITIAL AND is_request-root-edit_mode <> 'C' AND is_request-root-edit_mode <> 'U' AND is_request-root-edit_mode <> 'D'. raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_root_mode_invalid iv_code = zif_zodata_contract_constants=>c_code_validation_error ). ENDIF.
    LOOP AT is_request-checks ASSIGNING FIELD-SYMBOL(<ls_check>).
      IF <ls_check>-edit_mode IS INITIAL. raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_checks_mode_required iv_code = zif_zodata_contract_constants=>c_code_validation_error ). ENDIF.
      IF <ls_check>-edit_mode <> 'C' AND <ls_check>-edit_mode <> 'U' AND <ls_check>-edit_mode <> 'D'. raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_checks_mode_invalid iv_code = zif_zodata_contract_constants=>c_code_validation_error ). ENDIF.
    ENDLOOP.
    LOOP AT is_request-barriers ASSIGNING FIELD-SYMBOL(<ls_barrier>).
      IF <ls_barrier>-edit_mode IS INITIAL. raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_barriers_mode_required iv_code = zif_zodata_contract_constants=>c_code_validation_error ). ENDIF.
      IF <ls_barrier>-edit_mode <> 'C' AND <ls_barrier>-edit_mode <> 'U' AND <ls_barrier>-edit_mode <> 'D'. raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_barriers_mode_invalid iv_code = zif_zodata_contract_constants=>c_code_validation_error ). ENDIF.
    ENDLOOP.
    LOOP AT is_request-participants ASSIGNING FIELD-SYMBOL(<ls_participant>).
      IF <ls_participant>-edit_mode IS INITIAL. raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_participants_mode_required iv_code = zif_zodata_contract_constants=>c_code_validation_error ). ENDIF.
      IF <ls_participant>-edit_mode <> 'C' AND <ls_participant>-edit_mode <> 'U' AND <ls_participant>-edit_mode <> 'D'. raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_participants_mode_invalid iv_code = zif_zodata_contract_constants=>c_code_validation_error ). ENDIF.
    ENDLOOP.
    LOOP AT is_request-attachments ASSIGNING FIELD-SYMBOL(<ls_attachment>).
      IF <ls_attachment>-edit_mode IS INITIAL. raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_attachments_mode_required iv_code = zif_zodata_contract_constants=>c_code_validation_error ). ENDIF.
      IF <ls_attachment>-edit_mode <> 'C' AND <ls_attachment>-edit_mode <> 'U' AND <ls_attachment>-edit_mode <> 'D'. raise_busi_exception( iv_text = zif_zodata_contract_constants=>c_msg_save_attachments_mode_invalid iv_code = zif_zodata_contract_constants=>c_code_validation_error ). ENDIF.
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
    TYPES: BEGIN OF ty_check_agg,
      pcct_uuid TYPE sysuuid_x16,
      total TYPE int4,
      success TYPE int4,
    END OF ty_check_agg.
    TYPES: BEGIN OF ty_barrier_agg,
      pcct_uuid TYPE sysuuid_x16,
      total TYPE int4,
      success TYPE int4,
    END OF ty_barrier_agg.
    DATA ls_check_agg TYPE ty_check_agg.
    DATA ls_barrier_agg TYPE ty_barrier_agg.
    SELECT SINGLE pcct_uuid checklist_id lpc lpc_text status integration_flag date_check time_check time_zone equipment bukrs observer_fullname observer_perner observer_position observer_orgunit observed_fullname observed_perner observed_position observed_orgunit location_key location_name location_text changed_on changed_by created_on created_by version_number lock_owner lock_session tab_session_id lock_expires_at FROM ztodata_hdr INTO CORRESPONDING FIELDS OF @rs_root WHERE pcct_uuid = @iv_rootkey.
    IF sy-subrc <> 0. raise_busi_exception( iv_text = |ChecklistRoot not found for key { iv_rootkey }.| iv_code = zif_zodata_contract_constants=>c_code_validation_error ). ENDIF.
    SELECT SINGLE pcct_uuid,
                  COUNT( * ) AS total,
                  SUM( CASE WHEN result = @abap_true THEN 1 ELSE 0 END ) AS success
      FROM zpcct_check
      WHERE pcct_uuid = @iv_rootkey
      GROUP BY pcct_uuid
      INTO @ls_check_agg.
    IF sy-subrc = 0.
      rs_root-checks_total = ls_check_agg-total.
      rs_root-checks_success = ls_check_agg-success.
    ENDIF.
    SELECT SINGLE pcct_uuid,
                  COUNT( * ) AS total,
                  SUM( CASE WHEN result = @abap_true THEN 1 ELSE 0 END ) AS success
      FROM zpcct_barrier
      WHERE pcct_uuid = @iv_rootkey
      GROUP BY pcct_uuid
      INTO @ls_barrier_agg.
    IF sy-subrc = 0.
      rs_root-barriers_total = ls_barrier_agg-total.
      rs_root-barriers_success = ls_barrier_agg-success.
    ENDIF.
  ENDMETHOD.
  METHOD read_root_rows.
    TYPES: BEGIN OF ty_check_agg,
      pcct_uuid TYPE sysuuid_x16,
      total TYPE int4,
      success TYPE int4,
    END OF ty_check_agg,
    tt_check_agg TYPE HASHED TABLE OF ty_check_agg WITH UNIQUE KEY pcct_uuid.
    TYPES: BEGIN OF ty_barrier_agg,
      pcct_uuid TYPE sysuuid_x16,
      total TYPE int4,
      success TYPE int4,
    END OF ty_barrier_agg,
    tt_barrier_agg TYPE HASHED TABLE OF ty_barrier_agg WITH UNIQUE KEY pcct_uuid.
    DATA lt_check_agg TYPE tt_check_agg.
    DATA lt_barrier_agg TYPE tt_barrier_agg.
    SELECT pcct_uuid checklist_id lpc lpc_text status integration_flag date_check time_check time_zone equipment bukrs observer_fullname observer_perner observer_position observer_orgunit observed_fullname observed_perner observed_position observed_orgunit location_key location_name location_text changed_on changed_by created_on created_by version_number lock_owner lock_session tab_session_id lock_expires_at FROM ztodata_hdr INTO CORRESPONDING FIELDS OF TABLE @rt_root.
    IF rt_root IS INITIAL.
      RETURN.
    ENDIF.
    SELECT pcct_uuid,
           COUNT( * ) AS total,
           SUM( CASE WHEN result = @abap_true THEN 1 ELSE 0 END ) AS success
      FROM zpcct_check
      FOR ALL ENTRIES IN @rt_root
      WHERE pcct_uuid = @rt_root-pcct_uuid
      GROUP BY pcct_uuid
      INTO TABLE @lt_check_agg.
    SELECT pcct_uuid,
           COUNT( * ) AS total,
           SUM( CASE WHEN result = @abap_true THEN 1 ELSE 0 END ) AS success
      FROM zpcct_barrier
      FOR ALL ENTRIES IN @rt_root
      WHERE pcct_uuid = @rt_root-pcct_uuid
      GROUP BY pcct_uuid
      INTO TABLE @lt_barrier_agg.
    LOOP AT rt_root ASSIGNING FIELD-SYMBOL(<ls_root>).
      READ TABLE lt_check_agg ASSIGNING FIELD-SYMBOL(<ls_check_agg>) WITH TABLE KEY pcct_uuid = <ls_root>-pcct_uuid.
      IF sy-subrc = 0.
        <ls_root>-checks_total = <ls_check_agg>-total.
        <ls_root>-checks_success = <ls_check_agg>-success.
      ENDIF.
      READ TABLE lt_barrier_agg ASSIGNING FIELD-SYMBOL(<ls_barrier_agg>) WITH TABLE KEY pcct_uuid = <ls_root>-pcct_uuid.
      IF sy-subrc = 0.
        <ls_root>-barriers_total = <ls_barrier_agg>-total.
        <ls_root>-barriers_success = <ls_barrier_agg>-success.
      ENDIF.
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
    ensure_deps( ).
    DATA(ls_root) = read_root_row( iv_rootkey ).
    rs_result = mo_frontend_context->build_permission_result(
      iv_root_key = iv_rootkey
      iv_bukrs    = ls_root-bukrs ).
  ENDMETHOD.
  METHOD build_current_user_row.
    ensure_deps( ).
    rs_result = mo_frontend_context->build_current_user_result( ).
  ENDMETHOD.
  METHOD build_runtime_settings_row.
    ensure_deps( ).
    rs_result = mo_frontend_context->build_runtime_settings_result(
      iv_environment          = zif_zodata_contract_constants=>c_environment_production
      iv_heartbeat_ms         = zif_zodata_contract_constants=>c_heartbeat_ms
      iv_idle_ms              = zif_zodata_contract_constants=>c_idle_ms
      iv_autosave_interval_ms = zif_zodata_contract_constants=>c_autosave_ms
      iv_lock_refresh_ms      = zif_zodata_contract_constants=>c_lock_refresh_cooldown_ms
      iv_analytics_refresh_ms = zif_zodata_contract_constants=>c_analytics_refresh_ms
      iv_gcd_interval_ms      = zif_zodata_contract_constants=>c_gcd_interval_ms
      iv_network_grace_ms     = zif_zodata_contract_constants=>c_network_grace_ms
      iv_cache_tolerance_ms   = zif_zodata_contract_constants=>c_cache_tolerance_ms ).
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
      iv_text = |{ iv_context }: { zif_zodata_contract_constants=>c_msg_crud_not_allowed }|
      iv_code = zif_zodata_contract_constants=>c_code_technical_error ).
  ENDMETHOD.
ENDCLASS.
