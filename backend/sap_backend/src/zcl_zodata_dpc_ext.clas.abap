CLASS zcl_zodata_dpc_ext DEFINITION PUBLIC INHERITING FROM zcl_zodata_dpc CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor REDEFINITION.
    METHODS checklistrootset_get_entity REDEFINITION.
    METHODS checklistrootset_get_entityset REDEFINITION.
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
    METHODS lockstatusset_get_entity REDEFINITION.
    METHODS autosave_create_entity REDEFINITION.
    METHODS savechanges_create_entity REDEFINITION.
    METHODS createchecklist_create_entity REDEFINITION.
    METHODS copychecklist_create_entity REDEFINITION.
    METHODS analyticsrefreshtrigger_create_entity REDEFINITION.
    METHODS mpltreeset_get_entityset REDEFINITION.

  PRIVATE SECTION.
    DATA mo_lock_manager TYPE REF TO zif_zodata_lock_manager.
    DATA mo_mapper TYPE REF TO zif_zodata_bopf_mapper.
    DATA mo_contract TYPE REF TO zcl_zodata_contract_service.
    DATA mo_frontend_context TYPE REF TO zcl_zodata_frontend_context_svc.
    DATA mo_mpl_service TYPE REF TO zcl_zodata_mpl_service.
    DATA mo_runtime_settings TYPE REF TO zcl_zodata_runtime_settings_svc.
    DATA mo_read_service TYPE REF TO zcl_zodata_read_service.
    DATA mo_save_service TYPE REF TO zcl_zodata_save_service.
    DATA mv_deps_initialized TYPE abap_bool VALUE abap_false.

    METHODS init_deps.
    METHODS ensure_deps.
    METHODS get_srv_mgr RETURNING VALUE(ro_srv_mgr) TYPE REF TO /bobf/if_tra_service_manager.
    METHODS has_checklist_authority
      IMPORTING
        iv_activity          TYPE string
        iv_bukrs             TYPE bukrs
      RETURNING
        VALUE(rv_authorized) TYPE abap_bool.
    METHODS require_checklist_authority
      IMPORTING
        iv_activity TYPE string
        iv_bukrs    TYPE bukrs
        iv_text     TYPE string
        iv_code     TYPE string
      RAISING /iwbep/cx_mgw_busi_exception.
    METHODS filter_view_authorized_root_rows
      IMPORTING
        it_rows           TYPE zcl_zodata_read_service=>tt_root_row
      RETURNING
        VALUE(rt_allowed) TYPE zcl_zodata_read_service=>tt_root_row.
    METHODS read_save_request
      IMPORTING io_data_provider TYPE REF TO /iwbep/if_mgw_entry_provider
      RETURNING VALUE(rs_request) TYPE zstr_pcct_savechanges_rq.
    METHODS execute_function_save
      IMPORTING
        io_data_provider TYPE REF TO /iwbep/if_mgw_entry_provider
        iv_is_autosave   TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rs_response) TYPE zstr_pcct_savechanges_rs
      RAISING /iwbep/cx_mgw_busi_exception zcx_zodata_error.
    METHODS build_copy_request
      IMPORTING
        is_request TYPE zstr_pcct_savechanges_rq
      RETURNING VALUE(rs_request) TYPE zstr_pcct_savechanges_rq
      RAISING /iwbep/cx_mgw_busi_exception.
    METHODS raise_busi_exception
      IMPORTING
        iv_text TYPE string
        iv_code TYPE string OPTIONAL
      RAISING /iwbep/cx_mgw_busi_exception.
    METHODS raise_crud_not_allowed
      IMPORTING
        iv_context TYPE string
      RAISING /iwbep/cx_mgw_busi_exception.
    METHODS resolve_lock_db_key
      IMPORTING
        is_lock_request TYPE any
      RETURNING VALUE(rv_db_key) TYPE sysuuid_x16.
    METHODS read_db_key
      IMPORTING
        it_key_tab TYPE /iwbep/t_mgw_name_value_pair
      RETURNING VALUE(rv_db_key) TYPE sysuuid_x16
      RAISING /iwbep/cx_mgw_busi_exception.
    METHODS build_permission_row
      IMPORTING
        iv_db_key TYPE sysuuid_x16
      RETURNING VALUE(rs_result) TYPE zstr_pcct_permission_rs.
    METHODS build_current_user_row
      RETURNING VALUE(rs_result) TYPE zstr_pcct_current_user_rs.
    METHODS build_runtime_settings_row
      RETURNING VALUE(rs_result) TYPE zstr_pcct_runtime_settings_rs.
ENDCLASS.

CLASS zcl_zodata_dpc_ext IMPLEMENTATION.
  METHOD constructor.
    " mo_context is injected by the /IWBEP framework after construction.
    " Only initialize context-free collaborators here.
    super->constructor( ).
    mo_mapper = zcl_zodata_bopf_mapper=>create( ).
    mo_lock_manager = NEW zcl_zodata_lock_manager( ).
    mo_contract = NEW zcl_zodata_contract_service( ).
    mo_frontend_context = NEW zcl_zodata_frontend_context_svc( mo_contract ).
    mo_mpl_service = NEW zcl_zodata_mpl_service( ).
    mo_runtime_settings = NEW zcl_zodata_runtime_settings_svc( ).
  ENDMETHOD.

  METHOD init_deps.
    " Called from ensure_deps when mo_context is already live.
    mo_read_service = NEW zcl_zodata_read_service( mo_context ).
    mo_save_service = NEW zcl_zodata_save_service(
      io_mapper       = mo_mapper
      io_lock_manager = mo_lock_manager
      io_contract     = mo_contract
      io_srv_mgr      = get_srv_mgr( ) ).
  ENDMETHOD.

  METHOD ensure_deps.
    IF mv_deps_initialized = abap_true.
      RETURN.
    ENDIF.
    IF mo_mapper IS INITIAL. mo_mapper = zcl_zodata_bopf_mapper=>create( ). ENDIF.
    IF mo_lock_manager IS INITIAL. mo_lock_manager = NEW zcl_zodata_lock_manager( ). ENDIF.
    IF mo_contract IS INITIAL. mo_contract = NEW zcl_zodata_contract_service( ). ENDIF.
    IF mo_frontend_context IS INITIAL. mo_frontend_context = NEW zcl_zodata_frontend_context_svc( mo_contract ). ENDIF.
    IF mo_mpl_service IS INITIAL. mo_mpl_service = NEW zcl_zodata_mpl_service( ). ENDIF.
    IF mo_runtime_settings IS INITIAL. mo_runtime_settings = NEW zcl_zodata_runtime_settings_svc( ). ENDIF.
    IF mo_read_service IS INITIAL OR mo_save_service IS INITIAL.
      init_deps( ).
    ENDIF.
    mv_deps_initialized = abap_true.
  ENDMETHOD.

  METHOD get_srv_mgr.
    ro_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_i_bo_c=>sc_bo_key ).
  ENDMETHOD.

  METHOD has_checklist_authority.
    AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist
      ID 'ACTVT' FIELD iv_activity
      ID 'BUKRS' FIELD iv_bukrs.
    rv_authorized = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD require_checklist_authority.
    IF has_checklist_authority(
         iv_activity = iv_activity
         iv_bukrs    = iv_bukrs ) = abap_false.
      raise_busi_exception(
        iv_text = iv_text
        iv_code = iv_code ).
    ENDIF.
  ENDMETHOD.

  METHOD filter_view_authorized_root_rows.
    LOOP AT it_rows ASSIGNING FIELD-SYMBOL(<ls_root_row>).
      IF has_checklist_authority(
           iv_activity = zif_zodata_contract_constants=>c_op_view
           iv_bukrs    = <ls_root_row>-bukrs ) = abap_true.
        APPEND <ls_root_row> TO rt_allowed.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_save_request.
    io_data_provider->read_entry_data( IMPORTING es_data = rs_request ).
  ENDMETHOD.

  METHOD execute_function_save.
    ensure_deps( ).
    DATA(ls_request) = read_save_request( io_data_provider ).
    DATA(ls_root) = mo_read_service->read_root_row( ls_request-root-key ).
    rs_response = mo_save_service->execute_save(
      is_request        = ls_request
      iv_is_autosave    = iv_is_autosave
      iv_bukrs          = ls_root-bukrs
      iv_changed_on     = ls_root-changed_on
      iv_version_number = ls_root-version_number
      iv_lock_expires   = ls_root-lock_expires_at ).
  ENDMETHOD.

  METHOD build_copy_request.
    ensure_deps( ).
    DATA(ls_source_root) = mo_read_service->read_root_row( is_request-root-key ).
    require_checklist_authority(
      iv_activity = zif_zodata_contract_constants=>c_op_create
      iv_bukrs    = ls_source_root-bukrs
        iv_text     = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_no_create_auth_copy )
      iv_code     = zif_zodata_message_codes=>no_create_auth ).
    rs_request = mo_save_service->build_copy_request(
      is_request     = is_request
      is_source_root = ls_source_root
      it_checks      = mo_read_service->read_check_rows( is_request-root-key )
      it_barriers    = mo_read_service->read_barrier_rows( is_request-root-key ) ).
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

  METHOD createchecklist_create_entity.
    DATA ls_req TYPE zstr_pcct_savechanges_rq.
    DATA ls_resp TYPE zstr_pcct_savechanges_rs.
    ensure_deps( ).
    ls_req = read_save_request( io_data_provider ).
    ls_req-root-edit_mode = 'C'.
    AUTHORITY-CHECK OBJECT zif_zodata_contract_constants=>c_auth_object_checklist
      ID 'ACTVT' FIELD zif_zodata_contract_constants=>c_op_create
      ID 'BUKRS' FIELD ls_req-root-bukrs.
    IF sy-subrc <> 0.
      raise_busi_exception( iv_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_no_create_auth ) iv_code = zif_zodata_message_codes=>no_create_auth ).
    ENDIF.
    TRY.
        ls_resp = mo_save_service->execute_save(
          is_request        = ls_req
          iv_is_autosave    = abap_false
          iv_bukrs          = ''
          iv_changed_on     = CONV timestampl( 0 )
          iv_version_number = 0
          iv_lock_expires   = CONV timestampl( 0 ) ).
        copy_data_to_ref( EXPORTING is_data = ls_resp CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_create).
        raise_busi_exception( iv_text = lx_create->get_message_text( ) iv_code = lx_create->get_code( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD copychecklist_create_entity.
    DATA ls_resp TYPE zstr_pcct_savechanges_rs.
    DATA ls_req TYPE zstr_pcct_savechanges_rq.
    DATA ls_root TYPE zcl_zodata_read_service=>ty_root_row.
    ensure_deps( ).
    ls_req = build_copy_request( read_save_request( io_data_provider ) ).
    ls_root = mo_read_service->read_root_row( ls_req-root-key ).
    TRY.
        ls_resp = mo_save_service->execute_save(
          is_request        = ls_req
          iv_is_autosave    = abap_false
          iv_bukrs          = ls_root-bukrs
          iv_changed_on     = ls_root-changed_on
          iv_version_number = ls_root-version_number
          iv_lock_expires   = ls_root-lock_expires_at ).
        copy_data_to_ref( EXPORTING is_data = ls_resp CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_copy).
        raise_busi_exception( iv_text = lx_copy->get_message_text( ) iv_code = lx_copy->get_code( ) ).
    ENDTRY.
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

  METHOD analyticsrefreshtrigger_create_entity.
    DATA ls_result TYPE zstr_pcct_savechanges_rs.
    GET TIME STAMP FIELD ls_result-changed_on.
    ls_result-ok = abap_true.
    ls_result-success = abap_true.
    ls_result-reason_code = zif_zodata_message_codes=>triggered.
    copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
  ENDMETHOD.

  METHOD lockacquire_create_entity.
    DATA ls_req TYPE zstr_pcct_lock_acquire_rq.
    DATA ls_result TYPE zstr_pcct_lock_acquire_rs.
    DATA lv_db_key TYPE sysuuid_x16.
    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).
    lv_db_key = ls_req-db_key.
    IF lv_db_key IS INITIAL OR ls_req-session_guid IS INITIAL.
      raise_busi_exception( iv_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_lock_acquire_required ) iv_code = zif_zodata_message_codes=>validation_error ).
    ENDIF.
    DATA(ls_auth_lock) = mo_read_service->read_root_row( lv_db_key ).
    require_checklist_authority(
      iv_activity = zif_zodata_contract_constants=>c_op_change
      iv_bukrs    = ls_auth_lock-bukrs
        iv_text     = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_no_edit_auth )
      iv_code     = zif_zodata_message_codes=>no_edit_auth ).
    TRY.
        mo_lock_manager->acquire(
          EXPORTING
            is_key   = VALUE zif_zodata_lock_manager=>ty_key( bo_key = zif_i_bo_c=>sc_bo_key object_id = lv_db_key )
            is_owner = VALUE zif_zodata_lock_manager=>ty_owner( uname = sy-uname session_guid = ls_req-session_guid tab_session_id = ls_req-tab_session_id )
            iv_force_takeover = xsdbool( ls_req-force_takeover = abap_true )
          CHANGING
            cs_result = ls_result ).
        copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_lock_error).
        raise_busi_exception( iv_text = lx_lock_error->get_message_text( ) iv_code = lx_lock_error->get_code( ) ).
      CATCH zcx_lock_error INTO DATA(lx_lock).
        raise_busi_exception( iv_text = lx_lock->get_text( ) iv_code = zif_zodata_message_codes=>lock_stolen ).
    ENDTRY.
  ENDMETHOD.

  METHOD lockheartbeat_create_entity.
    DATA ls_req TYPE zstr_pcct_lock_heartbeat_rq.
    DATA ls_result TYPE zstr_pcct_lock_heartbeat_rs.
    DATA lv_db_key TYPE sysuuid_x16.
    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).
    lv_db_key = ls_req-db_key.
    IF lv_db_key IS INITIAL OR ls_req-session_guid IS INITIAL.
      raise_busi_exception( iv_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_lock_heartbeat_required ) iv_code = zif_zodata_message_codes=>validation_error ).
    ENDIF.
    TRY.
        mo_lock_manager->heartbeat(
          EXPORTING
            is_key          = VALUE zif_zodata_lock_manager=>ty_key( bo_key = zif_i_bo_c=>sc_bo_key object_id = lv_db_key )
            iv_session_guid = ls_req-session_guid
          CHANGING
            cs_result       = ls_result ).
        copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_heartbeat_error).
        raise_busi_exception( iv_text = lx_heartbeat_error->get_message_text( ) iv_code = lx_heartbeat_error->get_code( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD lockrelease_create_entity.
    DATA ls_req TYPE zstr_pcct_lock_release_rq.
    DATA ls_result TYPE zstr_pcct_lock_release_rs.
    DATA lv_now_ts TYPE timestampl.
    DATA lv_db_key TYPE sysuuid_x16.
    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).
    lv_db_key = ls_req-db_key.
    IF lv_db_key IS INITIAL OR ls_req-session_guid IS INITIAL.
      raise_busi_exception( iv_text = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_lock_release_required ) iv_code = zif_zodata_message_codes=>validation_error ).
    ENDIF.
    TRY.
        mo_lock_manager->unlock(
          EXPORTING
            is_key          = VALUE zif_zodata_lock_manager=>ty_key( bo_key = zif_i_bo_c=>sc_bo_key object_id = lv_db_key )
            iv_session_guid = ls_req-session_guid ).
      CATCH zcx_lock_error INTO DATA(lx_release_lock).
        raise_busi_exception( iv_text = lx_release_lock->get_text( ) iv_code = zif_zodata_message_codes=>lock_not_owned_by_session ).
      CATCH zcx_zodata_error INTO DATA(lx_release_error).
        raise_busi_exception( iv_text = lx_release_error->get_message_text( ) iv_code = lx_release_error->get_code( ) ).
    ENDTRY.
    GET TIME STAMP FIELD lv_now_ts.
    mo_contract->fill_lock_release_result(
      EXPORTING
        iv_ok                  = abap_true
        iv_code                = zif_zodata_message_codes=>lock_ok
        iv_db_key              = lv_db_key
        iv_owner_session       = ls_req-session_guid
        iv_server_now          = lv_now_ts
        iv_lock_refreshed      = abap_false
        iv_owner_session_match = abap_true
      CHANGING
        cs_result              = ls_result ).
    copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
  ENDMETHOD.

  METHOD lockstatusset_get_entity.
    DATA ls_result TYPE zstr_pcct_lock_acquire_rs.
    DATA lv_db_key TYPE sysuuid_x16.
    DATA lv_session_guid TYPE string.
    ensure_deps( ).
    lv_db_key = read_db_key( it_key_tab ).
    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<ls_session_guid>) WITH KEY name = 'SessionGuid'.
    IF sy-subrc = 0.
      lv_session_guid = <ls_session_guid>-value.
    ENDIF.
    TRY.
        mo_lock_manager->status(
          EXPORTING
            is_key          = VALUE zif_zodata_lock_manager=>ty_key( bo_key = zif_i_bo_c=>sc_bo_key object_id = lv_db_key )
            iv_session_guid = lv_session_guid
          CHANGING
            cs_result       = ls_result ).
        copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
      CATCH zcx_zodata_error INTO DATA(lx_status_error).
        raise_busi_exception( iv_text = lx_status_error->get_message_text( ) iv_code = lx_status_error->get_code( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD mpltreeset_get_entityset.
    DATA lt_tree TYPE ztt_pcct_mpl_tree.
    DATA lv_date TYPE datum.
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
    DATA ls_row TYPE zcl_zodata_read_service=>ty_root_row.
    ensure_deps( ).
    ls_row = mo_read_service->read_root_row( read_db_key( it_key_tab ) ).
    require_checklist_authority(
      iv_activity = zif_zodata_contract_constants=>c_op_view
      iv_bukrs    = ls_row-bukrs
        iv_text     = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_permission_no_view )
      iv_code     = zif_zodata_message_codes=>no_view_auth ).
    copy_data_to_ref( EXPORTING is_data = ls_row CHANGING cr_data = er_entity ).
  ENDMETHOD.

  METHOD checklistrootset_get_entityset.
    DATA lt_rows TYPE zcl_zodata_read_service=>tt_root_row.
    ensure_deps( ).
    lt_rows = mo_read_service->read_root_rows( ).
    copy_data_to_ref(
      EXPORTING
        is_data = filter_view_authorized_root_rows( lt_rows )
      CHANGING
        cr_data = er_entityset ).
  ENDMETHOD.

  METHOD checklistcheckset_get_entityset.
    DATA lv_db_key TYPE sysuuid_x16.
    DATA lt_rows TYPE zcl_zodata_read_service=>tt_check_row.
    DATA ls_root_auth TYPE zcl_zodata_read_service=>ty_root_row.
    ensure_deps( ).
    TRY. lv_db_key = read_db_key( it_key_tab ). CATCH /iwbep/cx_mgw_busi_exception. CLEAR lv_db_key. ENDTRY.
    IF lv_db_key IS INITIAL.
      RETURN.
    ENDIF.
    ls_root_auth = mo_read_service->read_root_row( lv_db_key ).
    require_checklist_authority(
      iv_activity = zif_zodata_contract_constants=>c_op_view
      iv_bukrs    = ls_root_auth-bukrs
        iv_text     = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_permission_no_view )
      iv_code     = zif_zodata_message_codes=>no_view_auth ).
    lt_rows = mo_read_service->read_check_rows( lv_db_key ).
    copy_data_to_ref( EXPORTING is_data = lt_rows CHANGING cr_data = er_entityset ).
  ENDMETHOD.

  METHOD checklistbarriers_get_entityset.
    DATA lv_db_key TYPE sysuuid_x16.
    DATA lt_rows TYPE zcl_zodata_read_service=>tt_barrier_row.
    DATA ls_root_auth TYPE zcl_zodata_read_service=>ty_root_row.
    ensure_deps( ).
    TRY. lv_db_key = read_db_key( it_key_tab ). CATCH /iwbep/cx_mgw_busi_exception. CLEAR lv_db_key. ENDTRY.
    IF lv_db_key IS INITIAL.
      RETURN.
    ENDIF.
    ls_root_auth = mo_read_service->read_root_row( lv_db_key ).
    require_checklist_authority(
      iv_activity = zif_zodata_contract_constants=>c_op_view
      iv_bukrs    = ls_root_auth-bukrs
        iv_text     = zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_permission_no_view )
      iv_code     = zif_zodata_message_codes=>no_view_auth ).
    lt_rows = mo_read_service->read_barrier_rows( lv_db_key ).
    copy_data_to_ref( EXPORTING is_data = lt_rows CHANGING cr_data = er_entityset ).
  ENDMETHOD.

  METHOD checklistpermissions_get_entity.
    DATA ls_result TYPE zstr_pcct_permission_rs.
    ls_result = build_permission_row( read_db_key( it_key_tab ) ).
    copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
  ENDMETHOD.

  METHOD checklistpermissions_get_entityset.
    DATA lt_root TYPE zcl_zodata_read_service=>tt_root_row.
    DATA lt_result TYPE STANDARD TABLE OF zstr_pcct_permission_rs WITH DEFAULT KEY.
    ensure_deps( ).
    lt_root = mo_read_service->read_root_rows( ).
    LOOP AT lt_root ASSIGNING FIELD-SYMBOL(<ls_root_permission_row>).
      APPEND build_permission_row( <ls_root_permission_row>-key ) TO lt_result.
    ENDLOOP.
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

  METHOD build_permission_row.
    ensure_deps( ).
    DATA(ls_root) = mo_read_service->read_root_row( iv_db_key ).
    rs_result = mo_frontend_context->build_permission_result(
      iv_root_key = iv_db_key
      iv_bukrs    = ls_root-bukrs ).
  ENDMETHOD.

  METHOD build_current_user_row.
    ensure_deps( ).
    DATA(ls_runtime_settings) = mo_runtime_settings->read_runtime_settings( ).
    rs_result = mo_frontend_context->build_current_user_result( ls_runtime_settings-permission_rules_json ).
  ENDMETHOD.

  METHOD build_runtime_settings_row.
    ensure_deps( ).
    DATA(ls_runtime_settings) = mo_runtime_settings->read_runtime_settings( ).
    rs_result = mo_frontend_context->build_runtime_settings_result(
      iv_environment          = ls_runtime_settings-environment
      iv_heartbeat_ms         = ls_runtime_settings-heartbeat_ms
      iv_idle_ms              = ls_runtime_settings-idle_ms
      iv_autosave_interval_ms = ls_runtime_settings-autosave_interval_ms
      iv_lock_refresh_ms      = ls_runtime_settings-lock_refresh_ms
      iv_analytics_refresh_ms = ls_runtime_settings-analytics_refresh_ms
      iv_gcd_interval_ms      = ls_runtime_settings-gcd_interval_ms
      iv_network_grace_ms     = ls_runtime_settings-network_grace_ms
      iv_cache_tolerance_ms   = ls_runtime_settings-cache_tolerance_ms ).
    rs_result-frontendvariablesjson = ls_runtime_settings-frontend_variables_json.
    rs_result-requiredfieldsjson = ls_runtime_settings-required_fields_json.
    rs_result-uploadpolicyjson = ls_runtime_settings-upload_policy_json.
  ENDMETHOD.

  METHOD read_db_key.
    TRY.
        rv_db_key = zcl_zodata_odata_util=>get_uuid_from_it_key_tab( it_key_tab = it_key_tab iv_name = 'DB_KEY' ).
      CATCH zcx_zodata_error INTO DATA(lx_db_key).
        raise_busi_exception( iv_text = lx_db_key->get_message_text( ) iv_code = lx_db_key->get_code( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD resolve_lock_db_key.
    rv_db_key = is_lock_request-db_key.
  ENDMETHOD.

  METHOD raise_busi_exception.
    DATA(lo_container) = mo_context->get_message_container( ).
    DATA(lv_message_text) = COND string( WHEN iv_code IS INITIAL THEN iv_text ELSE |{ iv_code }: { iv_text }| ).
    lo_container->add_message_text_only( iv_msg_type = /iwbep/if_mgw_defines=>gcs_msg_type-error iv_msg_text = lv_message_text ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception EXPORTING message_container = lo_container.
  ENDMETHOD.

  METHOD raise_crud_not_allowed.
      raise_busi_exception( iv_text = |{ iv_context }: { zcl_zodata_message_texts=>get_text( zcl_zodata_message_texts=>c_key_crud_not_allowed ) }| iv_code = zif_zodata_message_codes=>technical_error ).
  ENDMETHOD.
ENDCLASS.
