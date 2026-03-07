CLASS zcl_zodata_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zodata_dpc
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~create_entity REDEFINITION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~update_entity REDEFINITION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~delete_entity REDEFINITION.

    " Function imports (Gateway)
    METHODS lockacquire_create_entity REDEFINITION.
    METHODS lockheartbeat_create_entity REDEFINITION.
    METHODS lockrelease_create_entity REDEFINITION.
    METHODS autosave_create_entity REDEFINITION.
    METHODS savechanges_create_entity REDEFINITION.
    METHODS mpltreeset_get_entityset REDEFINITION.

  PRIVATE SECTION.
    DATA mo_lock_manager TYPE REF TO zif_zodata_lock_manager.
    DATA mo_mapper       TYPE REF TO zif_zodata_bopf_mapper.

    METHODS get_srv_mgr
      RETURNING
        VALUE(ro_srv_mgr) TYPE REF TO /bobf/if_tra_service_manager.

    METHODS ensure_deps.
ENDCLASS.

CLASS zcl_zodata_dpc_ext IMPLEMENTATION.

  METHOD ensure_deps.
    IF mo_mapper IS INITIAL.
      mo_mapper = NEW zcl_zodata_bopf_mapper( ).
    ENDIF.
    IF mo_lock_manager IS INITIAL.
      mo_lock_manager = NEW zcl_zodata_lock_manager( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_srv_mgr.
    ro_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_i_bo_c=>sc_bo_key ).
  ENDMETHOD.

  METHOD /iwbep/if_mgw_appl_srv_runtime~create_entity.
    " CRUD fallback: route to SaveDraft/SaveChanges contracts for CUD orchestration
    savechanges_create_entity(
      EXPORTING
        iv_entity_name          = iv_entity_name
        iv_entity_set_name      = iv_entity_set_name
        iv_source_name          = iv_source_name
        it_key_tab              = it_key_tab
        it_navigation_path      = it_navigation_path
        io_tech_request_context = io_tech_request_context
        io_data_provider        = io_data_provider
      IMPORTING
        er_entity               = er_entity ).
  ENDMETHOD.

  METHOD /iwbep/if_mgw_appl_srv_runtime~update_entity.
    DATA lv_guid TYPE sysuuid_x16.

    TRY.
        lv_guid = zcl_zodata_odata_util=>get_uuid_from_it_key_tab(
          it_key_tab = it_key_tab
          iv_name    = 'ObjectUuid' ).

        zcl_lock_manager=>get_instance( )->lock_object(
          iv_guid = lv_guid
          iv_user = sy-uname ).

        savechanges_create_entity(
          EXPORTING
            iv_entity_name          = iv_entity_name
            iv_entity_set_name      = iv_entity_set_name
            iv_source_name          = iv_source_name
            it_key_tab              = it_key_tab
            it_navigation_path      = it_navigation_path
            io_tech_request_context = io_tech_request_context
            io_data_provider        = io_data_provider
          IMPORTING
            er_entity               = er_entity ).

        zcl_lock_manager=>get_instance( )->unlock_on_commit(
          iv_guid = lv_guid
          iv_user = sy-uname ).

      CATCH zcx_lock_error INTO DATA(lx_lock).
        DATA(lo_container) = mo_context->get_message_container( ).
        lo_container->add_message_text_only(
          iv_msg_type = /iwbep/if_mgw_defines=>gcs_msg_type-error
          iv_msg_text = |Заблокировано: { lx_lock->user_fullname } (Таб.№ { lx_lock->pernr })| ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING message_container = lo_container.
    ENDTRY.
  ENDMETHOD.

  METHOD /iwbep/if_mgw_appl_srv_runtime~delete_entity.
    DATA: lo_srv_mgr TYPE REF TO /bobf/if_tra_service_manager,
          lt_change  TYPE zif_zodata_bopf_mapper=>tt_change,
          lt_mod     TYPE /bobf/t_frw_modification,
          lt_failed  TYPE /bobf/t_frw_key,
          lt_msg     TYPE /bobf/t_frw_message_k,
          ls_key     TYPE zif_zodata_lock_manager=>ty_key.

    ensure_deps( ).

    ls_key-bo_key = zif_i_bo_c=>sc_bo_key.
    ls_key-object_id = zcl_zodata_odata_util=>get_uuid_from_it_key_tab(
      it_key_tab = it_key_tab
      iv_name    = 'ObjectUuid' ).

    mo_lock_manager->lock( ls_key ).

    DATA lr_del TYPE REF TO zstr_bo_root.
    CREATE DATA lr_del.
    lr_del->pcct_uuid = ls_key-object_id.

    APPEND VALUE #(
      edit_mode = 'D'
      node_key  = zif_i_bo_c=>sc_node-root
      key       = ls_key-bo_key
      internal  = lr_del ) TO lt_change.

    lt_mod = mo_mapper->map_to_modification( lt_change ).

    lo_srv_mgr = get_srv_mgr( ).
    lo_srv_mgr->modify(
      EXPORTING it_modification = lt_mod
      IMPORTING et_failed_key   = lt_failed
                et_message      = lt_msg ).

    zcl_zodata_bopf_msg_helper=>raise_on_failed_keys(
      it_failed_key = lt_failed
      it_message    = lt_msg ).

    COMMIT WORK AND WAIT.
    mo_lock_manager->unlock( ls_key ).
  ENDMETHOD.

  METHOD lockacquire_create_entity.
    DATA ls_result TYPE zstr_pcct_lock_acquire_rs.
    DATA ls_req    TYPE zstr_pcct_lock_acquire_rq.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).

    TRY.
        zcl_lock_manager=>get_instance( )->lock_object(
          iv_guid = ls_req-object_uuid
          iv_user = ls_req-uname ).

        ls_result-success      = abap_true.
        ls_result-action       = 'ACQUIRED'.
        ls_result-owner        = ls_req-uname.
        ls_result-owner_session = ls_req-session_guid.
        ls_result-lock_expires = sy-datum && sy-uzeit.
        copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).

      CATCH zcx_lock_error INTO DATA(lx_lock).
        DATA(lo_container) = mo_context->get_message_container( ).
        lo_container->add_message_text_only(
          iv_msg_type = /iwbep/if_mgw_defines=>gcs_msg_type-error
          iv_msg_text = |Заблокировано: { lx_lock->user_fullname } (Таб.№ { lx_lock->pernr })| ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING message_container = lo_container.
    ENDTRY.
  ENDMETHOD.

  METHOD lockheartbeat_create_entity.
    DATA ls_result TYPE zstr_pcct_lock_heartbeat_rs.
    DATA ls_req    TYPE zstr_pcct_lock_heartbeat_rq.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).

    CALL FUNCTION 'Z_PCCT_LOCK_HEARTBEAT'
      EXPORTING
        iv_object_uuid  = ls_req-object_uuid
        iv_session_guid = ls_req-session_guid
      IMPORTING
        es_result       = ls_result
      EXCEPTIONS
        heartbeat_failed = 1
        OTHERS           = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING message = 'LOCK_HEARTBEAT failed'.
    ENDIF.

    copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
  ENDMETHOD.

  METHOD lockrelease_create_entity.
    DATA ls_result TYPE zstr_pcct_lock_release_rs.
    DATA ls_req    TYPE zstr_pcct_lock_release_rq.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).

    " Optional: keep FM for compatibility
    zcl_lock_manager=>get_instance( )->unlock_immediately(
      iv_guid = ls_req-object_uuid
      iv_user = sy-uname ).

    CALL FUNCTION 'Z_PCCT_LOCK_RELEASE'
      EXPORTING
        iv_object_uuid  = ls_req-object_uuid
        iv_session_guid = ls_req-session_guid
        iv_try_save     = ls_req-try_save
        is_payload      = ls_req-payload
      IMPORTING
        es_result       = ls_result
      EXCEPTIONS
        release_failed  = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING message = 'LOCK_RELEASE failed'.
    ENDIF.

    COMMIT WORK AND WAIT.
    copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
  ENDMETHOD.

  METHOD autosave_create_entity.
    " Delta path: CUD payload processed through universal mapper
    DATA: lo_srv_mgr TYPE REF TO /bobf/if_tra_service_manager,
          lt_change  TYPE zif_zodata_bopf_mapper=>tt_change,
          lt_mod     TYPE /bobf/t_frw_modification,
          lt_failed  TYPE /bobf/t_frw_key,
          lt_msg     TYPE /bobf/t_frw_message_k,
          ls_req     TYPE zstr_pcct_savechanges_rq,
          ls_resp    TYPE zstr_pcct_savechanges_rs.

    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).

    " Root delta
    DATA lr_root_int TYPE REF TO zstr_bo_root.
    CREATE DATA lr_root_int.
    APPEND VALUE #(
      edit_mode = 'U'
      node_key  = zif_i_bo_c=>sc_node-root
      key       = ls_req-root-pcct_uuid
      external  = REF #( ls_req-root )
      internal  = lr_root_int ) TO lt_change.

    " Checks delta (C/U/D)
    LOOP AT ls_req-checks ASSIGNING FIELD-SYMBOL(<ls_check_delta>).
      DATA lr_check_int TYPE REF TO zstr_bo_check.
      CREATE DATA lr_check_int.
      APPEND VALUE #(
        edit_mode   = <ls_check_delta>-edit_mode
        node_key    = zif_i_bo_c=>sc_node-checks
        source_node = zif_i_bo_c=>sc_node-root
        association = zif_i_bo_c=>sc_association-root-checks
        source_key  = ls_req-root-pcct_uuid
        key         = <ls_check_delta>-check_uuid
        external    = REF #( <ls_check_delta> )
        internal    = lr_check_int ) TO lt_change.
    ENDLOOP.

    " Barriers delta (C/U/D)
    LOOP AT ls_req-barriers ASSIGNING FIELD-SYMBOL(<ls_barrier_delta>).
      DATA lr_barrier_int TYPE REF TO zstr_bo_barrier.
      CREATE DATA lr_barrier_int.
      APPEND VALUE #(
        edit_mode   = <ls_barrier_delta>-edit_mode
        node_key    = zif_i_bo_c=>sc_node-barriers
        source_node = zif_i_bo_c=>sc_node-root
        association = zif_i_bo_c=>sc_association-root-barriers
        source_key  = ls_req-root-pcct_uuid
        key         = <ls_barrier_delta>-barrier_uuid
        external    = REF #( <ls_barrier_delta> )
        internal    = lr_barrier_int ) TO lt_change.
    ENDLOOP.

    lt_mod = mo_mapper->map_to_modification( lt_change ).

    lo_srv_mgr = get_srv_mgr( ).
    lo_srv_mgr->modify(
      EXPORTING it_modification = lt_mod
      IMPORTING et_failed_key   = lt_failed
                et_message      = lt_msg ).

    zcl_zodata_bopf_msg_helper=>raise_on_failed_keys(
      it_failed_key = lt_failed
      it_message    = lt_msg ).

    COMMIT WORK AND WAIT.

    ls_resp-pcct_uuid      = ls_req-root-pcct_uuid.
    ls_resp-changed_on     = sy-datum && sy-uzeit.
    ls_resp-version_number = ls_req-client_version + 1.
    copy_data_to_ref( EXPORTING is_data = ls_resp CHANGING cr_data = er_entity ).
  ENDMETHOD.

  METHOD savechanges_create_entity.
    " Full-save and autosave share one CUD entry point; function import remains explicit by contract
    autosave_create_entity(
      EXPORTING
        iv_entity_name          = iv_entity_name
        iv_entity_set_name      = iv_entity_set_name
        iv_source_name          = iv_source_name
        it_key_tab              = it_key_tab
        it_navigation_path      = it_navigation_path
        io_tech_request_context = io_tech_request_context
        io_data_provider        = io_data_provider
      IMPORTING
        er_entity               = er_entity ).
  ENDMETHOD.

  METHOD mpltreeset_get_entityset.
    DATA: lt_tree TYPE ztt_pcct_mpl_tree,
          lv_date TYPE datum.

    lv_date = sy-datum.
    READ TABLE it_filter_select_options ASSIGNING FIELD-SYMBOL(<ls_filter>) WITH KEY property = 'Date'.
    IF sy-subrc = 0 AND <ls_filter>-select_options IS NOT INITIAL.
      lv_date = <ls_filter>-select_options[ 1 ]-low.
    ENDIF.

    CALL FUNCTION 'Z_PCCT_MPL_TREE_GET'
      EXPORTING
        iv_date = lv_date
      TABLES
        et_tree = lt_tree
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING message = 'MPL tree read failed'.
    ENDIF.

    copy_data_to_ref( EXPORTING is_data = lt_tree CHANGING cr_data = er_entityset ).
  ENDMETHOD.

ENDCLASS.
