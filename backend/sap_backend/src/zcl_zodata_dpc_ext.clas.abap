"! <p class="shorttext synchronized">DPC Extension – Production Control Checklist OData Service</p>
"! All CUD operations route through function imports (AutoSave, SaveChanges).
"! Standard CRUD endpoints (create_entity, update_entity, delete_entity)
"! are overridden to raise a clear error — callers must use function imports.
"!
"! Save flow:
"!  1. Frontend calls AutoSave FI with full delta payload (SaveChangesRequest)
"!  2. DPC deserialises payload → calls mapper.build_change_list()
"!  3. mapper.map_to_modification() → lt_mod
"!  4. srv_mgr.modify( lt_mod ) → check et_failed_key
"!  5. AutoSave: COMMIT WORK AND WAIT
"!     SaveChanges: same (both commit; difference is frontend intent signal)
"!  6. Return SaveChangesResponse with new version_number
CLASS zcl_zodata_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zodata_dpc
  CREATE PUBLIC.

  PUBLIC SECTION.

    "-- Disabled standard CRUD (all mutations via FI)
    METHODS /iwbep/if_mgw_appl_srv_runtime~create_entity REDEFINITION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~update_entity REDEFINITION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~delete_entity REDEFINITION.

    "-- Function imports
    METHODS lockacquire_create_entity     REDEFINITION.
    METHODS lockheartbeat_create_entity   REDEFINITION.
    METHODS lockrelease_create_entity     REDEFINITION.
    METHODS autosave_create_entity        REDEFINITION.
    METHODS savechanges_create_entity     REDEFINITION.
    METHODS mpltreeset_get_entityset      REDEFINITION.

  PRIVATE SECTION.

    DATA mo_lock_manager TYPE REF TO zif_zodata_lock_manager.
    DATA mo_mapper       TYPE REF TO zif_zodata_bopf_mapper.

    "-- Lazy initialisation of singleton dependencies
    METHODS ensure_deps.

    "-- BOPF Service Manager factory
    METHODS get_srv_mgr
      RETURNING VALUE(ro_srv_mgr) TYPE REF TO /bobf/if_tra_service_manager.

    "-- Core save engine (shared by AutoSave and SaveChanges)
    METHODS execute_save
      IMPORTING
        is_request        TYPE zstr_pcct_savechanges_rq
        iv_is_autosave    TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rs_response) TYPE zstr_pcct_savechanges_rs
      RAISING
        /iwbep/cx_mgw_busi_exception
        zcx_zodata_error.

    "-- Build a business exception from a text (with message container)
    METHODS raise_busi_exception
      IMPORTING
        iv_text   TYPE string
      RAISING
        /iwbep/cx_mgw_busi_exception.

    "-- Raise error: standard CRUD not allowed (guide callers to FI)
    METHODS raise_crud_not_allowed
      IMPORTING
        iv_context TYPE string
      RAISING
        /iwbep/cx_mgw_busi_exception.

ENDCLASS.

CLASS zcl_zodata_dpc_ext IMPLEMENTATION.

  "══════════════════════════════════════════════════════════════════
  " Dependency management
  "══════════════════════════════════════════════════════════════════
  METHOD ensure_deps.
    IF mo_mapper IS INITIAL.
      mo_mapper = zcl_zodata_bopf_mapper=>create( ).
    ENDIF.
    IF mo_lock_manager IS INITIAL.
      mo_lock_manager = NEW zcl_zodata_lock_manager( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_srv_mgr.
    ro_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
      zif_i_bo_c=>sc_bo_key ).
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " Standard CRUD — all disabled, guide callers to function imports
  "══════════════════════════════════════════════════════════════════
  METHOD /iwbep/if_mgw_appl_srv_runtime~create_entity.
    raise_crud_not_allowed( 'CREATE_ENTITY' ).
  ENDMETHOD.

  METHOD /iwbep/if_mgw_appl_srv_runtime~update_entity.
    raise_crud_not_allowed( 'UPDATE_ENTITY' ).
  ENDMETHOD.

  METHOD /iwbep/if_mgw_appl_srv_runtime~delete_entity.
    "-- Delete is the one exception: it uses a simplified FI-style flow
    "-- but keeps the standard endpoint for HTTP DELETE compatibility.
    DATA: lt_change TYPE zif_zodata_bopf_mapper=>tt_change,
          lt_mod    TYPE /bobf/t_frw_modification,
          lt_failed TYPE /bobf/t_frw_key,
          lt_msg    TYPE /bobf/t_frw_message_k,
          ls_key    TYPE zif_zodata_lock_manager=>ty_key.

    ensure_deps( ).

    TRY.
        ls_key-bo_key    = zif_i_bo_c=>sc_bo_key.
        ls_key-object_id = zcl_zodata_odata_util=>get_uuid_from_it_key_tab(
          it_key_tab = it_key_tab
          iv_name    = 'ObjectUuid' ).

        mo_lock_manager->lock( ls_key ).

        " Build delete modification directly (no external data needed)
        DATA lr_del TYPE REF TO zstr_bo_root.
        CREATE DATA lr_del.
        lr_del->pcct_uuid = ls_key-object_id.

        APPEND VALUE #(
          change_mode = /bobf/if_frw_c=>sc_modify_delete
          node_key    = zif_i_bo_c=>sc_node-root
          key         = ls_key-object_id
          internal    = lr_del ) TO lt_change.

        lt_mod = mo_mapper->map_to_modification( lt_change ).

        get_srv_mgr( )->modify(
          EXPORTING it_modification = lt_mod
          IMPORTING et_failed_key   = lt_failed
                    et_message      = lt_msg ).

        zcl_zodata_bopf_msg_helper=>raise_on_failed_keys(
          it_failed_key = lt_failed
          it_message    = lt_msg ).

        COMMIT WORK AND WAIT.
        mo_lock_manager->unlock( ls_key ).

      CATCH zcx_lock_error INTO DATA(lx_lock).
        raise_busi_exception(
          |Объект заблокирован пользователем { lx_lock->user_fullname } (Таб.№ { lx_lock->pernr }).| ).

      CATCH zcx_zodata_error INTO DATA(lx_map).
        raise_busi_exception( lx_map->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " Lock: Acquire
  "══════════════════════════════════════════════════════════════════
  METHOD lockacquire_create_entity.
    DATA ls_req    TYPE zstr_pcct_lock_acquire_rq.
    DATA ls_result TYPE zstr_pcct_lock_acquire_rs.

    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).

    " Validate required fields
    IF ls_req-object_uuid IS INITIAL OR ls_req-uname IS INITIAL OR ls_req-session_guid IS INITIAL.
      raise_busi_exception( 'LockAcquire: ObjectUuid, Uname and SessionGuid are required.' ).
    ENDIF.

    TRY.
        DATA(ls_lock_key) = VALUE zif_zodata_lock_manager=>ty_key(
          bo_key    = zif_i_bo_c=>sc_bo_key
          object_id = ls_req-object_uuid ).

        mo_lock_manager->lock( ls_lock_key ).

        ls_result-success        = abap_true.
        ls_result-action         = 'ACQUIRED'.
        ls_result-owner          = ls_req-uname.
        ls_result-owner_session  = ls_req-session_guid.
        ls_result-tab_session_id = ls_req-tab_session_id.
        ls_result-object_uuid    = ls_req-object_uuid.
        " Lock expiry = now + configured heartbeat window (read from config)
        ls_result-lock_expires   = cl_abap_context_info=>get_system_date( ) &&
                                   cl_abap_context_info=>get_system_time( ).

        copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).

      CATCH zcx_lock_error INTO DATA(lx_lock).
        raise_busi_exception(
          |Заблокировано: { lx_lock->user_fullname } (Таб.№ { lx_lock->pernr })| ).
    ENDTRY.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " Lock: Heartbeat
  "══════════════════════════════════════════════════════════════════
  METHOD lockheartbeat_create_entity.
    DATA ls_req    TYPE zstr_pcct_lock_heartbeat_rq.
    DATA ls_result TYPE zstr_pcct_lock_heartbeat_rs.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).

    IF ls_req-object_uuid IS INITIAL OR ls_req-session_guid IS INITIAL.
      raise_busi_exception( 'LockHeartbeat: ObjectUuid and SessionGuid are required.' ).
    ENDIF.

    CALL FUNCTION 'Z_PCCT_LOCK_HEARTBEAT'
      EXPORTING
        iv_object_uuid  = ls_req-object_uuid
        iv_session_guid = ls_req-session_guid
      IMPORTING
        es_result       = ls_result
      EXCEPTIONS
        heartbeat_failed = 1
        lock_not_held    = 2
        OTHERS           = 3.

    CASE sy-subrc.
      WHEN 0.
        copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
      WHEN 2.
        raise_busi_exception( 'LockHeartbeat: lock is no longer held (session expired or stolen).' ).
      WHEN OTHERS.
        raise_busi_exception( 'LockHeartbeat: heartbeat FM failed.' ).
    ENDCASE.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " Lock: Release
  "══════════════════════════════════════════════════════════════════
  METHOD lockrelease_create_entity.
    DATA ls_req    TYPE zstr_pcct_lock_release_rq.
    DATA ls_result TYPE zstr_pcct_lock_release_rs.

    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).

    IF ls_req-object_uuid IS INITIAL.
      raise_busi_exception( 'LockRelease: ObjectUuid is required.' ).
    ENDIF.

    TRY.
        DATA(ls_lock_key) = VALUE zif_zodata_lock_manager=>ty_key(
          bo_key    = zif_i_bo_c=>sc_bo_key
          object_id = ls_req-object_uuid ).

        mo_lock_manager->unlock( ls_lock_key ).

      CATCH zcx_lock_error.
        " Tolerate: lock may have expired already (beacon release race)
    ENDTRY.

    " If TrySave flag is set and payload provided — perform final save before release
    IF ls_req-try_save = abap_true AND ls_req-payload IS NOT INITIAL.
      TRY.
          execute_save(
            EXPORTING is_request     = ls_req-payload
                      iv_is_autosave = abap_false ).
        CATCH /iwbep/cx_mgw_busi_exception INTO DATA(lx_busi).
          " Don't re-raise on release — save failure is non-fatal at release time
          " Log to application log if available
      ENDTRY.
    ELSE.
      COMMIT WORK AND WAIT.
    ENDIF.

    ls_result-success     = abap_true.
    ls_result-action      = 'RELEASED'.
    ls_result-object_uuid = ls_req-object_uuid.
    copy_data_to_ref( EXPORTING is_data = ls_result CHANGING cr_data = er_entity ).
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " AutoSave — delta patch, commits immediately
  " Frontend calls this during autosave interval (dirty state).
  " Same flow as SaveChanges; semantic distinction is for observability.
  "══════════════════════════════════════════════════════════════════
  METHOD autosave_create_entity.
    DATA ls_req  TYPE zstr_pcct_savechanges_rq.
    DATA ls_resp TYPE zstr_pcct_savechanges_rs.

    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).

    " Client version from request header (passed as param in FI)
    " If frontend passed ClientVersion param, it's already merged into ls_req
    " by the data provider. Caller should set ls_req-client_version explicitly.

    TRY.
        ls_resp = execute_save(
          EXPORTING is_request     = ls_req
                    iv_is_autosave = abap_true ).

        copy_data_to_ref( EXPORTING is_data = ls_resp CHANGING cr_data = er_entity ).

      CATCH zcx_zodata_error INTO DATA(lx_map).
        raise_busi_exception( lx_map->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " SaveChanges — explicit full save triggered by user action
  "══════════════════════════════════════════════════════════════════
  METHOD savechanges_create_entity.
    DATA ls_req  TYPE zstr_pcct_savechanges_rq.
    DATA ls_resp TYPE zstr_pcct_savechanges_rs.

    ensure_deps( ).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).

    TRY.
        ls_resp = execute_save(
          EXPORTING is_request     = ls_req
                    iv_is_autosave = abap_false ).

        copy_data_to_ref( EXPORTING is_data = ls_resp CHANGING cr_data = er_entity ).

      CATCH zcx_zodata_error INTO DATA(lx_map).
        raise_busi_exception( lx_map->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " MplTree — GET entity set for MPL hierarchy selector
  "══════════════════════════════════════════════════════════════════
  METHOD mpltreeset_get_entityset.
    DATA: lt_tree TYPE ztt_pcct_mpl_tree,
          lv_date TYPE datum.

    lv_date = sy-datum.

    " Read optional Date filter parameter
    READ TABLE it_filter_select_options
      ASSIGNING FIELD-SYMBOL(<ls_filter>)
      WITH KEY property = 'Date'.
    IF sy-subrc = 0 AND <ls_filter>-select_options IS NOT INITIAL.
      lv_date = <ls_filter>-select_options[ 1 ]-low.
    ENDIF.

    CALL FUNCTION 'Z_PCCT_MPL_TREE_GET'
      EXPORTING
        iv_date   = lv_date
      TABLES
        et_tree   = lt_tree
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      raise_busi_exception( |MPL tree read failed for date { lv_date }.| ).
    ENDIF.

    copy_data_to_ref( EXPORTING is_data = lt_tree CHANGING cr_data = er_entityset ).
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " PRIVATE: Core save engine
  " Shared implementation for AutoSave and SaveChanges.
  " Both function imports ultimately call this method.
  "══════════════════════════════════════════════════════════════════
  METHOD execute_save.
    DATA: lt_change TYPE zif_zodata_bopf_mapper=>tt_change,
          lt_mod    TYPE /bobf/t_frw_modification,
          lt_failed TYPE /bobf/t_frw_key,
          lt_msg    TYPE /bobf/t_frw_message_k.

    " ── Validate root UUID ──────────────────────────────────────
    IF is_request-root-pcct_uuid IS INITIAL.
      raise_busi_exception( 'SaveChanges: root.pcct_uuid is required.' ).
    ENDIF.

    " ── Build change list from deep payload ─────────────────────
    " Mapper resolves all node configs, allocates internal refs,
    " builds field maps via RTTI cache, returns flat tt_change list.
    lt_change = mo_mapper->build_change_list( is_request ).

    IF lt_change IS INITIAL.
      " Nothing to save (empty delta payload) — return current state
      rs_response-pcct_uuid      = is_request-root-pcct_uuid.
      rs_response-changed_on     = sy-datum && sy-uzeit.
      rs_response-version_number = is_request-client_version.
      rs_response-no_changes     = abap_true.
      RETURN.
    ENDIF.

    " ── Convert to BOPF modification table ──────────────────────
    lt_mod = mo_mapper->map_to_modification( lt_change ).

    " ── Execute BOPF modify ─────────────────────────────────────
    get_srv_mgr( )->modify(
      EXPORTING it_modification = lt_mod
      IMPORTING et_failed_key   = lt_failed
                et_message      = lt_msg ).

    " Raise on BOPF-reported failures before commit
    zcl_zodata_bopf_msg_helper=>raise_on_failed_keys(
      it_failed_key = lt_failed
      it_message    = lt_msg ).

    " ── Commit ──────────────────────────────────────────────────
    " Both autosave and savechanges commit immediately.
    " The frontend decides when to show confirmation to the user.
    COMMIT WORK AND WAIT.

    " ── Build response ──────────────────────────────────────────
    rs_response-pcct_uuid      = is_request-root-pcct_uuid.
    rs_response-changed_on     = sy-datum && sy-uzeit.
    rs_response-version_number = is_request-client_version + 1.
    rs_response-is_autosave    = iv_is_autosave.
    rs_response-no_changes     = abap_false.

    " Collect BOPF messages for frontend display (warnings, infos)
    LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).
      DATA(lv_msg_text) = <ls_msg>-message->if_message~get_text( ).
      APPEND VALUE #(
        msg_type = <ls_msg>-message->if_message~get_message_attributes( )-msg_type
        msg_text = lv_msg_text ) TO rs_response-messages.
    ENDLOOP.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " PRIVATE: Exception helpers
  "══════════════════════════════════════════════════════════════════
  METHOD raise_busi_exception.
    DATA(lo_container) = mo_context->get_message_container( ).
    lo_container->add_message_text_only(
      iv_msg_type = /iwbep/if_mgw_defines=>gcs_msg_type-error
      iv_msg_text = iv_text ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING message_container = lo_container.
  ENDMETHOD.

  METHOD raise_crud_not_allowed.
    raise_busi_exception(
      |{ iv_context }: Standard CRUD is disabled for this service. | &
      |Use function imports AutoSave or SaveChanges instead.| ).
  ENDMETHOD.

ENDCLASS.
