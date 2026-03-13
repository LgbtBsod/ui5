"! <p class="shorttext synchronized">MPC Extension – Production Control Checklist OData Service</p>
"! Deep entity types for SaveChanges / AutoSave function imports.
"! All CUD operations use function imports exclusively (no standard CRUD).
CLASS zcl_zodata_mpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zodata_mpc
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS define REDEFINITION.

  PRIVATE SECTION.
    "-- Deep complex types used by SaveChanges / AutoSave payloads
    METHODS define_deep_types.

    "-- All function imports (Lock, AutoSave, SaveChanges, MplTree)
    METHODS define_function_imports.

    "-- Entity sets used only for GET operations (search + read)
    METHODS configure_etag.

    "-- Shared helper: add parameter to function import
    METHODS add_fi_param
      IMPORTING
        io_fi       TYPE REF TO /iwbep/if_mgw_odata_func_imp
        iv_name     TYPE string
        iv_edm_type TYPE string
        iv_nullable TYPE abap_bool DEFAULT abap_true
        iv_mode     TYPE string    DEFAULT 'In'.

ENDCLASS.

CLASS zcl_zodata_mpc_ext IMPLEMENTATION.

  METHOD define.
    super->define( ).
    configure_etag( ).
    define_deep_types( ).
    define_function_imports( ).
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " DEEP ENTITY / COMPLEX TYPE DEFINITIONS
  "══════════════════════════════════════════════════════════════════
  METHOD define_deep_types.

    " ── Root payload ─────────────────────────────────────────────
    " Maps to ZSTR_PCCT_SAVECHANGES_RQ (DDIC structure)
    " Contains:
    "   root     : ZSTR_PCCT_ROOT_DELTA   (single root record delta with EDIT_MODE)
    "   checks   : ZTAB_PCCT_CHECK_DELTA  (table of check items)
    "   barriers : ZTAB_PCCT_BARRIER_DELTA(table of barrier items)
    "   participants : ZTAB_PCCT_PART_DELTA  (row delta with EDIT_MODE)
    "   attachments  : ZTAB_PCCT_ATTACH_DELTA(row delta with EDIT_MODE)
    "   client_version : INT4 (optimistic concurrency)

    model->create_complex_type(
      iv_complex_type_name   = 'SaveChangesRequest'
      iv_abap_structure_name = 'ZSTR_PCCT_SAVECHANGES_RQ' ).

    " ── Response payload ─────────────────────────────────────────
    " Returns: pcct_uuid, changed_on, version_number, messages[]
    model->create_complex_type(
      iv_complex_type_name   = 'SaveChangesResponse'
      iv_abap_structure_name = 'ZSTR_PCCT_SAVECHANGES_RS' ).

    " ── Lock acquire request / response ──────────────────────────
    model->create_complex_type(
      iv_complex_type_name   = 'LockAcquireRequest'
      iv_abap_structure_name = 'ZSTR_PCCT_LOCK_ACQUIRE_RQ' ).

    model->create_complex_type(
      iv_complex_type_name   = 'LockAcquireResponse'
      iv_abap_structure_name = 'ZSTR_PCCT_LOCK_ACQUIRE_RS' ).

    " ── Lock heartbeat request / response ────────────────────────
    model->create_complex_type(
      iv_complex_type_name   = 'LockHeartbeatRequest'
      iv_abap_structure_name = 'ZSTR_PCCT_LOCK_HEARTBEAT_RQ' ).

    model->create_complex_type(
      iv_complex_type_name   = 'LockHeartbeatResponse'
      iv_abap_structure_name = 'ZSTR_PCCT_LOCK_HEARTBEAT_RS' ).

    " ── Lock release request / response ──────────────────────────
    model->create_complex_type(
      iv_complex_type_name   = 'LockReleaseRequest'
      iv_abap_structure_name = 'ZSTR_PCCT_LOCK_RELEASE_RQ' ).

    model->create_complex_type(
      iv_complex_type_name   = 'LockReleaseResponse'
      iv_abap_structure_name = 'ZSTR_PCCT_LOCK_RELEASE_RS' ).

    " ── Message entry (part of SaveChangesResponse.messages[]) ───
    model->create_complex_type(
      iv_complex_type_name   = 'ServiceMessage'
      iv_abap_structure_name = 'ZSTR_PCCT_SERVICE_MSG' ).

  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " FUNCTION IMPORT DEFINITIONS
  " All mutations go through function imports — no CRUD endpoints.
  "══════════════════════════════════════════════════════════════════
  METHOD define_function_imports.
    DATA lo_fi TYPE REF TO /iwbep/if_mgw_odata_func_imp.

    " ── LockAcquire ──────────────────────────────────────────────
    lo_fi = model->create_function_import( iv_function_import_name = 'LockAcquire' ).
    lo_fi->set_http_method( /iwbep/if_mgw_odata_func_imp=>gcs_http_method-post ).
    lo_fi->set_return_type( iv_entity_type_name = 'LockAcquireResponse' ).
    add_fi_param( io_fi = lo_fi iv_name = 'ObjectUuid'   iv_edm_type = 'Guid'    iv_nullable = abap_false ).
    add_fi_param( io_fi = lo_fi iv_name = 'Uname'        iv_edm_type = 'String'  iv_nullable = abap_false ).
    add_fi_param( io_fi = lo_fi iv_name = 'SessionGuid'  iv_edm_type = 'Guid'    iv_nullable = abap_false ).
    add_fi_param( io_fi = lo_fi iv_name = 'TabSessionId' iv_edm_type = 'String'  iv_nullable = abap_true ).

    " ── LockHeartbeat ────────────────────────────────────────────
    lo_fi = model->create_function_import( iv_function_import_name = 'LockHeartbeat' ).
    lo_fi->set_http_method( /iwbep/if_mgw_odata_func_imp=>gcs_http_method-post ).
    lo_fi->set_return_type( iv_entity_type_name = 'LockHeartbeatResponse' ).
    add_fi_param( io_fi = lo_fi iv_name = 'ObjectUuid'  iv_edm_type = 'Guid' iv_nullable = abap_false ).
    add_fi_param( io_fi = lo_fi iv_name = 'SessionGuid' iv_edm_type = 'Guid' iv_nullable = abap_false ).

    " ── LockRelease ──────────────────────────────────────────────
    lo_fi = model->create_function_import( iv_function_import_name = 'LockRelease' ).
    lo_fi->set_http_method( /iwbep/if_mgw_odata_func_imp=>gcs_http_method-post ).
    lo_fi->set_return_type( iv_entity_type_name = 'LockReleaseResponse' ).
    add_fi_param( io_fi = lo_fi iv_name = 'ObjectUuid'  iv_edm_type = 'Guid'    iv_nullable = abap_false ).
    add_fi_param( io_fi = lo_fi iv_name = 'SessionGuid' iv_edm_type = 'Guid'    iv_nullable = abap_false ).
    add_fi_param( io_fi = lo_fi iv_name = 'TrySave'     iv_edm_type = 'Boolean' iv_nullable = abap_true ).

    " ── AutoSave (delta patch, no COMMIT WORK — deferred) ────────
    lo_fi = model->create_function_import( iv_function_import_name = 'AutoSave' ).
    lo_fi->set_http_method( /iwbep/if_mgw_odata_func_imp=>gcs_http_method-post ).
    lo_fi->set_return_type( iv_entity_type_name = 'SaveChangesResponse' ).
    add_fi_param( io_fi = lo_fi iv_name = 'Payload'       iv_edm_type = 'SaveChangesRequest' iv_nullable = abap_false ).
    add_fi_param( io_fi = lo_fi iv_name = 'ClientVersion' iv_edm_type = 'Int32'              iv_nullable = abap_true ).

    " ── SaveChanges (full save, triggers COMMIT WORK) ─────────────
    lo_fi = model->create_function_import( iv_function_import_name = 'SaveChanges' ).
    lo_fi->set_http_method( /iwbep/if_mgw_odata_func_imp=>gcs_http_method-post ).
    lo_fi->set_return_type( iv_entity_type_name = 'SaveChangesResponse' ).
    add_fi_param( io_fi = lo_fi iv_name = 'Payload'       iv_edm_type = 'SaveChangesRequest' iv_nullable = abap_false ).
    add_fi_param( io_fi = lo_fi iv_name = 'ClientVersion' iv_edm_type = 'Int32'              iv_nullable = abap_true ).

    " ── MplTree (GET hierarchy for MPL selector) ─────────────────
    lo_fi = model->create_function_import( iv_function_import_name = 'MplTree' ).
    lo_fi->set_http_method( /iwbep/if_mgw_odata_func_imp=>gcs_http_method-get ).
    lo_fi->set_return_entity_set( 'MplTreeSet' ).
    add_fi_param( io_fi = lo_fi iv_name = 'Date' iv_edm_type = 'Date' iv_nullable = abap_true iv_mode = 'In' ).

  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " ETag configuration for optimistic concurrency on Checklist root
  "══════════════════════════════════════════════════════════════════
  METHOD configure_etag.
    TRY.
        DATA(lo_root) = model->get_entity_type( iv_entity_name = 'Checklist' ).
        IF lo_root IS BOUND.
          DATA(lo_changed) = lo_root->get_property( iv_property_name = 'ChangedOn' ).
          IF lo_changed IS BOUND.
            lo_changed->set_is_etag( ).
          ENDIF.
        ENDIF.
      CATCH cx_root INTO DATA(lx).
        " Tolerate missing entity during initial metadata generation
        " Log but don't raise — schema still valid without ETag
    ENDTRY.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " Helper: add input parameter to a function import
  "══════════════════════════════════════════════════════════════════
  METHOD add_fi_param.
    DATA lo_param TYPE REF TO /iwbep/if_mgw_odata_param.
    lo_param = io_fi->create_parameter( iv_parameter_name = iv_name ).
    lo_param->set_edm_type( iv_edm_type ).
    IF iv_nullable = abap_false.
      lo_param->set_nullable( abap_false ).
    ENDIF.
    IF iv_mode = 'Out'.
      lo_param->set_mode( /iwbep/if_mgw_odata_param=>gcs_param_mode-out ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
