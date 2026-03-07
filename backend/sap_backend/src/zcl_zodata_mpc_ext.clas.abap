CLASS zcl_zodata_mpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zodata_mpc
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS define REDEFINITION.

  PRIVATE SECTION.
    METHODS define_function_imports.
    METHODS define_deep_types.
ENDCLASS.

CLASS zcl_zodata_mpc_ext IMPLEMENTATION.

  METHOD define.
    super->define( ).

    define_deep_types( ).
    define_function_imports( ).

    " Optimistic concurrency for checklist root
    DATA(lo_root) = model->get_entity_type( iv_entity_name = 'Checklist' ).
    IF lo_root IS BOUND.
      DATA(lo_changed) = lo_root->get_property( iv_property_name = 'ChangedOn' ).
      lo_changed->set_is_etag( ).
    ENDIF.
  ENDMETHOD.

  METHOD define_deep_types.
    " Bind deep request/response DDIC structures used by SaveChanges / AutoSave
    model->create_complex_type(
      iv_complex_type_name = 'SaveChangesRequest'
      iv_abap_structure_name = 'ZSTR_PCCT_SAVECHANGES_RQ' ).

    model->create_complex_type(
      iv_complex_type_name = 'SaveChangesResponse'
      iv_abap_structure_name = 'ZSTR_PCCT_SAVECHANGES_RS' ).

    model->create_complex_type(
      iv_complex_type_name = 'LockAcquireRequest'
      iv_abap_structure_name = 'ZSTR_PCCT_LOCK_ACQUIRE_RQ' ).

    model->create_complex_type(
      iv_complex_type_name = 'LockAcquireResponse'
      iv_abap_structure_name = 'ZSTR_PCCT_LOCK_ACQUIRE_RS' ).
  ENDMETHOD.

  METHOD define_function_imports.
    DATA(lo_fi) = model->create_function_import( iv_function_import_name = 'LockAcquire' ).
    lo_fi->set_http_method( 'POST' ).
    lo_fi->set_return_type( iv_entity_type_name = 'LockAcquireResponse' ).

    lo_fi = model->create_function_import( iv_function_import_name = 'LockHeartbeat' ).
    lo_fi->set_http_method( 'POST' ).
    lo_fi->set_return_type( iv_entity_type_name = 'LockHeartbeat' ).

    lo_fi = model->create_function_import( iv_function_import_name = 'LockRelease' ).
    lo_fi->set_http_method( 'POST' ).
    lo_fi->set_return_type( iv_entity_type_name = 'LockRelease' ).

    lo_fi = model->create_function_import( iv_function_import_name = 'AutoSave' ).
    lo_fi->set_http_method( 'POST' ).
    lo_fi->set_return_type( iv_entity_type_name = 'SaveChangesResponse' ).

    lo_fi = model->create_function_import( iv_function_import_name = 'SaveChanges' ).
    lo_fi->set_http_method( 'POST' ).
    lo_fi->set_return_type( iv_entity_type_name = 'SaveChangesResponse' ).

    lo_fi = model->create_function_import( iv_function_import_name = 'MplTree' ).
    lo_fi->set_http_method( 'GET' ).
    lo_fi->set_return_entity_set( 'MplTreeSet' ).
  ENDMETHOD.

ENDCLASS.
