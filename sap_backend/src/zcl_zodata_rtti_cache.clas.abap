CLASS zcl_zodata_rtti_cache DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_field_map,
             ext_name TYPE string,
             int_name TYPE string,
             ext_idx  TYPE i,
             int_idx  TYPE i,
           END OF ty_field_map,
           tt_field_map TYPE STANDARD TABLE OF ty_field_map WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_assoc,
             node_key    TYPE /bobf/obm_node_key,
             assoc_key   TYPE /bobf/obm_assoc_key,
             target_node TYPE /bobf/obm_node_key,
           END OF ty_assoc,
           tt_assoc TYPE STANDARD TABLE OF ty_assoc WITH DEFAULT KEY.

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_zodata_rtti_cache.

    METHODS get_field_map
      IMPORTING
        !io_ext_descr TYPE REF TO cl_abap_structdescr
        !io_int_descr TYPE REF TO cl_abap_structdescr
      RETURNING
        VALUE(rt_map) TYPE tt_field_map.

    METHODS get_assoc_target
      IMPORTING
        !iv_node_key  TYPE /bobf/obm_node_key
        !iv_assoc_key TYPE /bobf/obm_assoc_key
      RETURNING
        VALUE(rv_target_node) TYPE /bobf/obm_node_key.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_map_cache,
             cache_key TYPE string,
             map       TYPE tt_field_map,
           END OF ty_map_cache,
           tt_map_cache TYPE HASHED TABLE OF ty_map_cache WITH UNIQUE KEY cache_key.

    TYPES: BEGIN OF ty_assoc_cache,
             cache_key   TYPE string,
             target_node TYPE /bobf/obm_node_key,
           END OF ty_assoc_cache,
           tt_assoc_cache TYPE HASHED TABLE OF ty_assoc_cache WITH UNIQUE KEY cache_key.

    CLASS-DATA go_instance TYPE REF TO zcl_zodata_rtti_cache.

    DATA mt_map_cache   TYPE tt_map_cache.
    DATA mt_assoc_cache TYPE tt_assoc_cache.

    METHODS build_field_map
      IMPORTING
        !io_ext_descr TYPE REF TO cl_abap_structdescr
        !io_int_descr TYPE REF TO cl_abap_structdescr
      RETURNING
        VALUE(rt_map) TYPE tt_field_map.

    METHODS build_assoc_target
      IMPORTING
        !iv_node_key  TYPE /bobf/obm_node_key
        !iv_assoc_key TYPE /bobf/obm_assoc_key
      RETURNING
        VALUE(rv_target_node) TYPE /bobf/obm_node_key.

    METHODS map_cache_key
      IMPORTING
        !io_ext_descr TYPE REF TO cl_abap_structdescr
        !io_int_descr TYPE REF TO cl_abap_structdescr
      RETURNING
        VALUE(rv_key) TYPE string.

    METHODS assoc_cache_key
      IMPORTING
        !iv_node_key  TYPE /bobf/obm_node_key
        !iv_assoc_key TYPE /bobf/obm_assoc_key
      RETURNING
        VALUE(rv_key) TYPE string.
ENDCLASS.

CLASS zcl_zodata_rtti_cache IMPLEMENTATION.

  METHOD get_instance.
    go_instance = COND #( WHEN go_instance IS BOUND THEN go_instance ELSE NEW #( ) ).
    ro_obj = go_instance.
  ENDMETHOD.

  METHOD get_field_map.
    DATA(lv_key) = map_cache_key(
      io_ext_descr = io_ext_descr
      io_int_descr = io_int_descr ).

    READ TABLE mt_map_cache ASSIGNING FIELD-SYMBOL(<ls_cache>)
      WITH TABLE KEY cache_key = lv_key.
    IF sy-subrc = 0.
      rt_map = <ls_cache>-map.
      RETURN.
    ENDIF.

    IMPORT rt_map = rt_map FROM SHARED BUFFER indx(zb)
      ID |ZRTTI_MAP_{ lv_key }|.
    IF sy-subrc = 0.
      INSERT VALUE #( cache_key = lv_key map = rt_map ) INTO TABLE mt_map_cache.
      RETURN.
    ENDIF.

    rt_map = build_field_map(
      io_ext_descr = io_ext_descr
      io_int_descr = io_int_descr ).

    INSERT VALUE #( cache_key = lv_key map = rt_map ) INTO TABLE mt_map_cache.
    EXPORT rt_map = rt_map TO SHARED BUFFER indx(zb)
      ID |ZRTTI_MAP_{ lv_key }|.
  ENDMETHOD.

  METHOD get_assoc_target.
    DATA(lv_key) = assoc_cache_key(
      iv_node_key  = iv_node_key
      iv_assoc_key = iv_assoc_key ).

    READ TABLE mt_assoc_cache ASSIGNING FIELD-SYMBOL(<ls_assoc>)
      WITH TABLE KEY cache_key = lv_key.
    IF sy-subrc = 0.
      rv_target_node = <ls_assoc>-target_node.
      RETURN.
    ENDIF.

    IMPORT rv_target_node = rv_target_node FROM SHARED BUFFER indx(zb)
      ID |ZRTTI_ASSOC_{ lv_key }|.
    IF sy-subrc = 0.
      INSERT VALUE #( cache_key = lv_key target_node = rv_target_node ) INTO TABLE mt_assoc_cache.
      RETURN.
    ENDIF.

    rv_target_node = build_assoc_target(
      iv_node_key  = iv_node_key
      iv_assoc_key = iv_assoc_key ).

    INSERT VALUE #( cache_key = lv_key target_node = rv_target_node ) INTO TABLE mt_assoc_cache.
    EXPORT rv_target_node = rv_target_node TO SHARED BUFFER indx(zb)
      ID |ZRTTI_ASSOC_{ lv_key }|.
  ENDMETHOD.

  METHOD build_field_map.
    DATA lt_ext_components TYPE cl_abap_structdescr=>component_table.
    DATA lt_int_components TYPE cl_abap_structdescr=>component_table.

    TYPES: BEGIN OF ty_idx,
             name TYPE string,
             idx  TYPE i,
           END OF ty_idx,
           tt_idx TYPE STANDARD TABLE OF ty_idx WITH DEFAULT KEY.

    DATA lt_ext_idx TYPE tt_idx.
    DATA lt_int_idx TYPE tt_idx.

    lt_ext_components = io_ext_descr->get_components( ).
    lt_int_components = io_int_descr->get_components( ).

    LOOP AT lt_ext_components ASSIGNING FIELD-SYMBOL(<ls_ext_comp>).
      APPEND VALUE #( name = to_upper( <ls_ext_comp>-name ) idx = sy-tabix ) TO lt_ext_idx.
    ENDLOOP.

    LOOP AT lt_int_components ASSIGNING FIELD-SYMBOL(<ls_int_comp>).
      APPEND VALUE #( name = to_upper( <ls_int_comp>-name ) idx = sy-tabix ) TO lt_int_idx.
    ENDLOOP.

    SORT lt_ext_idx BY name.
    SORT lt_int_idx BY name.

    LOOP AT lt_ext_idx ASSIGNING FIELD-SYMBOL(<ls_ext_idx>).
      READ TABLE lt_int_idx ASSIGNING FIELD-SYMBOL(<ls_int_idx>)
        WITH KEY name = <ls_ext_idx>-name BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND VALUE #( 
        ext_name = <ls_ext_idx>-name
        int_name = <ls_int_idx>-name
        ext_idx  = <ls_ext_idx>-idx
        int_idx  = <ls_int_idx>-idx ) TO rt_map.
    ENDLOOP.

    SORT rt_map BY ext_idx.
  ENDMETHOD.

  METHOD build_assoc_target.
    TRY.
        DATA(lo_conf) = /bobf/cl_frw_factory=>get_configuration( /bobf/if_frw_c=>sc_content_nod ).
        lo_conf->get_assoc(
          EXPORTING iv_node_key  = iv_node_key
                    iv_assoc_key = iv_assoc_key
          IMPORTING ev_target_node_key = rv_target_node ).
      CATCH cx_root.
        CLEAR rv_target_node.
    ENDTRY.
  ENDMETHOD.

  METHOD map_cache_key.
    rv_key = |{ io_ext_descr->absolute_name }=>{ io_int_descr->absolute_name }|.
  ENDMETHOD.

  METHOD assoc_cache_key.
    rv_key = |{ iv_node_key }=>{ iv_assoc_key }|.
  ENDMETHOD.

ENDCLASS.
