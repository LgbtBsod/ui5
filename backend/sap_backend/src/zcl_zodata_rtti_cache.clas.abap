"! <p class="shorttext synchronized">RTTI Cache - Field Map & Association Registry</p>
"! Provides an in-memory singleton cache for RTTI-derived
"! field maps and association targets.
"!
"! Cache hierarchy:
"!  L1: process-local sorted cache
"!  L2: RTTI / BOPF configuration build
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

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_zodata_rtti_cache.

    METHODS get_field_map
      IMPORTING
        !io_ext_descr  TYPE REF TO cl_abap_structdescr
        !io_int_descr  TYPE REF TO cl_abap_structdescr
      RETURNING
        VALUE(rt_map)  TYPE tt_field_map.

    METHODS get_assoc_target
      IMPORTING
        !iv_node_key          TYPE /bobf/obm_node_key
        !iv_assoc_key         TYPE /bobf/obm_assoc_key
      RETURNING
        VALUE(rv_target_node) TYPE /bobf/obm_node_key.

    METHODS invalidate_all.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_map_cache,
             cache_key TYPE string,
             map       TYPE tt_field_map,
           END OF ty_map_cache,
           tt_map_cache TYPE SORTED TABLE OF ty_map_cache
                        WITH UNIQUE KEY cache_key.

    TYPES: BEGIN OF ty_assoc_cache,
             cache_key   TYPE string,
             target_node TYPE /bobf/obm_node_key,
           END OF ty_assoc_cache,
           tt_assoc_cache TYPE SORTED TABLE OF ty_assoc_cache
                          WITH UNIQUE KEY cache_key.

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

    METHODS store_map_cache
      IMPORTING
        !iv_key TYPE string
        !it_map TYPE tt_field_map.

    METHODS store_assoc_cache
      IMPORTING
        !iv_key         TYPE string
        !iv_target_node TYPE /bobf/obm_node_key.

ENDCLASS.

CLASS zcl_zodata_rtti_cache IMPLEMENTATION.

  METHOD get_instance.
    IF go_instance IS NOT BOUND.
      go_instance = NEW #( ).
    ENDIF.
    ro_obj = go_instance.
  ENDMETHOD.

  METHOD get_field_map.
    DATA(lv_key) = map_cache_key(
      io_ext_descr = io_ext_descr
      io_int_descr = io_int_descr ).

    READ TABLE mt_map_cache ASSIGNING FIELD-SYMBOL(<ls_map>)
      WITH TABLE KEY cache_key = lv_key.
    IF sy-subrc = 0.
      rt_map = <ls_map>-map.
      RETURN.
    ENDIF.

    rt_map = build_field_map(
      io_ext_descr = io_ext_descr
      io_int_descr = io_int_descr ).

    store_map_cache(
      iv_key = lv_key
      it_map = rt_map ).
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

    rv_target_node = build_assoc_target(
      iv_node_key  = iv_node_key
      iv_assoc_key = iv_assoc_key ).

    store_assoc_cache(
      iv_key         = lv_key
      iv_target_node = rv_target_node ).
  ENDMETHOD.

  METHOD invalidate_all.
    CLEAR mt_map_cache.
    CLEAR mt_assoc_cache.
  ENDMETHOD.

  METHOD build_field_map.
    DATA lt_ext_comp TYPE cl_abap_structdescr=>component_table.
    DATA lt_int_comp TYPE cl_abap_structdescr=>component_table.

    TYPES: BEGIN OF ty_idx,
             name TYPE string,
             pos  TYPE i,
           END OF ty_idx,
           tt_idx TYPE SORTED TABLE OF ty_idx WITH UNIQUE KEY name.

    DATA lt_ext_idx TYPE tt_idx.
    DATA lt_int_idx TYPE tt_idx.

    lt_ext_comp = io_ext_descr->get_components( ).
    lt_int_comp = io_int_descr->get_components( ).

    LOOP AT lt_ext_comp ASSIGNING FIELD-SYMBOL(<ls_ext_comp>).
      INSERT VALUE #( name = to_upper( <ls_ext_comp>-name ) pos = sy-tabix ) INTO TABLE lt_ext_idx.
    ENDLOOP.

    LOOP AT lt_int_comp ASSIGNING FIELD-SYMBOL(<ls_int_comp>).
      INSERT VALUE #( name = to_upper( <ls_int_comp>-name ) pos = sy-tabix ) INTO TABLE lt_int_idx.
    ENDLOOP.

    LOOP AT lt_ext_idx ASSIGNING FIELD-SYMBOL(<ls_ext_idx>).
      READ TABLE lt_int_idx ASSIGNING FIELD-SYMBOL(<ls_int_idx>)
        WITH TABLE KEY name = <ls_ext_idx>-name.
      IF sy-subrc = 0.
        APPEND VALUE #(
          ext_name = <ls_ext_idx>-name
          int_name = <ls_int_idx>-name
          ext_idx  = <ls_ext_idx>-pos
          int_idx  = <ls_int_idx>-pos ) TO rt_map.
      ENDIF.
    ENDLOOP.

    SORT rt_map BY ext_idx ASCENDING.
  ENDMETHOD.

  METHOD build_assoc_target.
    TRY.
        DATA(lo_conf) = /bobf/cl_frw_factory=>get_configuration( /bobf/if_frw_c=>sc_content_nod ).
        lo_conf->get_assoc(
          EXPORTING
            iv_node_key        = iv_node_key
            iv_assoc_key       = iv_assoc_key
          IMPORTING
            ev_target_node_key = rv_target_node ).
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

  METHOD store_map_cache.
    DELETE mt_map_cache WHERE cache_key = iv_key.
    INSERT VALUE #( cache_key = iv_key map = it_map ) INTO TABLE mt_map_cache.
  ENDMETHOD.

  METHOD store_assoc_cache.
    DELETE mt_assoc_cache WHERE cache_key = iv_key.
    INSERT VALUE #( cache_key = iv_key target_node = iv_target_node ) INTO TABLE mt_assoc_cache.
  ENDMETHOD.

ENDCLASS.
