"! <p class="shorttext synchronized">RTTI + Shared Buffer Cache - Field Map & Association Registry</p>
"! Provides in-memory old/new caches plus shared-buffer caching for RTTI-derived
"! field maps and association targets.
"!
"! Cache hierarchy:
"!  L1-old: stable in-memory sorted cache, checked first
"!  L1-new: hot in-memory sorted cache, checked second
"!          when L1-new reaches 1000 entries it is rotated into L1-old
"!  L2: SAP Shared Buffer indx(zb)
"!  L3: RTTI / BOPF configuration build
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

    CONSTANTS:
      c_shb_area        TYPE c LENGTH 4 VALUE 'indx',
      c_shb_subkey      TYPE c LENGTH 2 VALUE 'zb',
      c_shb_prefix_map  TYPE string VALUE 'ZM_',
      c_shb_prefix_asc  TYPE string VALUE 'ZA_',
      c_hot_cache_limit TYPE i VALUE 1000.

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
    DATA mt_map_cache_old   TYPE tt_map_cache.
    DATA mt_map_cache_new   TYPE tt_map_cache.
    DATA mt_assoc_cache_old TYPE tt_assoc_cache.
    DATA mt_assoc_cache_new TYPE tt_assoc_cache.

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

    METHODS make_shb_id
      IMPORTING
        !iv_prefix   TYPE string
        !iv_raw_key  TYPE string
      RETURNING
        VALUE(rv_id) TYPE c LENGTH 32.

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

    METHODS promote_map_cache
      IMPORTING
        !iv_key TYPE string
        !it_map TYPE tt_field_map.

    METHODS promote_assoc_cache
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

    READ TABLE mt_map_cache_old ASSIGNING FIELD-SYMBOL(<ls_old>)
      WITH TABLE KEY cache_key = lv_key.
    IF sy-subrc = 0.
      rt_map = <ls_old>-map.
      RETURN.
    ENDIF.

    READ TABLE mt_map_cache_new ASSIGNING FIELD-SYMBOL(<ls_new>)
      WITH TABLE KEY cache_key = lv_key.
    IF sy-subrc = 0.
      rt_map = <ls_new>-map.
      RETURN.
    ENDIF.

    DATA(lv_shb_id) = make_shb_id(
      iv_prefix  = c_shb_prefix_map
      iv_raw_key = lv_key ).
    IMPORT rt_map = rt_map FROM SHARED BUFFER (c_shb_area)(c_shb_subkey) ID lv_shb_id.
    IF sy-subrc = 0.
      promote_map_cache(
        iv_key = lv_key
        it_map = rt_map ).
      RETURN.
    ENDIF.

    rt_map = build_field_map(
      io_ext_descr = io_ext_descr
      io_int_descr = io_int_descr ).

    promote_map_cache(
      iv_key = lv_key
      it_map = rt_map ).
    EXPORT rt_map = rt_map TO SHARED BUFFER (c_shb_area)(c_shb_subkey) ID lv_shb_id.
  ENDMETHOD.

  METHOD get_assoc_target.
    DATA(lv_key) = assoc_cache_key(
      iv_node_key  = iv_node_key
      iv_assoc_key = iv_assoc_key ).

    READ TABLE mt_assoc_cache_old ASSIGNING FIELD-SYMBOL(<ls_assoc_old>)
      WITH TABLE KEY cache_key = lv_key.
    IF sy-subrc = 0.
      rv_target_node = <ls_assoc_old>-target_node.
      RETURN.
    ENDIF.

    READ TABLE mt_assoc_cache_new ASSIGNING FIELD-SYMBOL(<ls_assoc_new>)
      WITH TABLE KEY cache_key = lv_key.
    IF sy-subrc = 0.
      rv_target_node = <ls_assoc_new>-target_node.
      RETURN.
    ENDIF.

    DATA(lv_shb_id) = make_shb_id(
      iv_prefix  = c_shb_prefix_asc
      iv_raw_key = lv_key ).
    IMPORT rv_target_node = rv_target_node FROM SHARED BUFFER (c_shb_area)(c_shb_subkey) ID lv_shb_id.
    IF sy-subrc = 0.
      promote_assoc_cache(
        iv_key = lv_key
        iv_target_node = rv_target_node ).
      RETURN.
    ENDIF.

    rv_target_node = build_assoc_target(
      iv_node_key  = iv_node_key
      iv_assoc_key = iv_assoc_key ).

    promote_assoc_cache(
      iv_key = lv_key
      iv_target_node = rv_target_node ).
    EXPORT rv_target_node = rv_target_node TO SHARED BUFFER (c_shb_area)(c_shb_subkey) ID lv_shb_id.
  ENDMETHOD.

  METHOD invalidate_all.
    CLEAR mt_map_cache_old.
    CLEAR mt_map_cache_new.
    CLEAR mt_assoc_cache_old.
    CLEAR mt_assoc_cache_new.
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
            iv_node_key       = iv_node_key
            iv_assoc_key      = iv_assoc_key
          IMPORTING
            ev_target_node_key = rv_target_node ).
      CATCH cx_root.
        CLEAR rv_target_node.
    ENDTRY.
  ENDMETHOD.

  METHOD make_shb_id.
    DATA lv_hash TYPE c LENGTH 29.
    DATA lv_raw  TYPE string.

    lv_raw = iv_raw_key.
    IF strlen( lv_raw ) <= 29.
      lv_hash = lv_raw.
    ELSE.
      lv_hash = lv_raw(14) && lv_raw+( strlen( lv_raw ) - 15 )(15).
    ENDIF.
    rv_id = iv_prefix && lv_hash.
  ENDMETHOD.

  METHOD map_cache_key.
    rv_key = |{ io_ext_descr->absolute_name }=>{ io_int_descr->absolute_name }|.
  ENDMETHOD.

  METHOD assoc_cache_key.
    rv_key = |{ iv_node_key }=>{ iv_assoc_key }|.
  ENDMETHOD.

  METHOD promote_map_cache.
    DELETE mt_map_cache_old WHERE cache_key = iv_key.
    DELETE mt_map_cache_new WHERE cache_key = iv_key.
    IF lines( mt_map_cache_new ) >= c_hot_cache_limit.
      mt_map_cache_old = mt_map_cache_new.
      CLEAR mt_map_cache_new.
    ENDIF.
    INSERT VALUE #( cache_key = iv_key map = it_map ) INTO TABLE mt_map_cache_new.
  ENDMETHOD.

  METHOD promote_assoc_cache.
    DELETE mt_assoc_cache_old WHERE cache_key = iv_key.
    DELETE mt_assoc_cache_new WHERE cache_key = iv_key.
    IF lines( mt_assoc_cache_new ) >= c_hot_cache_limit.
      mt_assoc_cache_old = mt_assoc_cache_new.
      CLEAR mt_assoc_cache_new.
    ENDIF.
    INSERT VALUE #( cache_key = iv_key target_node = iv_target_node ) INTO TABLE mt_assoc_cache_new.
  ENDMETHOD.

ENDCLASS.
