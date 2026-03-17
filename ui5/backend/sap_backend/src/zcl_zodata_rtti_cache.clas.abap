"! <p class="shorttext synchronized">RTTI + Shared Buffer Cache – Field Map &amp; Association Registry</p>
"! Provides in-memory (hashed table) + persistent (Shared Buffer indx/zb) caching
"! of RTTI-derived field maps (ext↔int) and BOPF association targets.
"!
"! Cache hierarchy:
"!  L1: In-memory hashed table  (per work process, zero-cost read)
"!  L2: SAP Shared Buffer indx(zb)  (cross-WP, survives request boundaries)
"!  L3: RTTI build  (expensive: reflections + sort; write-through to L1 + L2)
"!
"! Key format: '<absolute_ext_type>=><absolute_int_type>'
"! Truncated to 32 chars for Shared Buffer ID compatibility.
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

    "-- Get (or build) field map for an ext/int structure pair
    METHODS get_field_map
      IMPORTING
        !io_ext_descr  TYPE REF TO cl_abap_structdescr
        !io_int_descr  TYPE REF TO cl_abap_structdescr
      RETURNING
        VALUE(rt_map)  TYPE tt_field_map.

    "-- Get (or resolve) target node for a BOPF association
    METHODS get_assoc_target
      IMPORTING
        !iv_node_key         TYPE /bobf/obm_node_key
        !iv_assoc_key        TYPE /bobf/obm_assoc_key
      RETURNING
        VALUE(rv_target_node) TYPE /bobf/obm_node_key.

    "-- Invalidate all in-memory cache entries (e.g. after transport)
    METHODS invalidate_all.

  PRIVATE SECTION.

    CONSTANTS:
      c_shb_area       TYPE c LENGTH 4   VALUE 'indx',
      c_shb_subkey     TYPE c LENGTH 2   VALUE 'zb',
      c_shb_prefix_map TYPE string VALUE 'ZM_',   " field map
      c_shb_prefix_asc TYPE string VALUE 'ZA_'.   " association

    TYPES: BEGIN OF ty_map_cache,
             cache_key TYPE string,
             map       TYPE tt_field_map,
           END OF ty_map_cache,
           tt_map_cache TYPE HASHED TABLE OF ty_map_cache
                        WITH UNIQUE KEY cache_key.

    TYPES: BEGIN OF ty_assoc_cache,
             cache_key   TYPE string,
             target_node TYPE /bobf/obm_node_key,
           END OF ty_assoc_cache,
           tt_assoc_cache TYPE HASHED TABLE OF ty_assoc_cache
                          WITH UNIQUE KEY cache_key.

    CLASS-DATA go_instance     TYPE REF TO zcl_zodata_rtti_cache.
    DATA       mt_map_cache    TYPE tt_map_cache.
    DATA       mt_assoc_cache  TYPE tt_assoc_cache.

    "-- Build field map from RTTI components (L3 — expensive)
    METHODS build_field_map
      IMPORTING
        !io_ext_descr TYPE REF TO cl_abap_structdescr
        !io_int_descr TYPE REF TO cl_abap_structdescr
      RETURNING
        VALUE(rt_map) TYPE tt_field_map.

    "-- Resolve association target via BOPF configuration (L3)
    METHODS build_assoc_target
      IMPORTING
        !iv_node_key  TYPE /bobf/obm_node_key
        !iv_assoc_key TYPE /bobf/obm_assoc_key
      RETURNING
        VALUE(rv_target_node) TYPE /bobf/obm_node_key.

    "-- Produce Shared Buffer ID (max 32 chars for indx area)
    METHODS make_shb_id
      IMPORTING
        !iv_prefix    TYPE string
        !iv_raw_key   TYPE string
      RETURNING
        VALUE(rv_id)  TYPE c LENGTH 32.

    "-- Cache key for field map
    METHODS map_cache_key
      IMPORTING
        !io_ext_descr TYPE REF TO cl_abap_structdescr
        !io_int_descr TYPE REF TO cl_abap_structdescr
      RETURNING
        VALUE(rv_key) TYPE string.

    "-- Cache key for association
    METHODS assoc_cache_key
      IMPORTING
        !iv_node_key  TYPE /bobf/obm_node_key
        !iv_assoc_key TYPE /bobf/obm_assoc_key
      RETURNING
        VALUE(rv_key) TYPE string.

ENDCLASS.

CLASS zcl_zodata_rtti_cache IMPLEMENTATION.

  "══════════════════════════════════════════════════════════════════
  " Singleton
  "══════════════════════════════════════════════════════════════════
  METHOD get_instance.
    IF go_instance IS NOT BOUND.
      go_instance = NEW #( ).
    ENDIF.
    ro_obj = go_instance.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " Field map — L1 → L2 → L3 with write-through
  "══════════════════════════════════════════════════════════════════
  METHOD get_field_map.
    DATA(lv_key) = map_cache_key(
      io_ext_descr = io_ext_descr
      io_int_descr = io_int_descr ).

    " L1: in-memory
    READ TABLE mt_map_cache ASSIGNING FIELD-SYMBOL(<ls_l1>)
      WITH TABLE KEY cache_key = lv_key.
    IF sy-subrc = 0.
      rt_map = <ls_l1>-map.
      RETURN.
    ENDIF.

    " L2: shared buffer
    DATA(lv_shb_id) = make_shb_id( iv_prefix = c_shb_prefix_map iv_raw_key = lv_key ).
    IMPORT rt_map = rt_map FROM SHARED BUFFER (c_shb_area)(c_shb_subkey) ID lv_shb_id.
    IF sy-subrc = 0.
      INSERT VALUE #( cache_key = lv_key map = rt_map ) INTO TABLE mt_map_cache.
      RETURN.
    ENDIF.

    " L3: RTTI build
    rt_map = build_field_map(
      io_ext_descr = io_ext_descr
      io_int_descr = io_int_descr ).

    " Write-through to L1 + L2
    INSERT VALUE #( cache_key = lv_key map = rt_map ) INTO TABLE mt_map_cache.
    EXPORT rt_map = rt_map TO SHARED BUFFER (c_shb_area)(c_shb_subkey) ID lv_shb_id.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " Association target — L1 → L2 → L3 with write-through
  "══════════════════════════════════════════════════════════════════
  METHOD get_assoc_target.
    DATA(lv_key) = assoc_cache_key(
      iv_node_key  = iv_node_key
      iv_assoc_key = iv_assoc_key ).

    " L1
    READ TABLE mt_assoc_cache ASSIGNING FIELD-SYMBOL(<ls_l1>)
      WITH TABLE KEY cache_key = lv_key.
    IF sy-subrc = 0.
      rv_target_node = <ls_l1>-target_node.
      RETURN.
    ENDIF.

    " L2
    DATA(lv_shb_id) = make_shb_id( iv_prefix = c_shb_prefix_asc iv_raw_key = lv_key ).
    IMPORT rv_target_node = rv_target_node FROM SHARED BUFFER (c_shb_area)(c_shb_subkey) ID lv_shb_id.
    IF sy-subrc = 0.
      INSERT VALUE #( cache_key = lv_key target_node = rv_target_node ) INTO TABLE mt_assoc_cache.
      RETURN.
    ENDIF.

    " L3
    rv_target_node = build_assoc_target(
      iv_node_key  = iv_node_key
      iv_assoc_key = iv_assoc_key ).

    INSERT VALUE #( cache_key = lv_key target_node = rv_target_node ) INTO TABLE mt_assoc_cache.
    EXPORT rv_target_node = rv_target_node TO SHARED BUFFER (c_shb_area)(c_shb_subkey) ID lv_shb_id.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " Invalidate all L1 entries (call after transport activate if needed)
  "══════════════════════════════════════════════════════════════════
  METHOD invalidate_all.
    CLEAR mt_map_cache.
    CLEAR mt_assoc_cache.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " L3 builders
  "══════════════════════════════════════════════════════════════════
  METHOD build_field_map.
    " Build sorted index tables for ext and int field names, then
    " binary-search-join to produce the mapping array.
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

    LOOP AT lt_ext_comp ASSIGNING FIELD-SYMBOL(<lc>).
      INSERT VALUE #( name = to_upper( <lc>-name ) pos = sy-tabix ) INTO TABLE lt_ext_idx.
    ENDLOOP.

    LOOP AT lt_int_comp ASSIGNING FIELD-SYMBOL(<lci>).
      INSERT VALUE #( name = to_upper( <lci>-name ) pos = sy-tabix ) INTO TABLE lt_int_idx.
    ENDLOOP.

    LOOP AT lt_ext_idx ASSIGNING FIELD-SYMBOL(<le>).
      READ TABLE lt_int_idx ASSIGNING FIELD-SYMBOL(<li>)
        WITH TABLE KEY name = <le>-name.
      IF sy-subrc = 0.
        APPEND VALUE #(
          ext_name = <le>-name
          int_name = <li>-name
          ext_idx  = <le>-pos
          int_idx  = <li>-pos ) TO rt_map.
      ENDIF.
    ENDLOOP.
    " Keep sorted by ext_idx for predictable copy order
    SORT rt_map BY ext_idx ASCENDING.
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

  "══════════════════════════════════════════════════════════════════
  " Key helpers
  "══════════════════════════════════════════════════════════════════
  METHOD map_cache_key.
    rv_key = |{ io_ext_descr->absolute_name }=>{ io_int_descr->absolute_name }|.
  ENDMETHOD.

  METHOD assoc_cache_key.
    rv_key = |{ iv_node_key }=>{ iv_assoc_key }|.
  ENDMETHOD.

  METHOD make_shb_id.
    " SAP indx shared buffer IDs must be <= 32 chars.
    " Use: prefix (3) + hash of raw_key (29).
    DATA lv_hash TYPE c LENGTH 29.
    " Simple deterministic truncation via MD5-like position folding:
    DATA lv_raw  TYPE string.
    lv_raw = iv_raw_key.
    IF strlen( lv_raw ) <= 29.
      lv_hash = lv_raw.
    ELSE.
      " Fold: take first 14 + last 15 chars (covers both type names)
      lv_hash = lv_raw(14) && lv_raw+( strlen( lv_raw ) - 15 )(15).
    ENDIF.
    rv_id = iv_prefix && lv_hash.
  ENDMETHOD.

ENDCLASS.
