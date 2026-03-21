"! <p class="shorttext synchronized">BOPF Mapper – Production Control Checklist</p>
"! Converts normalized aggregate write payloads into
"! /bobf/t_frw_modification tables for BOPF Service Manager modify().
"!
"! Design:
"!  1. Node Configuration Registry: static table of known BOPF nodes
"!     with their DDIC structure names.  Used to auto-allocate internal
"!     data references from the node_key alone.
"!
"!  2. Field Map Resolution (RTTI + Shared Buffer cache):
"!     For each ext/int structure pair, build ext_name→int_name index
"!     once, then persist to Shared Buffer (indx area zb).
"!     In-memory hashed cache prevents repeated SHB reads per request.
"!
"!  3. build_change_list(): high-level entry point.
"!     Iterates all delta collections in SaveChangesRequest and produces
"!     a flat tt_change list that map_to_modification() then processes.
"!
"!  4. map_to_modification(): low-level entry point.
"!     Loops tt_change, auto-generates keys for Creates, moves fields
"!     via index-based copy (no MOVE-CORRESPONDING overhead), fills
"!     /bobf/s_frw_modification and appends to rt_modification.
CLASS zcl_zodata_bopf_mapper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_zodata_bopf_mapper.

    " Singleton-style factory (allows mock injection in tests)
    CLASS-METHODS create
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_zodata_bopf_mapper.

  PRIVATE SECTION.

    " ── RTTI / Shared Buffer cache ─────────────────────────────
    DATA mo_rtti_cache TYPE REF TO zcl_zodata_rtti_cache.

    METHODS get_cache
      RETURNING VALUE(ro_cache) TYPE REF TO zcl_zodata_rtti_cache.

    " ── Node configuration registry ───────────────────────────
    " Populated once at construction.
    DATA mt_node_config TYPE zif_zodata_bopf_mapper=>tt_node_config.

    METHODS init_node_config.

    METHODS get_node_config
      IMPORTING
        iv_node_key            TYPE /bobf/obm_node_key
      RETURNING
        VALUE(rs_config)       TYPE zif_zodata_bopf_mapper=>ty_node_config
      RAISING
        zcx_zodata_error.

    " ── Internal allocation helper ─────────────────────────────
    METHODS create_internal_ref
      IMPORTING
        iv_ddic_type           TYPE string
      RETURNING
        VALUE(ro_ref)          TYPE REF TO data
      RAISING
        zcx_zodata_error.

    " ── Edit mode converter ────────────────────────────────────
    METHODS map_edit_mode
      IMPORTING
        iv_edit_mode           TYPE c LENGTH 1
      RETURNING
        VALUE(rv_change_mode)  TYPE /bobf/conf_change_mode
      RAISING
        zcx_zodata_error.

    " ── Field-by-index copy (core performance hot path) ────────
    METHODS move_fields_by_map
      IMPORTING
        ir_external            TYPE REF TO data
        ir_internal            TYPE REF TO data
        it_map                 TYPE zif_zodata_bopf_mapper=>tt_field_map
      RAISING
        zcx_zodata_error.

    " ── Delta append helpers (called from build_change_list) ───
    METHODS append_root_delta
      IMPORTING
        is_root                TYPE zstr_pcct_root_delta
        iv_root_key            TYPE /bobf/conf_key
      CHANGING
        ct_change              TYPE zif_zodata_bopf_mapper=>tt_change
      RAISING
        zcx_zodata_error.

    METHODS append_check_deltas
      IMPORTING
        it_checks              TYPE ztab_pcct_check_delta
        iv_root_key            TYPE /bobf/conf_key
      CHANGING
        ct_change              TYPE zif_zodata_bopf_mapper=>tt_change
      RAISING
        zcx_zodata_error.

    METHODS append_barrier_deltas
      IMPORTING
        it_barriers            TYPE ztab_pcct_barrier_delta
        iv_root_key            TYPE /bobf/conf_key
      CHANGING
        ct_change              TYPE zif_zodata_bopf_mapper=>tt_change
      RAISING
        zcx_zodata_error.

    METHODS append_participant_deltas
      IMPORTING
        it_participants        TYPE ztab_pcct_part_delta
        iv_root_key            TYPE /bobf/conf_key
      CHANGING
        ct_change              TYPE zif_zodata_bopf_mapper=>tt_change
      RAISING
        zcx_zodata_error.

    METHODS append_attachment_deltas
      IMPORTING
        it_attachments         TYPE ztab_pcct_attach_delta
        iv_root_key            TYPE /bobf/conf_key
      CHANGING
        ct_change              TYPE zif_zodata_bopf_mapper=>tt_change
      RAISING
        zcx_zodata_error.

ENDCLASS.

CLASS zcl_zodata_bopf_mapper IMPLEMENTATION.

  "══════════════════════════════════════════════════════════════════
  " Factory
  "══════════════════════════════════════════════════════════════════
  METHOD create.
    ro_obj = NEW #( ).
    ro_obj->init_node_config( ).
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " Node configuration registry
  "══════════════════════════════════════════════════════════════════
  METHOD init_node_config.
    " Each entry maps a BOPF node key constant to its DDIC structures.
    " ext_struct_type = OData / SaveChanges delta structure
    " int_struct_type = BOPF BO node data structure
    mt_node_config = VALUE #(
      ( node_key        = zif_i_bo_c=>sc_node-root
        ext_struct_type = 'ZSTR_PCCT_ROOT_DELTA'
        int_struct_type = 'ZSTR_BO_ROOT' )

      ( node_key        = zif_i_bo_c=>sc_node-checks
        ext_struct_type = 'ZSTR_PCCT_CHECK_DELTA'
        int_struct_type = 'ZSTR_BO_CHECK' )

      ( node_key        = zif_i_bo_c=>sc_node-barriers
        ext_struct_type = 'ZSTR_PCCT_BARRIER_DELTA'
        int_struct_type = 'ZSTR_BO_BARRIER' )

      ( node_key        = zif_i_bo_c=>sc_node-participants
        ext_struct_type = 'ZSTR_PCCT_PART_DELTA'
        int_struct_type = 'ZSTR_BO_PARTICIPANT' )

      ( node_key        = zif_i_bo_c=>sc_node-attachments
        ext_struct_type = 'ZSTR_PCCT_ATTACH_DELTA'
        int_struct_type = 'ZSTR_BO_ATTACHMENT' )
    ).
  ENDMETHOD.

  METHOD get_node_config.
    READ TABLE mt_node_config
      INTO rs_config
      WITH TABLE KEY node_key = iv_node_key.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_zodata_error
        EXPORTING iv_code = 'TECHNICAL_ERROR'
                  iv_msg  = |BOPF mapper: no node config registered for node key '{ iv_node_key }'.|.
    ENDIF.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " build_change_list — high-level entry point
  " Builds a flat tt_change from a structured SaveChanges request.
  "══════════════════════════════════════════════════════════════════
  METHOD zif_zodata_bopf_mapper~build_change_list.
    DATA(lv_root_key) = is_request-root-pcct_uuid.

    " Guard: root uuid must be provided
    IF lv_root_key IS INITIAL.
      RAISE EXCEPTION TYPE zcx_zodata_error
        EXPORTING iv_code = 'VALIDATION_ERROR'
                  iv_msg  = 'build_change_list: SaveChangesRequest.root.pcct_uuid is initial.'.
    ENDIF.

    append_root_delta(
      EXPORTING is_root    = is_request-root
                iv_root_key = lv_root_key
      CHANGING  ct_change  = rt_change ).

    IF is_request-checks IS NOT INITIAL.
      append_check_deltas(
        EXPORTING it_checks   = is_request-checks
                  iv_root_key  = lv_root_key
        CHANGING  ct_change   = rt_change ).
    ENDIF.

    IF is_request-barriers IS NOT INITIAL.
      append_barrier_deltas(
        EXPORTING it_barriers = is_request-barriers
                  iv_root_key  = lv_root_key
        CHANGING  ct_change   = rt_change ).
    ENDIF.

    IF is_request-participants IS NOT INITIAL.
      append_participant_deltas(
        EXPORTING it_participants = is_request-participants
                  iv_root_key     = lv_root_key
        CHANGING  ct_change       = rt_change ).
    ENDIF.

    IF is_request-attachments IS NOT INITIAL.
      append_attachment_deltas(
        EXPORTING it_attachments = is_request-attachments
                  iv_root_key    = lv_root_key
        CHANGING  ct_change      = rt_change ).
    ENDIF.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " map_to_modification — low-level entry point
  " Converts tt_change → /bobf/t_frw_modification
  "══════════════════════════════════════════════════════════════════
  METHOD zif_zodata_bopf_mapper~map_to_modification.
    DATA ls_mod  TYPE /bobf/s_frw_modification.
    DATA lt_map  TYPE zif_zodata_bopf_mapper=>tt_field_map.

    LOOP AT it_change ASSIGNING FIELD-SYMBOL(<ls_change>).
      CLEAR ls_mod.

      " ── Resolve change mode ──────────────────────────────────
      IF <ls_change>-change_mode IS NOT INITIAL.
        ls_mod-change_mode = <ls_change>-change_mode.
      ELSEIF <ls_change>-edit_mode IS NOT INITIAL.
        ls_mod-change_mode = map_edit_mode( <ls_change>-edit_mode ).
      ELSE.
        ls_mod-change_mode = /bobf/if_frw_c=>sc_modify_update.
      ENDIF.

      " ── Fill node / key fields ────────────────────────────────
      ls_mod-node        = <ls_change>-node_key.
      ls_mod-source_node = <ls_change>-source_node.
      ls_mod-source_key  = <ls_change>-source_key.
      ls_mod-association = <ls_change>-association.

      " Auto-generate key for Creates; preserve existing key for U/D
      IF <ls_change>-key IS NOT INITIAL.
        ls_mod-key = <ls_change>-key.
      ELSEIF ls_mod-change_mode = /bobf/if_frw_c=>sc_modify_create.
        ls_mod-key = /bobf/cl_frw_factory=>get_new_key( ).
      ELSE.
        " Update/Delete with no key — programming error, raise explicitly
        RAISE EXCEPTION TYPE zcx_zodata_error
          EXPORTING iv_code = 'VALIDATION_ERROR'
                    iv_msg  = |map_to_modification: key is initial for U/D on node { <ls_change>-node_key }.|.
      ENDIF.

      " ── Internal data reference (must be pre-allocated) ───────
      IF <ls_change>-internal IS INITIAL.
        RAISE EXCEPTION TYPE zcx_zodata_error
          EXPORTING iv_code = 'TECHNICAL_ERROR'
                    iv_msg  = |map_to_modification: internal data ref is initial for node { <ls_change>-node_key }.|.
      ENDIF.

      " ── Field-map resolution and copy (skip for Deletes) ──────
      IF ls_mod-change_mode <> /bobf/if_frw_c=>sc_modify_delete
         AND <ls_change>-external IS BOUND.

        " Use caller-supplied map; fall back to RTTI cache resolution
        lt_map = <ls_change>-field_map.
        IF lt_map IS INITIAL.
          ASSIGN <ls_change>-external->* TO FIELD-SYMBOL(<ls_ext_any>).
          ASSIGN <ls_change>-internal->* TO FIELD-SYMBOL(<ls_int_any>).

          IF <ls_ext_any> IS NOT ASSIGNED OR <ls_int_any> IS NOT ASSIGNED.
            RAISE EXCEPTION TYPE zcx_zodata_error
              EXPORTING iv_code = 'TECHNICAL_ERROR'
                        iv_msg  = 'map_to_modification: cannot dereference data refs for auto field-map.'.
          ENDIF.

          DATA(lo_ext_descr) = CAST cl_abap_structdescr(
            cl_abap_typedescr=>describe_by_data( <ls_ext_any> ) ).
          DATA(lo_int_descr) = CAST cl_abap_structdescr(
            cl_abap_typedescr=>describe_by_data( <ls_int_any> ) ).

          lt_map = CORRESPONDING #(
            get_cache( )->get_field_map(
              io_ext_descr = lo_ext_descr
              io_int_descr = lo_int_descr ) ).
        ENDIF.

        move_fields_by_map(
          ir_external = <ls_change>-external
          ir_internal = <ls_change>-internal
          it_map      = lt_map ).
      ENDIF.

      ls_mod-data = <ls_change>-internal.
      APPEND ls_mod TO rt_modification.
    ENDLOOP.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " Delta append helpers
  "══════════════════════════════════════════════════════════════════
  METHOD append_root_delta.
    DATA(ls_config) = get_node_config( zif_i_bo_c=>sc_node-root ).
    DATA lr_int     TYPE REF TO data.
    lr_int = create_internal_ref( ls_config-int_struct_type ).

    APPEND VALUE #(
      edit_mode = COND #( WHEN is_root-edit_mode IS NOT INITIAL THEN is_root-edit_mode ELSE 'U' )
      node_key  = zif_i_bo_c=>sc_node-root
      key       = iv_root_key
      external  = REF #( is_root )
      internal  = lr_int ) TO ct_change.
  ENDMETHOD.

  METHOD append_check_deltas.
    DATA(ls_config) = get_node_config( zif_i_bo_c=>sc_node-checks ).

    LOOP AT it_checks ASSIGNING FIELD-SYMBOL(<ls_check>).
      DATA lr_int TYPE REF TO data.
      lr_int = create_internal_ref( ls_config-int_struct_type ).

      APPEND VALUE #(
        edit_mode   = <ls_check>-edit_mode
        node_key    = zif_i_bo_c=>sc_node-checks
        source_node = zif_i_bo_c=>sc_node-root
        association = zif_i_bo_c=>sc_association-root-checks
        source_key  = iv_root_key
        key         = <ls_check>-check_uuid
        external    = REF #( <ls_check> )
        internal    = lr_int ) TO ct_change.
    ENDLOOP.
  ENDMETHOD.

  METHOD append_barrier_deltas.
    DATA(ls_config) = get_node_config( zif_i_bo_c=>sc_node-barriers ).

    LOOP AT it_barriers ASSIGNING FIELD-SYMBOL(<ls_barrier>).
      DATA lr_int TYPE REF TO data.
      lr_int = create_internal_ref( ls_config-int_struct_type ).

      APPEND VALUE #(
        edit_mode   = <ls_barrier>-edit_mode
        node_key    = zif_i_bo_c=>sc_node-barriers
        source_node = zif_i_bo_c=>sc_node-root
        association = zif_i_bo_c=>sc_association-root-barriers
        source_key  = iv_root_key
        key         = <ls_barrier>-barrier_uuid
        external    = REF #( <ls_barrier> )
        internal    = lr_int ) TO ct_change.
    ENDLOOP.
  ENDMETHOD.

  METHOD append_participant_deltas.
    DATA(ls_config) = get_node_config( zif_i_bo_c=>sc_node-participants ).

    LOOP AT it_participants ASSIGNING FIELD-SYMBOL(<ls_part>).
      DATA lr_int TYPE REF TO data.
      lr_int = create_internal_ref( ls_config-int_struct_type ).

      APPEND VALUE #(
        edit_mode   = <ls_part>-edit_mode
        node_key    = zif_i_bo_c=>sc_node-participants
        source_node = zif_i_bo_c=>sc_node-root
        association = zif_i_bo_c=>sc_association-root-participants
        source_key  = iv_root_key
        key         = <ls_part>-part_uuid
        external    = REF #( <ls_part> )
        internal    = lr_int ) TO ct_change.
    ENDLOOP.
  ENDMETHOD.

  METHOD append_attachment_deltas.
    DATA(ls_config) = get_node_config( zif_i_bo_c=>sc_node-attachments ).

    LOOP AT it_attachments ASSIGNING FIELD-SYMBOL(<ls_attach>).
      DATA lr_int TYPE REF TO data.
      lr_int = create_internal_ref( ls_config-int_struct_type ).

      APPEND VALUE #(
        edit_mode   = <ls_attach>-edit_mode
        node_key    = zif_i_bo_c=>sc_node-attachments
        source_node = zif_i_bo_c=>sc_node-root
        association = zif_i_bo_c=>sc_association-root-attachments
        source_key  = iv_root_key
        key         = <ls_attach>-attach_uuid
        external    = REF #( <ls_attach> )
        internal    = lr_int ) TO ct_change.
    ENDLOOP.
  ENDMETHOD.

  "══════════════════════════════════════════════════════════════════
  " Internal helpers
  "══════════════════════════════════════════════════════════════════
  METHOD get_cache.
    IF mo_rtti_cache IS INITIAL.
      mo_rtti_cache = zcl_zodata_rtti_cache=>get_instance( ).
    ENDIF.
    ro_cache = mo_rtti_cache.
  ENDMETHOD.

  METHOD create_internal_ref.
    TRY.
        DATA(lo_descr) = cl_abap_typedescr=>describe_by_name( iv_ddic_type ).
        CREATE DATA ro_ref TYPE (iv_ddic_type).
      CATCH cx_sy_create_data_error cx_root INTO DATA(lx).
        RAISE EXCEPTION TYPE zcx_zodata_error
          EXPORTING iv_code = 'TECHNICAL_ERROR'
                    iv_msg  = |create_internal_ref: cannot create instance of '{ iv_ddic_type }'. { lx->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.

  METHOD map_edit_mode.
    CASE iv_edit_mode.
      WHEN 'C'. rv_change_mode = /bobf/if_frw_c=>sc_modify_create.
      WHEN 'U'. rv_change_mode = /bobf/if_frw_c=>sc_modify_update.
      WHEN 'D'. rv_change_mode = /bobf/if_frw_c=>sc_modify_delete.
      WHEN SPACE.
        " Default to update for backward compat (autosave root delta)
        rv_change_mode = /bobf/if_frw_c=>sc_modify_update.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_zodata_error
          EXPORTING iv_code = 'VALIDATION_ERROR'
                    iv_msg  = |map_edit_mode: unsupported edit_mode='{ iv_edit_mode }'. Expected C/U/D or space.|.
    ENDCASE.
  ENDMETHOD.

  METHOD move_fields_by_map.
    " Core performance hot path: index-based field copy.
    " Avoids MOVE-CORRESPONDING overhead and symbol table lookups per call.
    ASSIGN ir_external->* TO FIELD-SYMBOL(<ls_ext>).
    ASSIGN ir_internal->* TO FIELD-SYMBOL(<ls_int>).

    IF <ls_ext> IS NOT ASSIGNED OR <ls_int> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE zcx_zodata_error
        EXPORTING iv_code = 'TECHNICAL_ERROR'
                  iv_msg  = 'move_fields_by_map: cannot dereference EXT or INT data ref.'.
    ENDIF.

    LOOP AT it_map ASSIGNING FIELD-SYMBOL(<ls_map>).
      ASSIGN COMPONENT <ls_map>-ext_idx OF STRUCTURE <ls_ext> TO FIELD-SYMBOL(<lv_from>).
      ASSIGN COMPONENT <ls_map>-int_idx OF STRUCTURE <ls_int> TO FIELD-SYMBOL(<lv_to>).
      IF <lv_from> IS ASSIGNED AND <lv_to> IS ASSIGNED.
        <lv_to> = <lv_from>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
