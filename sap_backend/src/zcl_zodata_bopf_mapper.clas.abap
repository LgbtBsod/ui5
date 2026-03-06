CLASS zcl_zodata_bopf_mapper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_zodata_bopf_mapper.

  PRIVATE SECTION.
    DATA mo_rtti_cache TYPE REF TO zcl_zodata_rtti_cache.

    METHODS get_cache
      RETURNING VALUE(ro_cache) TYPE REF TO zcl_zodata_rtti_cache.

    METHODS map_edit_mode
      IMPORTING
        !iv_edit_mode TYPE c LENGTH 1
      RETURNING
        VALUE(rv_change_mode) TYPE /bobf/conf_change_mode
      RAISING
        zcx_zodata_error.

    METHODS move_fields_by_map
      IMPORTING
        !ir_external TYPE REF TO data
        !ir_internal TYPE REF TO data
        !it_map      TYPE zif_zodata_bopf_mapper=>tt_field_map
      RAISING
        zcx_zodata_error.
ENDCLASS.

CLASS zcl_zodata_bopf_mapper IMPLEMENTATION.

  METHOD get_cache.
    IF mo_rtti_cache IS INITIAL.
      mo_rtti_cache = zcl_zodata_rtti_cache=>get_instance( ).
    ENDIF.
    ro_cache = mo_rtti_cache.
  ENDMETHOD.

  METHOD zif_zodata_bopf_mapper~map_to_modification.
    DATA ls_mod TYPE /bobf/s_frw_modification.
    DATA lt_map TYPE zif_zodata_bopf_mapper=>tt_field_map.

    LOOP AT it_change ASSIGNING FIELD-SYMBOL(<ls_change>).
      CLEAR ls_mod.

      ls_mod-change_mode = <ls_change>-change_mode.
      IF ls_mod-change_mode IS INITIAL AND <ls_change>-edit_mode IS NOT INITIAL.
        ls_mod-change_mode = map_edit_mode( <ls_change>-edit_mode ).
      ENDIF.

      ls_mod-node        = <ls_change>-node_key.
      ls_mod-key         = <ls_change>-key.
      ls_mod-source_key  = <ls_change>-source_key.
      ls_mod-source_node = <ls_change>-source_node.
      ls_mod-association = <ls_change>-association.

      IF ls_mod-key IS INITIAL AND ls_mod-change_mode <> /bobf/if_frw_c=>sc_modify_delete.
        ls_mod-key = /bobf/cl_frw_factory=>get_new_key( ).
      ENDIF.

      IF <ls_change>-internal IS INITIAL.
        RAISE EXCEPTION TYPE zcx_zodata_error
          EXPORTING iv_msg = |BOPF mapper: INTERNAL data reference is initial for node { <ls_change>-node_key }.|.
      ENDIF.

      IF <ls_change>-external IS BOUND.
        ASSIGN <ls_change>-external->* TO FIELD-SYMBOL(<ls_ext_any>).
        ASSIGN <ls_change>-internal->* TO FIELD-SYMBOL(<ls_int_any>).

        IF <ls_ext_any> IS NOT ASSIGNED OR <ls_int_any> IS NOT ASSIGNED.
          RAISE EXCEPTION TYPE zcx_zodata_error
            EXPORTING iv_msg = |BOPF mapper: failed to dereference EXT/INT data refs.|.
        ENDIF.

        DATA(lo_ext_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <ls_ext_any> ) ).
        DATA(lo_int_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <ls_int_any> ) ).

        lt_map = <ls_change>-field_map.
        IF lt_map IS INITIAL.
          lt_map = CORRESPONDING #( get_cache( )->get_field_map(
            io_ext_descr = lo_ext_descr
            io_int_descr = lo_int_descr ) ).
        ENDIF.

        move_fields_by_map(
          ir_external = <ls_change>-external
          ir_internal = <ls_change>-internal
          it_map      = lt_map ).
      ENDIF.

      " Association recursion hint: cache target node lookup for nested graphs
      IF ls_mod-association IS NOT INITIAL AND ls_mod-source_node IS NOT INITIAL.
        DATA(lv_target_node) = get_cache( )->get_assoc_target(
          iv_node_key  = ls_mod-source_node
          iv_assoc_key = ls_mod-association ).
        IF lv_target_node IS INITIAL.
          " keep current node if association metadata unavailable in draft runtime
        ENDIF.
      ENDIF.

      ls_mod-data = <ls_change>-internal.
      APPEND ls_mod TO rt_modification.
    ENDLOOP.
  ENDMETHOD.

  METHOD map_edit_mode.
    CASE iv_edit_mode.
      WHEN 'C'. rv_change_mode = /bobf/if_frw_c=>sc_modify_create.
      WHEN 'U'. rv_change_mode = /bobf/if_frw_c=>sc_modify_update.
      WHEN 'D'. rv_change_mode = /bobf/if_frw_c=>sc_modify_delete.
      WHEN SPACE. rv_change_mode = /bobf/if_frw_c=>sc_modify_update.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_zodata_error
          EXPORTING iv_msg = |Unsupported edit_mode={ iv_edit_mode }. Expected C/U/D/space.|.
    ENDCASE.
  ENDMETHOD.

  METHOD move_fields_by_map.
    ASSIGN ir_external->* TO FIELD-SYMBOL(<ls_ext_any>).
    ASSIGN ir_internal->* TO FIELD-SYMBOL(<ls_int_any>).

    IF <ls_ext_any> IS NOT ASSIGNED OR <ls_int_any> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE zcx_zodata_error
        EXPORTING iv_msg = 'BOPF mapper: data references cannot be assigned.'.
    ENDIF.

    LOOP AT it_map ASSIGNING FIELD-SYMBOL(<ls_map>).
      ASSIGN COMPONENT <ls_map>-ext_idx OF STRUCTURE <ls_ext_any> TO FIELD-SYMBOL(<lv_ext_val>).
      ASSIGN COMPONENT <ls_map>-int_idx OF STRUCTURE <ls_int_any> TO FIELD-SYMBOL(<lv_int_val>).
      IF <lv_ext_val> IS ASSIGNED AND <lv_int_val> IS ASSIGNED.
        <lv_int_val> = <lv_ext_val>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
