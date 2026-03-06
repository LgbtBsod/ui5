INTERFACE zif_zodata_bopf_mapper PUBLIC.

  TYPES: BEGIN OF ty_field_map,
           ext_name TYPE string,
           int_name TYPE string,
           ext_idx  TYPE i,
           int_idx  TYPE i,
         END OF ty_field_map,
         tt_field_map TYPE STANDARD TABLE OF ty_field_map WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_change,
           edit_mode   TYPE c LENGTH 1,
           change_mode TYPE /bobf/conf_change_mode,
           node_key    TYPE /bobf/obm_node_key,
           key         TYPE /bobf/conf_key,
           source_key  TYPE /bobf/conf_key,
           source_node TYPE /bobf/obm_node_key,
           association TYPE /bobf/obm_assoc_key,
           external    TYPE REF TO data,
           internal    TYPE REF TO data,
           field_map   TYPE tt_field_map,
         END OF ty_change,
         tt_change TYPE STANDARD TABLE OF ty_change WITH DEFAULT KEY.

  METHODS map_to_modification
    IMPORTING
      !it_change TYPE tt_change
    RETURNING
      VALUE(rt_modification) TYPE /bobf/t_frw_modification
    RAISING
      zcx_zodata_error.

ENDINTERFACE.
