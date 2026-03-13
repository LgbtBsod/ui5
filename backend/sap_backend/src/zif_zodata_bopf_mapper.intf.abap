"! <p class="shorttext synchronized">BOPF Mapper Interface – Production Control Checklist</p>
"! Contract for mapping OData deep payloads to BOPF modification tables.
"! The mapper is node-config driven: callers register node mappings once,
"! and the mapper resolves field maps automatically via RTTI + shared buffer cache.
INTERFACE zif_zodata_bopf_mapper PUBLIC.

  " ── Field-level mapping: external (OData) ↔ internal (BOPF) ──
  TYPES:
    BEGIN OF ty_field_map,
      ext_name TYPE string,     " OData / DDIC external field name (uppercased)
      int_name TYPE string,     " BOPF internal field name (uppercased)
      ext_idx  TYPE i,          " Positional index in external structure
      int_idx  TYPE i,          " Positional index in internal structure
    END OF ty_field_map,
    tt_field_map TYPE STANDARD TABLE OF ty_field_map WITH DEFAULT KEY.

  " ── Per-node change descriptor ────────────────────────────────
  " One entry per node instance to be modified (root, check, barrier …).
  " Caller fills: edit_mode OR change_mode, node/key fields, data refs.
  " field_map is optional: if initial, auto-resolved via RTTI cache.
  TYPES:
    BEGIN OF ty_change,
      "-- Operation
      edit_mode   TYPE c LENGTH 1,            " C=Create U=Update D=Delete (shorthand)
      change_mode TYPE /bobf/conf_change_mode, " Explicit BOBF constant (preferred)

      "-- Node identification
      node_key    TYPE /bobf/obm_node_key,    " Target BOPF node key (e.g. sc_node-root)
      key         TYPE /bobf/conf_key,        " Instance key (sysuuid_x16); auto-generated for C

      "-- Association context (needed for child nodes)
      source_key  TYPE /bobf/conf_key,        " Parent instance key
      source_node TYPE /bobf/obm_node_key,    " Parent node key
      association TYPE /bobf/obm_assoc_key,   " Association from parent → child node

      "-- Data references
      external    TYPE REF TO data,           " OData source structure (web contract)
      internal    TYPE REF TO data,           " BOPF target structure (pre-allocated by caller)

      "-- Field mapping (optional: auto-resolved when initial)
      field_map   TYPE tt_field_map,
    END OF ty_change,
    tt_change TYPE STANDARD TABLE OF ty_change WITH DEFAULT KEY.

  " ── Node configuration registry entry ─────────────────────────
  " Describes a known BOPF node: its key constants and DDIC types.
  " The mapper uses this to resolve node_key → internal structure type
  " automatically, so callers need not allocate internal refs manually.
  TYPES:
    BEGIN OF ty_node_config,
      node_key          TYPE /bobf/obm_node_key,  " BOPF node key constant
      int_struct_type   TYPE string,               " DDIC name of BOPF internal structure
      ext_struct_type   TYPE string,               " DDIC name of OData external structure
    END OF ty_node_config,
    tt_node_config TYPE STANDARD TABLE OF ty_node_config
                   WITH UNIQUE KEY node_key.

  " ── Main mapping method ────────────────────────────────────────
  " Converts tt_change → /bobf/t_frw_modification.
  " Caller passes lt_change, receives lt_modification ready for
  " /bobf/if_tra_service_manager~modify( it_modification = lt_mod ).
  METHODS map_to_modification
    IMPORTING
      !it_change         TYPE tt_change
    RETURNING
      VALUE(rt_modification) TYPE /bobf/t_frw_modification
    RAISING
      zcx_zodata_error.

  " ── Auto-change-list builder (from deep payload) ───────────────
  " High-level helper: given a structured SaveChanges request,
  " build the full tt_change list by iterating all delta tables.
  " The mapper owns node-config, so callers don't need BOPF constants.
  " Expected deep contract:
  "   - root/checks/barriers/participants/attachments form one delta payload
  "   - mutable rows carry explicit edit_mode = C|U|D
  "   - root-edit_mode is temporarily backward-compatible and may default to U
  METHODS build_change_list
    IMPORTING
      !is_request        TYPE zstr_pcct_savechanges_rq
    RETURNING
      VALUE(rt_change)   TYPE tt_change
    RAISING
      zcx_zodata_error.

ENDINTERFACE.
