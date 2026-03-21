CLASS zcl_zodata_runtime_settings_svc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_runtime_settings,
             environment TYPE string,
             heartbeat_ms TYPE int4,
             idle_ms TYPE int4,
             autosave_interval_ms TYPE int4,
             lock_refresh_ms TYPE int4,
             analytics_refresh_ms TYPE int4,
             gcd_interval_ms TYPE int4,
             network_grace_ms TYPE int4,
             cache_tolerance_ms TYPE int4,
             frontend_variables_json TYPE string,
             required_fields_json TYPE string,
             upload_policy_json TYPE string,
             permission_rules_json TYPE string,
           END OF ty_runtime_settings.

    METHODS read_runtime_settings
      RETURNING
        VALUE(rs_result) TYPE ty_runtime_settings.

    METHODS read_frontend_variables_json
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS read_required_fields_json
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS read_upload_policy_json
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS read_permission_rules_json
      RETURNING
        VALUE(rv_result) TYPE string.

  PRIVATE SECTION.
    METHODS read_runtime_settings_source
      RETURNING
        VALUE(rs_result) TYPE ty_runtime_settings.

    METHODS apply_fallback_defaults
      CHANGING
        cs_result TYPE ty_runtime_settings.
ENDCLASS.

CLASS zcl_zodata_runtime_settings_svc IMPLEMENTATION.
  METHOD read_runtime_settings.
    rs_result = read_runtime_settings_source( ).
    rs_result-frontend_variables_json = read_frontend_variables_json( ).
    rs_result-required_fields_json = read_required_fields_json( ).
    rs_result-upload_policy_json = read_upload_policy_json( ).
    rs_result-permission_rules_json = read_permission_rules_json( ).
    apply_fallback_defaults( CHANGING cs_result = rs_result ).
  ENDMETHOD.

  METHOD read_runtime_settings_source.
    " Integration seam: read active customizing from CDS/table source.
    " Target DDLS examples stored in:
    "   ZC_PCCT_RuntimeSettings
    "   ZC_PCCT_FrontendVariables
    "   ZC_PCCT_RequiredFields
    "   ZC_PCCT_UploadPolicy
    "   ZC_PCCT_PermissionRules
  ENDMETHOD.

  METHOD read_frontend_variables_json.
    rv_result = ``.
  ENDMETHOD.

  METHOD read_required_fields_json.
    rv_result = ``.
  ENDMETHOD.

  METHOD read_upload_policy_json.
    rv_result = ``.
  ENDMETHOD.

  METHOD read_permission_rules_json.
    rv_result = `{}`.
  ENDMETHOD.

  METHOD apply_fallback_defaults.
    IF cs_result-environment IS INITIAL.
      cs_result-environment = 'production'.
    ENDIF.
    IF cs_result-heartbeat_ms IS INITIAL.
      cs_result-heartbeat_ms = 270000.
    ENDIF.
    IF cs_result-idle_ms IS INITIAL.
      cs_result-idle_ms = 570000.
    ENDIF.
    IF cs_result-autosave_interval_ms IS INITIAL.
      cs_result-autosave_interval_ms = 150000.
    ENDIF.
    IF cs_result-lock_refresh_ms IS INITIAL.
      cs_result-lock_refresh_ms = 150000.
    ENDIF.
    IF cs_result-analytics_refresh_ms IS INITIAL.
      cs_result-analytics_refresh_ms = 900000.
    ENDIF.
    IF cs_result-gcd_interval_ms IS INITIAL.
      cs_result-gcd_interval_ms = 30000.
    ENDIF.
    IF cs_result-network_grace_ms IS INITIAL.
      cs_result-network_grace_ms = 15000.
    ENDIF.
    IF cs_result-cache_tolerance_ms IS INITIAL.
      cs_result-cache_tolerance_ms = 5500.
    ENDIF.
    IF cs_result-permission_rules_json IS INITIAL.
      cs_result-permission_rules_json = `{}`.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
