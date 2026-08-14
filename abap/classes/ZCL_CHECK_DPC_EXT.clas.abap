"! <p class="shorttext synchronized" lang="en">Data Provider Class extension for ZCHECK_SRV</p>
"! Classic (non-RAP) Gateway OData V2 service. Redefines only the methods
"! that need logic not expressible declaratively in CDS; plain reads stay on
"! the generated SADL redirect to ZI_CheckRoot/ZI_CheckItem/...
"!
"! Security: all DB access here is static SQL (SELECT/UPDATE/INSERT ... FROM
"! with typed structures). No dynamic SQL exists or should be introduced
"! without CL_ABAP_DYN_PRG=>check_column_name/check_table_name_str.
"!
"! Exact redefinition signatures and the mo_model attribute name depend on
"! the generated parent (ZCL_CHECK_DPC, SEGW wizard) — verify on activation.
CLASS zcl_check_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_check_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS checkroots_create_deep_entity REDEFINITION.
    METHODS checkroots_get_entity         REDEFINITION.
    METHODS checkroots_get_entityset      REDEFINITION.
    METHODS checkroots_update_entity      REDEFINITION.
    METHODS checkbasics_update_entity     REDEFINITION.
    METHODS checkitems_create_entity      REDEFINITION.
    METHODS checkitems_update_entity      REDEFINITION.
    METHODS barriers_create_entity        REDEFINITION.
    METHODS barriers_update_entity        REDEFINITION.

  PRIVATE SECTION.
    TYPES: tt_fieldname TYPE HASHED TABLE OF fieldname WITH UNIQUE KEY table_line.

    TYPES: BEGIN OF ty_readonly_cache_entry,
             entity_set_name TYPE string,
             fields          TYPE tt_fieldname,
           END OF ty_readonly_cache_entry.

    "! Per-process cache of readonly fields, keyed by entity set name.
    "! Populated once from the service metamodel, not hardcoded.
    CLASS-DATA gt_readonly_cache TYPE HASHED TABLE OF ty_readonly_cache_entry WITH UNIQUE KEY entity_set_name.

    "! 'X' = satisfactory. Same literal as ZI_CheckItemAgg/ZI_BarrierAgg,
    "! serve_config.py RESULT_CODE_SATISFACTORY and Constants.js
    "! RESULT_CODES.SATISFACTORY — an unavoidable cross-stack SSOT cost
    "! (see abap/README.md).
    CONSTANTS c_result_satisfactory TYPE zchk_item-result VALUE 'X'.

    "! Source of truth is the service metamodel (sap:updatable), not a manual list.
    METHODS get_readonly_fields
      IMPORTING
        iv_entity_set_name  TYPE string
      RETURNING
        VALUE(rt_fieldname) TYPE tt_fieldname.

    "! Removes any field from it_changed_fields that is in get_readonly_fields( ).
    METHODS filter_readonly_fields
      IMPORTING
        iv_entity_set_name TYPE string
        it_changed_fields  TYPE /iwbep/t_mgw_name_value_pair
      RETURNING
        VALUE(rt_fields)   TYPE /iwbep/t_mgw_name_value_pair.

    METHODS validate_required_fields
      IMPORTING
        is_root  TYPE zchk_root
        is_basic TYPE zchk_basic
      RAISING
        /iwbep/cx_mgw_busi_exception.

    "! Conflict of interest: an inspector cannot inspect themselves. Compared
    "! by Pernr, not by full name (names can coincide between different people).
    METHODS validate_no_conflict_of_interest
      IMPORTING
        is_basic TYPE zchk_basic
      RAISING
        /iwbep/cx_mgw_busi_exception.

    "! Single entry point for both business rules, called from both CREATE and
    "! UPDATE — the reason it exists separately from the two methods above.
    METHODS validate_basic_business_rules
      IMPORTING
        is_root  TYPE zchk_root
        is_basic TYPE zchk_basic
      RAISING
        /iwbep/cx_mgw_busi_exception.

    "! Builds the post-patch state of ZCHK_BASIC (current DB row + the changed,
    "! already readonly-filtered fields), so validation runs against the
    "! resulting record, not only the delta.
    METHODS build_basic_after_patch
      IMPORTING
        iv_root_id      TYPE zchk_root-root_id
        it_changed_safe TYPE /iwbep/t_mgw_name_value_pair
      RETURNING
        VALUE(rs_basic) TYPE zchk_basic.

    "! Not in CDS: string formatting is fragile to format changes and the same
    "! format independently lives in ext/util/Constants.js/i18n.properties for
    "! the transient (unsaved) object case.
    METHODS format_kpi_title
      IMPORTING
        iv_lpc_text     TYPE zchk_basic-lpc_key
        iv_prof_text    TYPE zchk_basic-prof_key
      RETURNING
        VALUE(rv_title) TYPE string.

    METHODS format_kpi_subtitle
      IMPORTING
        iv_checks_success  TYPE i
        iv_barriers_amount TYPE i
      RETURNING
        VALUE(rv_subtitle) TYPE string.

    "! Single point for the composite ETag (AMDP push-down) — used by both
    "! GET_ENTITY (one row) and GET_ENTITYSET (one batched call per page).
    METHODS resolve_composite_etags
      IMPORTING
        it_root_id            TYPE zcl_check_root_etag_amdp=>tt_root_id
      RETURNING
        VALUE(rt_etag_result) TYPE zcl_check_root_etag_amdp=>tt_etag_result.

    METHODS get_next_doc_id
      RETURNING
        VALUE(rv_doc_id) TYPE zchk_root-doc_id.

ENDCLASS.


CLASS zcl_check_dpc_ext IMPLEMENTATION.

  METHOD get_readonly_fields.
    ASSIGN gt_readonly_cache[ entity_set_name = iv_entity_set_name ] TO FIELD-SYMBOL(<ls_cached>).
    IF sy-subrc = 0.
      rt_fieldname = <ls_cached>-fields.
      RETURN.
    ENDIF.

    DATA(lo_entity_type) = mo_model->get_entity_type( iv_entity_name = iv_entity_set_name ).
    IF lo_entity_type IS BOUND.
      rt_fieldname = VALUE #(
        FOR lo_property IN lo_entity_type->get_properties( )
        WHERE ( updatable = abap_false )
        ( CONV fieldname( lo_property->get_name( ) ) ) ).
    ENDIF.

    INSERT VALUE #( entity_set_name = iv_entity_set_name fields = rt_fieldname ) INTO TABLE gt_readonly_cache.
  ENDMETHOD.


  METHOD filter_readonly_fields.
    DATA(lt_readonly) = get_readonly_fields( iv_entity_set_name ).
    rt_fields = VALUE #(
      FOR ls_field IN it_changed_fields
      WHERE ( name NOT IN lt_readonly )
      ( ls_field ) ).
  ENDMETHOD.


  METHOD validate_required_fields.
    DATA(lt_missing) = VALUE string_table(
      ( COND #( WHEN is_basic-observer_fullname IS INITIAL AND is_root-observer_fullname IS INITIAL THEN 'ФИО инспектора' ) )
      ( COND #( WHEN is_basic-observed_fullname IS INITIAL AND is_root-observed_fullname IS INITIAL THEN 'ФИО проверяемого' ) )
      ( COND #( WHEN is_basic-date     IS INITIAL THEN 'Дата проверки' ) )
      ( COND #( WHEN is_basic-time     IS INITIAL THEN 'Время проверки' ) )
      ( COND #( WHEN is_basic-timezone IS INITIAL THEN 'Часовой пояс' ) )
      ( COND #( WHEN is_basic-lpc_key  IS INITIAL THEN 'Уровень КПР' ) )
      ( COND #( WHEN is_basic-prof_key IS INITIAL THEN 'Профессия' ) ) ).

    DELETE lt_missing WHERE table_line IS INITIAL.
    CHECK lt_missing IS NOT INITIAL.

    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = |Не заполнены обязательные поля: { concat_lines_of( table = lt_missing sep = ', ' ) }|.
  ENDMETHOD.


  METHOD validate_no_conflict_of_interest.
    CHECK is_basic-observer_pernr IS NOT INITIAL
      AND is_basic-observer_pernr = is_basic-observed_pernr.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Инспектор не может проверять сам себя (конфликт интересов)'.
  ENDMETHOD.


  METHOD validate_basic_business_rules.
    validate_required_fields( is_root = is_root is_basic = is_basic ).
    validate_no_conflict_of_interest( is_basic ).
  ENDMETHOD.


  METHOD build_basic_after_patch.
    SELECT SINGLE * FROM zchk_basic WHERE root_id = @iv_root_id INTO @rs_basic.

    " RTTI-based assignment by component name — typed, not a string-built
    " statement, so there is no injection surface here.
    DATA(lo_struct) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( rs_basic ) ).
    LOOP AT it_changed_safe INTO DATA(ls_changed).
      TRY.
          lo_struct->get_component( to_upper( ls_changed-name ) ).
          ASSIGN COMPONENT ls_changed-name OF STRUCTURE rs_basic TO FIELD-SYMBOL(<lv_target>).
          IF sy-subrc = 0.
            <lv_target> = ls_changed-value.
          ENDIF.
        CATCH cx_sy_move_cast_error.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD format_kpi_title.
    DATA(lv_lpc)  = COND string( WHEN iv_lpc_text  IS NOT INITIAL THEN iv_lpc_text  ELSE '—' ).
    DATA(lv_prof) = COND string( WHEN iv_prof_text IS NOT INITIAL THEN iv_prof_text ELSE '—' ).
    rv_title = |КПР: { lv_lpc } · Профессия: { lv_prof }|.
  ENDMETHOD.


  METHOD format_kpi_subtitle.
    rv_subtitle = |Успешных проверок: { iv_checks_success } · Барьеров: { iv_barriers_amount }|.
  ENDMETHOD.


  METHOD resolve_composite_etags.
    zcl_check_root_etag_amdp=>get_etag_timestamps(
      EXPORTING it_root_id     = it_root_id
      IMPORTING et_etag_result = rt_etag_result ).
  ENDMETHOD.


  METHOD get_next_doc_id.
    DATA(lv_number) = cl_numberrange_runtime=>number_get_next(
      nr_range_nr = '01'
      object      = 'ZCHK_DOC' ).
    rv_doc_id = |DOC-{ lv_number WIDTH = 6 ALIGN = RIGHT PAD = '0' }|.
  ENDMETHOD.


  METHOD checkroots_create_deep_entity.
    io_data_provider->read_entry_data( IMPORTING es_data = DATA(ls_deep) ).
    DATA(ls_root)  = CORRESPONDING zchk_root( ls_deep ).
    DATA(ls_basic) = CORRESPONDING zchk_basic( ls_deep-to_basic ).
    DATA(lt_items) = CORRESPONDING TABLE OF zchk_item( ls_deep-to_checks ).
    DATA(lt_barrs) = CORRESPONDING TABLE OF zchk_barr( ls_deep-to_barriers ).

    validate_basic_business_rules( is_root = ls_root is_basic = ls_basic ).

    TRY.
        ls_root-root_id = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(lx_uuid).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING iv_msg_type = 'E' iv_msg_text = lx_uuid->get_text( ).
    ENDTRY.

    GET TIME STAMP FIELD DATA(lv_now).
    ls_root = VALUE #( BASE ls_root
      doc_id = get_next_doc_id( ) status = 'OK' this_is_integration_data = abap_false
      created_by = sy-uname created_at = lv_now last_changed_at = lv_now ).
    ls_basic-root_id         = ls_root-root_id.
    ls_basic-last_changed_at = lv_now.

    lt_items = VALUE #( FOR ls_item IN lt_items ( CORRESPONDING #( BASE ( ls_item )
      root_id = ls_root-root_id item_id = cl_system_uuid=>create_uuid_x16_static( ) last_changed_at = lv_now ) ) ).
    lt_barrs = VALUE #( FOR ls_barr IN lt_barrs ( CORRESPONDING #( BASE ( ls_barr )
      root_id = ls_root-root_id barrier_id = cl_system_uuid=>create_uuid_x16_static( ) last_changed_at = lv_now ) ) ).

    INSERT zchk_root FROM ls_root.
    INSERT zchk_basic FROM ls_basic.
    IF lt_items IS NOT INITIAL.
      INSERT zchk_item FROM TABLE lt_items.
    ENDIF.
    IF lt_barrs IS NOT INITIAL.
      INSERT zchk_barr FROM TABLE lt_barrs.
    ENDIF.

    " Response goes through ZI_CheckRoot (not the raw ls_root) so KPI/criticality/
    " hidden fields are push-down computed the same way as on a plain GET.
    SELECT SINGLE * FROM zi_checkroot
      WHERE rootid = @ls_root-root_id
      INTO CORRESPONDING FIELDS OF @DATA(ls_response).
    ls_response-headerkpititle    = format_kpi_title( iv_lpc_text = ls_basic-lpc_key iv_prof_text = ls_basic-prof_key ).
    ls_response-headerkpisubtitle = format_kpi_subtitle( iv_checks_success = 0 iv_barriers_amount = lines( lt_barrs ) ).

    CREATE DATA er_deep_entity LIKE ls_response.
    ASSIGN er_deep_entity->* TO FIELD-SYMBOL(<ls_deep_entity>).
    <ls_deep_entity> = ls_response.
  ENDMETHOD.


  METHOD checkroots_get_entity.
    super->checkroots_get_entity(
      EXPORTING
        iv_entity_name           = iv_entity_name
        iv_entity_set_name       = iv_entity_set_name
        iv_source_name           = iv_source_name
        it_filter_select_options = it_filter_select_options
        it_key_tab               = it_key_tab
        it_navigation_path       = it_navigation_path
        io_tech_request_context  = io_tech_request_context
      IMPORTING
        er_entity = er_entity ).

    LOOP AT resolve_composite_etags( VALUE #( ( root_id = er_entity-rootid ) ) ) INTO DATA(ls_etag).
      er_entity-lastchangedat = ls_etag-max_changed_at.
    ENDLOOP.

    er_entity-headerkpititle    = format_kpi_title( iv_lpc_text = er_entity-lpctext iv_prof_text = er_entity-proftext ).
    er_entity-headerkpisubtitle = format_kpi_subtitle(
      iv_checks_success  = er_entity-checkssuccess
      iv_barriers_amount = er_entity-barriersamount ).
  ENDMETHOD.


  METHOD checkroots_get_entityset.
    super->checkroots_get_entityset(
      EXPORTING
        iv_entity_name           = iv_entity_name
        iv_entity_set_name       = iv_entity_set_name
        iv_source_name           = iv_source_name
        it_filter_select_options = it_filter_select_options
        it_order                 = it_order
        is_paging                = is_paging
        it_navigation_path       = it_navigation_path
        io_tech_request_context  = io_tech_request_context
      IMPORTING
        et_entityset = et_entityset ).

    IF et_entityset IS INITIAL.
      RETURN.
    ENDIF.

    " One AMDP call for the whole result page, not one per row.
    DATA(lt_etag) = resolve_composite_etags(
      VALUE #( FOR ls_row IN et_entityset ( root_id = ls_row-rootid ) ) ).

    LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<ls_row>).
      READ TABLE lt_etag INTO DATA(ls_etag) WITH KEY root_id = <ls_row>-rootid.
      IF sy-subrc = 0.
        <ls_row>-lastchangedat = ls_etag-max_changed_at.
      ENDIF.
      <ls_row>-headerkpititle    = format_kpi_title( iv_lpc_text = <ls_row>-lpctext iv_prof_text = <ls_row>-proftext ).
      <ls_row>-headerkpisubtitle = format_kpi_subtitle(
        iv_checks_success  = <ls_row>-checkssuccess
        iv_barriers_amount = <ls_row>-barriersamount ).
    ENDLOOP.
  ENDMETHOD.


  METHOD checkroots_update_entity.
    io_data_provider->read_entry_data( IMPORTING es_data = DATA(ls_input) ).
    DATA(lv_root_id) = CONV zchk_root-root_id( it_key_tab[ name = 'RootId' ]-value ).
    DATA(lt_safe) = filter_readonly_fields(
      iv_entity_set_name = 'CheckRoots'
      it_changed_fields  = io_data_provider->get_changed_fields( ) ).

    " Basic-proxy fields sent via CheckRoots must pass the same business rules
    " as a direct PATCH on CheckBasics.
    DATA(ls_basic_after) = build_basic_after_patch( iv_root_id = lv_root_id it_changed_safe = lt_safe ).
    validate_basic_business_rules( is_root = VALUE #( root_id = lv_root_id ) is_basic = ls_basic_after ).

    UPDATE zchk_root SET last_changed_at = cl_abap_context_info=>get_system_time( ) WHERE root_id = lv_root_id.
    UPDATE zchk_basic FROM ls_basic_after.
  ENDMETHOD.


  METHOD checkbasics_update_entity.
    DATA(lv_root_id) = CONV zchk_root-root_id( it_key_tab[ name = 'RootId' ]-value ).
    DATA(lt_safe) = filter_readonly_fields(
      iv_entity_set_name = 'CheckBasics'
      it_changed_fields  = io_data_provider->get_changed_fields( ) ).

    DATA(ls_basic_after) = build_basic_after_patch( iv_root_id = lv_root_id it_changed_safe = lt_safe ).
    validate_basic_business_rules( is_root = VALUE #( root_id = lv_root_id ) is_basic = ls_basic_after ).

    UPDATE zchk_basic FROM ls_basic_after.
  ENDMETHOD.


  METHOD checkitems_create_entity.
    io_data_provider->read_entry_data( IMPORTING es_data = DATA(ls_item) ).
    TRY.
        ls_item-item_id = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(lx_uuid).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING iv_msg_type = 'E' iv_msg_text = lx_uuid->get_text( ).
    ENDTRY.
    GET TIME STAMP FIELD ls_item-last_changed_at.
    INSERT zchk_item FROM ls_item.
    er_entity = CORRESPONDING #( ls_item ).
  ENDMETHOD.


  METHOD checkitems_update_entity.
    DATA(lt_safe) = filter_readonly_fields(
      iv_entity_set_name = 'CheckItems'
      it_changed_fields  = io_data_provider->get_changed_fields( ) ).
    " UPDATE zchk_item по lt_safe — required/conflict rules do not apply to child rows.
  ENDMETHOD.


  METHOD barriers_create_entity.
    io_data_provider->read_entry_data( IMPORTING es_data = DATA(ls_barr) ).
    TRY.
        ls_barr-barrier_id = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(lx_uuid).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING iv_msg_type = 'E' iv_msg_text = lx_uuid->get_text( ).
    ENDTRY.
    GET TIME STAMP FIELD ls_barr-last_changed_at.
    INSERT zchk_barr FROM ls_barr.
    er_entity = CORRESPONDING #( ls_barr ).
  ENDMETHOD.


  METHOD barriers_update_entity.
    DATA(lt_safe) = filter_readonly_fields(
      iv_entity_set_name = 'Barriers'
      it_changed_fields  = io_data_provider->get_changed_fields( ) ).
    " UPDATE zchk_barr по lt_safe.
  ENDMETHOD.

ENDCLASS.
