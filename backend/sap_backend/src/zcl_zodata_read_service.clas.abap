CLASS zcl_zodata_read_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_root_row,
      pcct_uuid TYPE sysuuid_x16,
      checklist_id TYPE char20,
      lpc TYPE char10,
      lpc_text TYPE string,
      status TYPE char20,
      integration_flag TYPE abap_bool,
      date_check TYPE datum,
      time_check TYPE tims,
      time_zone TYPE string,
      equipment TYPE string,
      bukrs TYPE bukrs,
      observer_fullname TYPE string,
      observer_perner TYPE pernr_d,
      observer_position TYPE string,
      observer_orgunit TYPE string,
      observed_fullname TYPE string,
      observed_perner TYPE pernr_d,
      observed_position TYPE string,
      observed_orgunit TYPE string,
      location_key TYPE string,
      location_name TYPE string,
      location_text TYPE string,
      changed_on TYPE timestampl,
      changed_by TYPE syuname,
      created_on TYPE timestampl,
      created_by TYPE syuname,
      version_number TYPE int4,
      checks_total TYPE int4,
      checks_success TYPE int4,
      barriers_total TYPE int4,
      barriers_success TYPE int4,
      lock_owner TYPE syuname,
      lock_session TYPE string,
      tab_session_id TYPE string,
      lock_expires_at TYPE timestampl,
    END OF ty_root_row,
    tt_root_row TYPE STANDARD TABLE OF ty_root_row WITH DEFAULT KEY,
    BEGIN OF ty_check_row,
      key_uuid TYPE sysuuid_x16,
      parent_key TYPE sysuuid_x16,
      checks_num TYPE int4,
      text TYPE string,
      comment_text TYPE string,
      result TYPE abap_bool,
      changed_on TYPE timestampl,
    END OF ty_check_row,
    tt_check_row TYPE STANDARD TABLE OF ty_check_row WITH DEFAULT KEY,
    BEGIN OF ty_barrier_row,
      key_uuid TYPE sysuuid_x16,
      parent_key TYPE sysuuid_x16,
      barriers_num TYPE int4,
      text TYPE string,
      comment_text TYPE string,
      result TYPE abap_bool,
      changed_on TYPE timestampl,
    END OF ty_barrier_row,
    tt_barrier_row TYPE STANDARD TABLE OF ty_barrier_row WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        io_context TYPE REF TO /iwbep/if_mgw_conv_srv_runtime OPTIONAL.

    METHODS read_root_row
      IMPORTING
        iv_db_key TYPE sysuuid_x16
      RETURNING
        VALUE(rs_root) TYPE ty_root_row
      RAISING
        /iwbep/cx_mgw_busi_exception.

    METHODS read_root_rows
      RETURNING
        VALUE(rt_root) TYPE tt_root_row.

    METHODS read_check_rows
      IMPORTING
        iv_parent_key TYPE sysuuid_x16 OPTIONAL
      RETURNING
        VALUE(rt_checks) TYPE tt_check_row.

    METHODS read_barrier_rows
      IMPORTING
        iv_parent_key TYPE sysuuid_x16 OPTIONAL
      RETURNING
        VALUE(rt_barriers) TYPE tt_barrier_row.

  PRIVATE SECTION.
    DATA mo_context TYPE REF TO /iwbep/if_mgw_conv_srv_runtime.

    METHODS raise_busi_exception
      IMPORTING
        iv_text TYPE string
        iv_code TYPE string
      RAISING
        zcx_zodata_error.
ENDCLASS.

CLASS zcl_zodata_read_service IMPLEMENTATION.
  METHOD constructor.
    mo_context = io_context.
  ENDMETHOD.

  METHOD raise_busi_exception.
    RAISE EXCEPTION TYPE zcx_zodata_error
      EXPORTING
        iv_code = iv_code
        iv_msg  = iv_text.
  ENDMETHOD.

  METHOD read_root_row.
    TYPES: BEGIN OF ty_check_agg,
      pcct_uuid TYPE sysuuid_x16,
      total TYPE int4,
      success TYPE int4,
    END OF ty_check_agg.
    TYPES: BEGIN OF ty_barrier_agg,
      pcct_uuid TYPE sysuuid_x16,
      total TYPE int4,
      success TYPE int4,
    END OF ty_barrier_agg.
    DATA ls_check_agg TYPE ty_check_agg.
    DATA ls_barrier_agg TYPE ty_barrier_agg.

    SELECT SINGLE pcct_uuid checklist_id lpc lpc_text status integration_flag date_check time_check time_zone equipment bukrs observer_fullname observer_perner observer_position observer_orgunit observed_fullname observed_perner observed_position observed_orgunit location_key location_name location_text changed_on changed_by created_on created_by version_number lock_owner lock_session tab_session_id lock_expires_at
      FROM ztodata_hdr
      INTO CORRESPONDING FIELDS OF @rs_root
      WHERE pcct_uuid = @iv_db_key.
    IF sy-subrc <> 0.
    raise_busi_exception( iv_text = |ChecklistRoot not found for key { iv_db_key }.| iv_code = zif_zodata_message_codes=>validation_error ).
    ENDIF.

    SELECT SINGLE pcct_uuid,
                  COUNT( * ) AS total,
                  SUM( CASE WHEN result = @abap_true THEN 1 ELSE 0 END ) AS success
      FROM zpcct_check
      WHERE pcct_uuid = @iv_db_key
      GROUP BY pcct_uuid
      INTO @ls_check_agg.
    IF sy-subrc = 0.
      rs_root-checks_total = ls_check_agg-total.
      rs_root-checks_success = ls_check_agg-success.
    ENDIF.

    SELECT SINGLE pcct_uuid,
                  COUNT( * ) AS total,
                  SUM( CASE WHEN result = @abap_true THEN 1 ELSE 0 END ) AS success
      FROM zpcct_barrier
      WHERE pcct_uuid = @iv_db_key
      GROUP BY pcct_uuid
      INTO @ls_barrier_agg.
    IF sy-subrc = 0.
      rs_root-barriers_total = ls_barrier_agg-total.
      rs_root-barriers_success = ls_barrier_agg-success.
    ENDIF.
  ENDMETHOD.

  METHOD read_root_rows.
    TYPES: BEGIN OF ty_check_agg,
      pcct_uuid TYPE sysuuid_x16,
      total TYPE int4,
      success TYPE int4,
    END OF ty_check_agg,
    tt_check_agg TYPE HASHED TABLE OF ty_check_agg WITH UNIQUE KEY pcct_uuid.
    TYPES: BEGIN OF ty_barrier_agg,
      pcct_uuid TYPE sysuuid_x16,
      total TYPE int4,
      success TYPE int4,
    END OF ty_barrier_agg,
    tt_barrier_agg TYPE HASHED TABLE OF ty_barrier_agg WITH UNIQUE KEY pcct_uuid.
    DATA lt_check_agg TYPE tt_check_agg.
    DATA lt_barrier_agg TYPE tt_barrier_agg.
    CONSTANTS lc_root_read_limit TYPE i VALUE 500.

    SELECT pcct_uuid checklist_id lpc lpc_text status integration_flag date_check time_check time_zone equipment bukrs observer_fullname observer_perner observer_position observer_orgunit observed_fullname observed_perner observed_position observed_orgunit location_key location_name location_text changed_on changed_by created_on created_by version_number lock_owner lock_session tab_session_id lock_expires_at
      FROM ztodata_hdr
      INTO CORRESPONDING FIELDS OF TABLE @rt_root
      ORDER BY changed_on DESCENDING
      UP TO @lc_root_read_limit ROWS.
    IF rt_root IS INITIAL.
      RETURN.
    ENDIF.

    SELECT pcct_uuid,
           COUNT( * ) AS total,
           SUM( CASE WHEN result = @abap_true THEN 1 ELSE 0 END ) AS success
      FROM zpcct_check
      GROUP BY pcct_uuid
      INTO TABLE @lt_check_agg.

    SELECT pcct_uuid,
           COUNT( * ) AS total,
           SUM( CASE WHEN result = @abap_true THEN 1 ELSE 0 END ) AS success
      FROM zpcct_barrier
      GROUP BY pcct_uuid
      INTO TABLE @lt_barrier_agg.

    LOOP AT rt_root ASSIGNING FIELD-SYMBOL(<ls_root>).
      READ TABLE lt_check_agg ASSIGNING FIELD-SYMBOL(<ls_check_agg>) WITH TABLE KEY pcct_uuid = <ls_root>-pcct_uuid.
      IF sy-subrc = 0.
        <ls_root>-checks_total = <ls_check_agg>-total.
        <ls_root>-checks_success = <ls_check_agg>-success.
      ENDIF.
      READ TABLE lt_barrier_agg ASSIGNING FIELD-SYMBOL(<ls_barrier_agg>) WITH TABLE KEY pcct_uuid = <ls_root>-pcct_uuid.
      IF sy-subrc = 0.
        <ls_root>-barriers_total = <ls_barrier_agg>-total.
        <ls_root>-barriers_success = <ls_barrier_agg>-success.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_check_rows.
    IF iv_parent_key IS INITIAL.
      SELECT check_uuid AS key_uuid pcct_uuid AS parent_key checks_num check_text AS text comment_text result changed_on
        FROM zpcct_check
        INTO CORRESPONDING FIELDS OF TABLE @rt_checks.
      RETURN.
    ENDIF.
    SELECT check_uuid AS key_uuid pcct_uuid AS parent_key checks_num check_text AS text comment_text result changed_on
      FROM zpcct_check
      INTO CORRESPONDING FIELDS OF TABLE @rt_checks
      WHERE pcct_uuid = @iv_parent_key.
  ENDMETHOD.

  METHOD read_barrier_rows.
    IF iv_parent_key IS INITIAL.
      SELECT barrier_uuid AS key_uuid pcct_uuid AS parent_key barriers_num barrier_text AS text comment_text result changed_on
        FROM zpcct_barrier
        INTO CORRESPONDING FIELDS OF TABLE @rt_barriers.
      RETURN.
    ENDIF.
    SELECT barrier_uuid AS key_uuid pcct_uuid AS parent_key barriers_num barrier_text AS text comment_text result changed_on
      FROM zpcct_barrier
      INTO CORRESPONDING FIELDS OF TABLE @rt_barriers
      WHERE pcct_uuid = @iv_parent_key.
  ENDMETHOD.
ENDCLASS.
