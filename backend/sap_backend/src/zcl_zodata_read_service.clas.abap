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

    METHODS build_db_key_not_found_text
      IMPORTING
        iv_db_key TYPE sysuuid_x16
      RETURNING
        VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_zodata_read_service IMPLEMENTATION.
  METHOD constructor.
    mo_context = io_context.
  ENDMETHOD.

  METHOD build_db_key_not_found_text.
    rv_text = |{ 'Checklist root not found: ' }{ iv_db_key }|.
  ENDMETHOD.

  METHOD read_root_row.
    DATA(ls_root) = VALUE ty_root_row( ).
    DATA lt_checks_agg TYPE HASHED TABLE OF
      BEGIN OF STRUCTURE
        pcct_uuid TYPE sysuuid_x16,
        total TYPE int4,
        success TYPE int4,
      END OF STRUCTURE
      WITH UNIQUE KEY pcct_uuid.
    DATA lt_barriers_agg TYPE HASHED TABLE OF
      BEGIN OF STRUCTURE
        pcct_uuid TYPE sysuuid_x16,
        total TYPE int4,
        success TYPE int4,
      END OF STRUCTURE
      WITH UNIQUE KEY pcct_uuid.

    " Single optimized query with LEFT JOIN and aggregations (Code-to-Data)
    SELECT SINGLE z~pcct_uuid, z~checklist_id, z~lpc, z~lpc_text, z~status, z~integration_flag,
                  z~date_check, z~time_check, z~time_zone, z~equipment, z~bukrs,
                  z~observer_fullname, z~observer_perner, z~observer_position, z~observer_orgunit,
                  z~observed_fullname, z~observed_perner, z~observed_position, z~observed_orgunit,
                  z~location_key, z~location_name, z~location_text, z~changed_on, z~changed_by,
                  z~created_on, z~created_by, z~version_number, z~lock_owner, z~lock_session,
                  z~tab_session_id, z~lock_expires_at,
                  COUNT( c~check_uuid ) AS checks_total,
                  SUM( CASE WHEN c~result = @abap_true THEN 1 ELSE 0 END ) AS checks_success,
                  COUNT( b~barrier_uuid ) AS barriers_total,
                  SUM( CASE WHEN b~result = @abap_true THEN 1 ELSE 0 END ) AS barriers_success
      FROM ztodata_hdr AS z
      LEFT JOIN zpcct_check AS c ON z~pcct_uuid = c~pcct_uuid
      LEFT JOIN zpcct_barrier AS b ON z~pcct_uuid = b~pcct_uuid
      WHERE z~pcct_uuid = @iv_db_key
      GROUP BY z~pcct_uuid, z~checklist_id, z~lpc, z~lpc_text, z~status, z~integration_flag,
               z~date_check, z~time_check, z~time_zone, z~equipment, z~bukrs,
               z~observer_fullname, z~observer_perner, z~observer_position, z~observer_orgunit,
               z~observed_fullname, z~observed_perner, z~observed_position, z~observed_orgunit,
               z~location_key, z~location_name, z~location_text, z~changed_on, z~changed_by,
               z~created_on, z~created_by, z~version_number, z~lock_owner, z~lock_session,
               z~tab_session_id, z~lock_expires_at
      INTO CORRESPONDING FIELDS OF @ls_root.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid = 'CHECKLIST_NOT_FOUND'
          message = |{ build_db_key_not_found_text( iv_db_key ) }|.
    ENDIF.

    rs_root = ls_root.
  ENDMETHOD.

  METHOD read_root_rows.
    DATA lt_root_data TYPE STANDARD TABLE OF
      BEGIN OF STRUCTURE
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
        lock_owner TYPE syuname,
        lock_session TYPE string,
        tab_session_id TYPE string,
        lock_expires_at TYPE timestampl,
      END OF STRUCTURE.

    DATA lt_checks_agg TYPE HASHED TABLE OF
      BEGIN OF STRUCTURE
        pcct_uuid TYPE sysuuid_x16,
        total TYPE int4,
        success TYPE int4,
      END OF STRUCTURE
      WITH UNIQUE KEY pcct_uuid.
    DATA lt_barriers_agg TYPE HASHED TABLE OF\n      BEGIN OF STRUCTURE\n        pcct_uuid TYPE sysuuid_x16,\n        total TYPE int4,\n        success TYPE int4,\n      END OF STRUCTURE\n      WITH UNIQUE KEY pcct_uuid."}

    " Single optimized query with aggregations (Code-to-Data optimization)
    SELECT z~pcct_uuid, z~checklist_id, z~lpc, z~lpc_text, z~status, z~integration_flag,
           z~date_check, z~time_check, z~time_zone, z~equipment, z~bukrs,
           z~observer_fullname, z~observer_perner, z~observer_position, z~observer_orgunit,
           z~observed_fullname, z~observed_perner, z~observed_position, z~observed_orgunit,
           z~location_key, z~location_name, z~location_text, z~changed_on, z~changed_by,
           z~created_on, z~created_by, z~version_number, z~lock_owner, z~lock_session,
           z~tab_session_id, z~lock_expires_at,
           COUNT( c~check_uuid ) AS checks_total,
           SUM( CASE WHEN c~result = @abap_true THEN 1 ELSE 0 END ) AS checks_success,
           COUNT( b~barrier_uuid ) AS barriers_total,
           SUM( CASE WHEN b~result = @abap_true THEN 1 ELSE 0 END ) AS barriers_success
      FROM ztodata_hdr AS z
      LEFT JOIN zpcct_check AS c ON z~pcct_uuid = c~pcct_uuid
      LEFT JOIN zpcct_barrier AS b ON z~pcct_uuid = b~pcct_uuid
      GROUP BY z~pcct_uuid, z~checklist_id, z~lpc, z~lpc_text, z~status, z~integration_flag,
               z~date_check, z~time_check, z~time_zone, z~equipment, z~bukrs,
               z~observer_fullname, z~observer_perner, z~observer_position, z~observer_orgunit,
               z~observed_fullname, z~observed_perner, z~observed_position, z~observed_orgunit,
               z~location_key, z~location_name, z~location_text, z~changed_on, z~changed_by,
               z~created_on, z~created_by, z~version_number, z~lock_owner, z~lock_session,
               z~tab_session_id, z~lock_expires_at
      ORDER BY z~changed_on DESCENDING
      UP TO @500 ROWS
      INTO TABLE @lt_root_data.

    IF lt_root_data IS INITIAL.
      RETURN.
    ENDIF.

    " Populate root table with aggregated data
    LOOP AT lt_root_data ASSIGNING FIELD-SYMBOL(<ls_root_data>).
      APPEND INITIAL LINE TO rt_root ASSIGNING FIELD-SYMBOL(<ls_root>).
      <ls_root> = CORRESPONDING #( <ls_root_data> ).
    ENDLOOP.

    " Aggregated checks and barriers in single queries (no loops)
    SELECT pcct_uuid,
           COUNT( * ) AS total,
           SUM( CASE WHEN result = @abap_true THEN 1 ELSE 0 END ) AS success
      FROM zpcct_check
      GROUP BY pcct_uuid
      INTO TABLE @lt_checks_agg.

    SELECT pcct_uuid,
           COUNT( * ) AS total,
           SUM( CASE WHEN result = @abap_true THEN 1 ELSE 0 END ) AS success
      FROM zpcct_barrier
      GROUP BY pcct_uuid
      INTO TABLE @lt_barriers_agg.

    " Apply aggregations using hash table lookups (O(1) complexity)
    LOOP AT rt_root ASSIGNING FIELD-SYMBOL(<ls_root>).
      READ TABLE lt_checks_agg ASSIGNING FIELD-SYMBOL(<ls_checks_agg>) WITH TABLE KEY pcct_uuid = <ls_root>-pcct_uuid.
      IF sy-subrc = 0.
        <ls_root>-checks_total = <ls_checks_agg>-total.
        <ls_root>-checks_success = <ls_checks_agg>-success.
      ENDIF.
      READ TABLE lt_barriers_agg ASSIGNING FIELD-SYMBOL(<ls_barriers_agg>) WITH TABLE KEY pcct_uuid = <ls_root>-pcct_uuid.
      IF sy-subrc = 0.
        <ls_root>-barriers_total = <ls_barriers_agg>-total.
        <ls_root>-barriers_success = <ls_barriers_agg>-success.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_check_rows.
    IF iv_parent_key IS INITIAL.
      SELECT check_uuid AS key_uuid, pcct_uuid AS parent_key, checks_num, check_text AS text,
             comment_text, result, changed_on
        FROM zpcct_check
        INTO CORRESPONDING FIELDS OF TABLE @rt_checks.
      RETURN.
    ENDIF.

    SELECT check_uuid AS key_uuid, pcct_uuid AS parent_key, checks_num, check_text AS text,
           comment_text, result, changed_on
      FROM zpcct_check
      WHERE pcct_uuid = @iv_parent_key
      INTO CORRESPONDING FIELDS OF TABLE @rt_checks.
  ENDMETHOD.

  METHOD read_barrier_rows.
    IF iv_parent_key IS INITIAL.
      SELECT barrier_uuid AS key_uuid, pcct_uuid AS parent_key, barriers_num, barrier_text AS text,
             comment_text, result, changed_on
        FROM zpcct_barrier
        INTO CORRESPONDING FIELDS OF TABLE @rt_barriers.
      RETURN.
    ENDIF.

    SELECT barrier_uuid AS key_uuid, pcct_uuid AS parent_key, barriers_num, barrier_text AS text,
           comment_text, result, changed_on
      FROM zpcct_barrier
      WHERE pcct_uuid = @iv_parent_key
      INTO CORRESPONDING FIELDS OF TABLE @rt_barriers.
  ENDMETHOD.
ENDCLASS.
