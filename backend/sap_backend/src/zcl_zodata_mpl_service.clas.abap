CLASS zcl_zodata_mpl_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS read_tree
      IMPORTING
        iv_date TYPE datum
      RETURNING
        VALUE(rt_tree) TYPE ztt_pcct_mpl_tree
      RAISING
        zcx_zodata_error.
ENDCLASS.

CLASS zcl_zodata_mpl_service IMPLEMENTATION.
  METHOD read_tree.
    CALL FUNCTION 'Z_PCCT_MPL_TREE_GET'
      EXPORTING
        iv_date = iv_date
      TABLES
        et_tree = rt_tree
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_zodata_error
        EXPORTING
          iv_code = zif_zodata_message_codes=>technical_error
      iv_msg  = |{ zcl_zodata_message_texts=>c_msg_mpl_tree_read_failed_prefix } { iv_date }.|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
