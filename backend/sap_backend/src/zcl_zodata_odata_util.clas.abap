CLASS zcl_zodata_odata_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get_uuid_from_it_key_tab
      IMPORTING
        !it_key_tab TYPE /iwbep/t_mgw_name_value_pair
        !iv_name    TYPE string
      RETURNING
        VALUE(rv_uuid) TYPE sysuuid_x16
      RAISING
        zcx_zodata_error.
ENDCLASS.

CLASS zcl_zodata_odata_util IMPLEMENTATION.
  METHOD get_uuid_from_it_key_tab.
    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<ls_key>) WITH KEY name = iv_name.
    IF sy-subrc <> 0 OR <ls_key>-value IS INITIAL.
      RAISE EXCEPTION TYPE zcx_zodata_error
        EXPORTING iv_msg = |Key { iv_name } not found in request|.
    ENDIF.

    TRY.
        rv_uuid = <ls_key>-value.
      CATCH cx_sy_conversion_no_uuid.
        RAISE EXCEPTION TYPE zcx_zodata_error
          EXPORTING iv_msg = |Invalid UUID for key { iv_name }|.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
