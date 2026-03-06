CLASS zcl_zodata_bopf_msg_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS raise_on_failed_keys
      IMPORTING
        !it_failed_key TYPE /bobf/t_frw_key
        !it_message    TYPE /bobf/t_frw_message_k
      RAISING
        /iwbep/cx_mgw_busi_exception.
ENDCLASS.

CLASS zcl_zodata_bopf_msg_helper IMPLEMENTATION.
  METHOD raise_on_failed_keys.
    IF it_failed_key IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_text) = |BOPF modify failed. Failed rows: { lines( it_failed_key ) }.|.

    IF it_message IS NOT INITIAL.
      READ TABLE it_message ASSIGNING FIELD-SYMBOL(<ls_msg>) INDEX 1.
      IF sy-subrc = 0.
        lv_text = |{ lv_text } { <ls_msg>-message->if_message~get_text( ) }|.
      ENDIF.
    ENDIF.

    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid  = /iwbep/cx_mgw_busi_exception=>business_error
        message = lv_text.
  ENDMETHOD.
ENDCLASS.
