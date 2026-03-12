"! <p class="shorttext synchronized">BOPF Message Helper – Production Control Checklist</p>
"! Converts BOPF et_message / et_failed_key into Gateway exceptions
"! and structured message lists for client response.
CLASS zcl_zodata_bopf_msg_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    "-- Raise /iwbep/cx_mgw_busi_exception if lt_failed_key is not empty.
    "-- Collects all BOPF messages into the exception text.
    CLASS-METHODS raise_on_failed_keys
      IMPORTING
        !it_failed_key TYPE /bobf/t_frw_key
        !it_message    TYPE /bobf/t_frw_message_k
      RAISING
        /iwbep/cx_mgw_busi_exception.

    "-- Collect BOPF messages into a flat string table for response payloads.
    CLASS-METHODS collect_messages
      IMPORTING
        !it_message        TYPE /bobf/t_frw_message_k
        !iv_include_types  TYPE string DEFAULT 'EWI'   "E=Error W=Warn I=Info
      RETURNING
        VALUE(rt_texts)    TYPE string_table.

ENDCLASS.

CLASS zcl_zodata_bopf_msg_helper IMPLEMENTATION.

  METHOD raise_on_failed_keys.
    IF it_failed_key IS INITIAL.
      RETURN.
    ENDIF.

    DATA lv_summary TYPE string.
    lv_summary = |BOPF modify failed: { lines( it_failed_key ) } row(s) rejected.|.

    " Append first 5 error messages for diagnostics
    DATA lv_count TYPE i VALUE 0.
    LOOP AT it_message ASSIGNING FIELD-SYMBOL(<ls_msg>)
      WHERE message->if_message~get_message_attributes( )-msg_type CA 'EX'.
      lv_count = lv_count + 1.
      IF lv_count > 5. EXIT. ENDIF.
      lv_summary = lv_summary && | [{ lv_count }] { <ls_msg>-message->if_message~get_text( ) }|.
    ENDLOOP.

    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid  = /iwbep/cx_mgw_busi_exception=>business_error
        message = lv_summary.
  ENDMETHOD.

  METHOD collect_messages.
    LOOP AT it_message ASSIGNING FIELD-SYMBOL(<ls_msg>).
      DATA(lv_type) = <ls_msg>-message->if_message~get_message_attributes( )-msg_type.
      IF lv_type CA iv_include_types.
        APPEND <ls_msg>-message->if_message~get_text( ) TO rt_texts.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
