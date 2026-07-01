CLASS zcx_zodata_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

PUBLIC SECTION.
  INTERFACES if_t100_message.

  DATA mv_code TYPE string READ-ONLY.
  DATA mv_msg TYPE string READ-ONLY.

  METHODS constructor
    IMPORTING
      !textid   LIKE if_t100_message=>t100key OPTIONAL
      !previous LIKE previous OPTIONAL
      !iv_code  TYPE string OPTIONAL
      !iv_msg   TYPE string OPTIONAL.
  METHODS get_code
    RETURNING
      VALUE(rv_code) TYPE string.
  METHODS get_message_text
    REDEFINITION.
ENDCLASS.

CLASS zcx_zodata_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    mv_code = iv_code.
    mv_msg  = iv_msg.
    IF textid IS INITIAL.
      if_t100_message~t100key = VALUE #(
        msgid = 'ZODATA_MSG' msgno = '000' attr1 = 'MV_MSG' attr2 = 'MV_CODE' ).
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD get_code.
    rv_code = mv_code.
  ENDMETHOD.

  METHOD get_message_text.
    rv_msg = COND #( WHEN if_t100_message~t100key-msgid IS NOT INITIAL
                     THEN message_text = if_t100_message~t100key( )
                     ELSE mv_msg ).
  ENDMETHOD.
ENDCLASS.
