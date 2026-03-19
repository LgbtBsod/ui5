CLASS zcx_zodata_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA mv_code TYPE string READ-ONLY.
    DATA mv_msg TYPE string READ-ONLY.

    METHODS constructor
      IMPORTING
        !iv_code TYPE string OPTIONAL
        !iv_msg TYPE string
        !previous LIKE previous OPTIONAL.
    METHODS get_code
      RETURNING
        VALUE(rv_code) TYPE string.
    METHODS get_message_text
      RETURNING
        VALUE(rv_msg) TYPE string.
ENDCLASS.

CLASS zcx_zodata_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous ).
    mv_code = iv_code.
    mv_msg = iv_msg.
  ENDMETHOD.

  METHOD get_code.
    rv_code = mv_code.
  ENDMETHOD.

  METHOD get_message_text.
    rv_msg = mv_msg.
  ENDMETHOD.
ENDCLASS.
