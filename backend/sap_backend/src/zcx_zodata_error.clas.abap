CLASS zcx_zodata_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA mv_msg TYPE string READ-ONLY.

    METHODS constructor
      IMPORTING
        !iv_msg TYPE string
        !previous LIKE previous OPTIONAL.
ENDCLASS.

CLASS zcx_zodata_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous ).
    mv_msg = iv_msg.
  ENDMETHOD.
ENDCLASS.
