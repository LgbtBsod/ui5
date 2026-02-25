CLASS zcx_lock_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.

    DATA user_fullname TYPE string READ-ONLY.
    DATA pernr         TYPE p_pernr READ-ONLY.

    METHODS constructor
      IMPORTING
        !textid        LIKE if_t100_message=>t100key OPTIONAL
        !previous      LIKE previous OPTIONAL
        !user_fullname TYPE string OPTIONAL
        !pernr         TYPE p_pernr OPTIONAL.
ENDCLASS.

CLASS zcx_lock_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

    me->user_fullname = user_fullname.
    me->pernr         = pernr.

    IF textid IS INITIAL.
      if_t100_message~t100key = VALUE #( msgid = 'ZLOCK_MSG' msgno = '001' ).
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
