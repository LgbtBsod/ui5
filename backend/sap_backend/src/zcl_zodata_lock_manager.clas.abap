CLASS zcl_zodata_lock_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_zodata_lock_manager.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS call_lock_fm
      IMPORTING
        !iv_mode TYPE c
        !is_key  TYPE zif_zodata_lock_manager=>ty_key
      RAISING
        zcx_zodata_error.
ENDCLASS.

CLASS zcl_zodata_lock_manager IMPLEMENTATION.

  METHOD zif_zodata_lock_manager~lock.
    call_lock_fm(
      iv_mode = 'A'
      is_key  = is_key ).
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~unlock.
    call_lock_fm(
      iv_mode = 'R'
      is_key  = is_key ).
  ENDMETHOD.

  METHOD zif_zodata_lock_manager~update_last_touch.
    call_lock_fm(
      iv_mode = 'T'
      is_key  = is_key ).
  ENDMETHOD.

  METHOD call_lock_fm.
    CALL FUNCTION 'ZODATA_LOCK_CONTROL'
      EXPORTING
        iv_mode      = iv_mode
        iv_bo_key    = is_key-bo_key
        iv_object_id = is_key-object_id
      EXCEPTIONS
        lock_error   = 1
        update_error = 2
        OTHERS       = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_zodata_error
        EXPORTING
          iv_msg = |Lock manager failed. MODE={ iv_mode } BO_KEY={ is_key-bo_key } SUBRC={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
