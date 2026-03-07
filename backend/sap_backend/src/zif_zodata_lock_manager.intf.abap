INTERFACE zif_zodata_lock_manager PUBLIC.

  TYPES: BEGIN OF ty_key,
           bo_key    TYPE /bobf/conf_key,
           object_id TYPE sysuuid_x16,
         END OF ty_key,
         tt_key TYPE STANDARD TABLE OF ty_key WITH DEFAULT KEY.

  METHODS lock
    IMPORTING
      !is_key TYPE ty_key
    RAISING
      zcx_zodata_error.

  METHODS unlock
    IMPORTING
      !is_key TYPE ty_key
    RAISING
      zcx_zodata_error.

  METHODS update_last_touch
    IMPORTING
      !is_key TYPE ty_key
    RAISING
      zcx_zodata_error.

ENDINTERFACE.
