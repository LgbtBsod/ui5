CLASS zcl_zodata_lock_constants DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_mode,
             acquire   TYPE c VALUE 'A',
             release   TYPE c VALUE 'R',
             heartbeat TYPE c VALUE 'H',
             status    TYPE c VALUE 'S',
             validate  TYPE c VALUE 'V',
             touch     TYPE c VALUE 'T',
           END OF ty_mode.

    CONSTANTS cv_mode TYPE ty_mode.

    TYPES: BEGIN OF ty_exception,
             lock_error   TYPE i VALUE 1,
             update_error TYPE i VALUE 2,
             other_error  TYPE i VALUE 3,
           END OF ty_exception.

    CONSTANTS cv_exception TYPE ty_exception.

    CLASS-DATA: g_instance TYPE REF TO zcl_zodata_lock_constants READ-ONLY.

    CLASS-METHODS class_constructor.

  PRIVATE SECTION.
    METHODS constructor.
ENDCLASS.

CLASS zcl_zodata_lock_constants IMPLEMENTATION.
  METHOD class_constructor.
    CREATE OBJECT g_instance.
  ENDMETHOD.

  METHOD constructor.
  ENDMETHOD.
ENDCLASS.
