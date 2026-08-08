*&---------------------------------------------------------------*
*& Class ZCL_CHECK_MPC_EXT
*&---------------------------------------------------------------*
*& Model Provider Class extension — редактирует $metadata в рантайме.
*& Единственный реальный кейс здесь: sap:default-value на Result у
*& CheckItem/Barrier ('X' = удовлетворительно), чтобы SmartTable inline-
*& create (addEntry) уже показывал заполненный Result, а не пустую
*& строку до первого выбора пользователем — сегодня эту роль на клиенте
*& играет SubTableCrud.js (Constants.RESULT_CODES.SATISFACTORY,
*& захардкожен в JS-диалоге создания строки).
*&
*& ПОЧЕМУ НЕ В CDS: аннотации @ObjectModel.* управляют read/create/update-
*& enabled и foreign-key семантикой, но не default-значением OData-
*& свойства (sap:default-value — специфика классической Gateway-схемы
*& метаданных, не CDS). Значение статически можно было бы задать и через
*& редактор аннотаций SEGW (Properties -> Result -> Default Value) без
*& единой строчки ABAP — здесь сделано программно только чтобы не
*& дублировать литерал 'X' в третьем месте (см.
*& ZCL_CHECK_DPC_EXT=>C_RESULT_SATISFACTORY).
*&---------------------------------------------------------------*
CLASS zcl_check_mpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_check_mpc
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS define REDEFINITION.

  PRIVATE SECTION.
    "! iv_entity_name — 'CheckItem' или 'Barrier' (оба несут поле Result
    "! с одинаковой семантикой "X = удовлетворительно").
    METHODS set_result_default_value
      IMPORTING
        iv_entity_name TYPE string.

ENDCLASS.


CLASS zcl_check_mpc_ext IMPLEMENTATION.

  METHOD define.
    super->define( ).

    set_result_default_value( 'CheckItem' ).
    set_result_default_value( 'Barrier' ).
  ENDMETHOD.


  METHOD set_result_default_value.
    DATA(lo_entity_type) = model->get_entity_type( iv_entity_name = iv_entity_name ).
    CHECK lo_entity_type IS BOUND.

    TRY.
        DATA(lo_result_property) = lo_entity_type->get_property( iv_property_name = 'Result' ).
        lo_result_property->set_default_value( 'X' ).
      CATCH /iwbep/cx_mgw_med_exception.
        " Свойство переименовано/удалено в CDS-проекции — не валим весь
        " $metadata из-за одного дефолта, просто не проставляем его.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
