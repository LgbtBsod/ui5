*&---------------------------------------------------------------*
*& Class ZCL_CHECK_ROOT_ETAG_AMDP
*&---------------------------------------------------------------*
*& Аналог compute_etag_timestamp_ms() в serve.py.
*&
*& ПОЧЕМУ ЭТО AMDP, А НЕ CDS-ВЬЮХА:
*& ETag представления CheckRoots(...) обязан отражать max(LastChangedAt)
*& по ЧЕТЫРЁМ независимым таблицам (Root, Basic, все CheckItem, все
*& Barrier), потому что вычисляемые поля ChecksAmount/HeaderKpiTitle/...
*& в ZI_CheckRoot меняются при add/delete дочерней строки, даже когда сам
*& Root не PATCH-ился — если ETag взять только из ZCHK_ROOT.LAST_CHANGED_AT,
*& клиент не заметит изменение представления и получит 412 Precondition
*& Failed на следующем PATCH с устаревшим If-Match, либо (хуже) не увидит
*& нужды обновить кэш.
*&
*& Формально это МОЖНО выразить в classic CDS через ассоциации +
*& агрегирующие helper-вьюхи (как ZI_CheckItemAgg/ZI_BarrierAgg для
*& ChecksAmount), но для ЕДИНСТВЕННОГО скалярного MAX() по четырём
*& источникам ради одного поля ETag это избыточная вьюха ради вьюхи —
*& AMDP даёт то же самое push-down (один SQLScript-запрос, выполняется
*& в HANA, не в ABAP) компактнее и, что важнее, работает СРАЗУ для набора
*& RootId (batch/list-сценарий) без N+1 — DPC_EXT~GET_ENTITYSET вызывает
*& этот метод ОДИН раз на всю страницу результатов, не по разу на строку.
*&---------------------------------------------------------------*
CLASS zcl_check_root_etag_amdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.

    TYPES:
      BEGIN OF ty_root_id_row,
        root_id TYPE zchk_root-root_id,
      END OF ty_root_id_row,
      tt_root_id TYPE STANDARD TABLE OF ty_root_id_row WITH EMPTY KEY,

      BEGIN OF ty_etag_result,
        root_id        TYPE zchk_root-root_id,
        max_changed_at TYPE timestampl,
      END OF ty_etag_result,
      tt_etag_result TYPE STANDARD TABLE OF ty_etag_result WITH EMPTY KEY.

    "! Возвращает по одной строке на каждый запрошенный RootId с
    "! max(LastChangedAt) по Root + Basic + все CheckItem + все Barrier
    "! этого Root. RootId, для которых Root не найден, в результате не
    "! появляются (INNER JOIN) — вызывающий код (ZCL_CHECK_DPC_EXT)
    "! обязан обработать отсутствие как "нет такой записи", это не баг.
    CLASS-METHODS get_etag_timestamps
      IMPORTING
        VALUE(it_root_id)     TYPE tt_root_id
      EXPORTING
        VALUE(et_etag_result) TYPE tt_etag_result.

ENDCLASS.


CLASS zcl_check_root_etag_amdp IMPLEMENTATION.

  METHOD get_etag_timestamps
    BY DATABASE PROCEDURE
    FOR HDB
    LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY
    USING zchk_root zchk_basic zchk_item zchk_barr.

    et_etag_result =
      SELECT
        r.root_id                                    AS root_id,
        GREATEST(
          COALESCE( r.last_changed_at, r.created_at ),
          COALESCE( b.last_changed_at, r.created_at ),
          COALESCE( ci.max_changed_at, r.created_at ),
          COALESCE( cb.max_changed_at, r.created_at )
        )                                             AS max_changed_at
      FROM :it_root_id AS ids
      INNER JOIN zchk_root AS r
        ON r.root_id = ids.root_id
      LEFT OUTER JOIN zchk_basic AS b
        ON b.root_id = r.root_id
      LEFT OUTER JOIN (
          SELECT root_id, MAX( last_changed_at ) AS max_changed_at
          FROM zchk_item
          GROUP BY root_id
      ) AS ci
        ON ci.root_id = r.root_id
      LEFT OUTER JOIN (
          SELECT root_id, MAX( last_changed_at ) AS max_changed_at
          FROM zchk_barr
          GROUP BY root_id
      ) AS cb
        ON cb.root_id = r.root_id;

  ENDMETHOD.

ENDCLASS.
