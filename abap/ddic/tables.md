# Транспарентные таблицы (DDIC), лежащие под `abap/cds/*.ddls.asddls`

Не полные `.tabl`-описания (это делается через SE11/ADT мастер) — только состав
полей, чтобы CDS-вьюхи выше были осмысленными. Имена ≤16 символов (лимит DDIC).

Соглашение по ключам: `RootId`/`ItemId`/`BarrierId`/`LocationUuid` — `SYSUUID`/
`CHAR(36)`, генерируются в ABAP (`cl_system_uuid`), не в БД (см. `ZCL_CHECK_DPC_EXT`).

## Транзакционные

**ZCHK_ROOT** (шапка проверки)
| Поле | Тип | Прим. |
|---|---|---|
| MANDT | CLNT(3) | |
| ROOT_ID | CHAR(36) | Key |
| DOC_ID | CHAR(10) | Nullable в БД, обязателен в бизнес-смысле (draw_doc_id) |
| STATUS | CHAR(20) | 'OK' / 'CRITICAL' / 'WARNING' — источник StatusCriticality |
| THIS_IS_INTEGRATION_DATA | CHAR(1) | XFELD |
| CREATED_BY | CHAR(12) | SY-UNAME на create |
| CREATED_AT | TIMESTAMPL | |
| LAST_CHANGED_AT | TIMESTAMPL | ConcurrencyMode=Fixed в OData — источник ETag-компонента |

**ZCHK_BASIC** (1:1 к ZCHK_ROOT, независимый ETag)
`ROOT_ID` (key, FK ZCHK_ROOT), `DATE` (DATS), `TIME` (TIMS), `TIMEZONE` (CHAR 50),
`EQUIPMENT` (CHAR 100), `LOCATION_KEY` (CHAR 36), `LOCATION_NAME` (CHAR 100,
денормализованный fallback-текст — см. ZI_CheckBasic coalesce), `LOCATION_TEXT`
(CHAR 100, текст из интеграции), `OBSERVER_PERNR`/`OBSERVED_PERNR` (CHAR 8),
`OBSERVER_INTEGRATION_NAME`/`OBSERVED_INTEGRATION_NAME` (CHAR 100),
`OBSERVER_POSITION`/`OBSERVER_ORGUNIT`/`OBSERVED_POSITION`/`OBSERVED_ORGUNIT`
(CHAR 100, только из интеграции — UI read-only), `LPC_KEY` (CHAR 10),
`PROF_KEY` (CHAR 50), `LAST_CHANGED_AT` (TIMESTAMPL).

**ZCHK_ITEM** (строки проверки, N:1 к ZCHK_ROOT)
`ROOT_ID` (key, FK), `ITEM_ID` (key, CHAR 36), `CODE` (CHAR 30, FK ZCHK_CTYPE,
nullable — интеграция может не сопоставить), `RAW_TEXT` (CHAR 200),
`COMMENT` (CHAR 500), `RESULT` (CHAR 1, 'X'/''), `LAST_CHANGED_AT` (TIMESTAMPL).

**ZCHK_BARR** (барьеры, N:1 к ZCHK_ROOT)
`ROOT_ID` (key, FK), `BARRIER_ID` (key, CHAR 36), `CODE` (CHAR 30, FK
ZCHK_BTYPE, NOT NULL), `COMMENT` (CHAR 500), `RESULT` (CHAR 1),
`LAST_CHANGED_AT` (TIMESTAMPL).

## Справочники (customizing, ведение через SM30 generated maintenance view)

- **ZCHK_CTYPE**: `CHECK_TYPE_CODE` (key, CHAR 30), `CHECK_TYPE_TEXT` (CHAR 100),
  `CATEGORY` (CHAR 50), `PK_LEVELS` (CHAR 50, CSV — см. ограничение в
  ZI_CheckType.ddls.asddls).
- **ZCHK_BTYPE**: зеркало ZCHK_CTYPE для барьеров (`BARRIER_TYPE_CODE`/`_TEXT`).
- **ZCHK_CRES**: `RESULT_CODE` (key, CHAR 20), `RESULT_TEXT` (CHAR 100).
- **ZCHK_PKLVL**: `PK_LEVEL` (key, CHAR 10), `PK_LEVEL_TEXT` (CHAR 100).
- **ZCHK_TZONE**: `TIME_ZONE_CODE` (key, CHAR 50), `TIME_ZONE_TEXT` (CHAR 100).
- **ZCHK_PROF**: `PROFESSION_CODE` (key, CHAR 50), `PROFESSION_TEXT` (CHAR 100).
- **ZCHK_LOCH**: `LOCATION_UUID` (key, CHAR 36), `LOCATION_NAME` (CHAR 100),
  `LOCATION_CODE` (CHAR 20), `PARENT_LOCATION_UUID` (CHAR 36, self-FK, blank
  для корня), `HIERARCHY_LEVEL` (INT4). Питает и ZI_Location, и
  ZI_LocationHierarchy (см. их комментарии).
- **ZCHK_PERS**: `PERNR` (key, CHAR 8), `FIO` (CHAR 100), `ACTIVE_FROM`/
  `ACTIVE_TO` (DATS) — см. оговорку про HR-CDS в ZI_Person.ddls.asddls.

## Явно не переносится 1:1 из мока

`serve_config.py REFERENCE_DATA` — это seed-данные для локальной разработки
(6 сотрудников, 6 локаций, 8 типов проверок и т.д.). В реальной системе эти
таблицы наполняются через SM30/загрузку, не хардкодятся в коде.
