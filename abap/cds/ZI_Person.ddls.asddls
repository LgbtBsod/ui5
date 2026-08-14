@AbapCatalog.sqlViewName: 'ZICHKPERSSQL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Check: Person (inspector/observed) reference'
@Search.searchable: true

// Справочник сотрудников для *Fullname/*Pernr (ObserverPernr/ObservedPernr).
// В большинстве ландшафтов персонал уже мастерится в HR (PA0001/PA0002) —
// в реальном проекте предпочтительнее переиспользовать стандартный
// I_Employee/I_WorkforcePersonBasic (авторизация P_ORGIN уже отработана
// стандартом) вместо своей таблицы. Здесь — самостоятельная Z-таблица
// ZCHK_PERS, т.к. HR может не быть модулем-владельцем данных в этом
// ландшафте (не все инспекторы обязательно HR-сотрудники, например
// подрядчики) — решение зависит от конкретного заказчика.
define view ZI_Person
  as select from zchk_pers as Person
{
      @Search.defaultSearchElement: true
  key Person.pernr        as Pernr,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      Person.fio           as Fio,

      Person.active_from   as ActiveFrom,
      Person.active_to     as ActiveTo
}
