@AbapCatalog.sqlViewName: 'ZICHKBTYPSQL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Check: Barrier type reference (customizing)'

// Зеркало ZI_CheckType для барьеров — тот же компромисс по PkLevels
// (см. комментарий там). Отдельная таблица/CDS, а не общий "TypeCode +
// Discriminator"-справочник: у CheckType и BarrierType разная семантика
// набора значений и они никогда не выбираются из одного F4 одновременно
// (см. annotations.xml — раздельные Common.ValueList на CheckItem/Text и
// Barrier/Text) — общая таблица добавила бы условную логику без выгоды.
define view ZI_BarrierType
  as select from zchk_btype as BarrierType
{
  key BarrierType.barrier_type_code as BarrierTypeCode,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      BarrierType.barrier_type_text as BarrierTypeText,

      @Search.defaultSearchElement: true
      BarrierType.category          as Category,

      BarrierType.pk_levels          as PkLevels
}
