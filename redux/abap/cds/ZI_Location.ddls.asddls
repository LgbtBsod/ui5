@AbapCatalog.sqlViewName: 'ZICHKLOCSQL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Check: Location (flat value-help projection)'

// Тот же ZCHK_LOCH, что и ZI_LocationHierarchy, но плоская проекция без
// Parent/HasChildren — источник для Common.ValueList на
// CheckRoot/LocationName (простой F4 без дерева, см. annotations.xml).
// Два CDS-представления над одной таблицей вместо одного "универсального"
// с опциональными полями — разные потребители (дерево vs плоский F4) не
// должны знать о полях, которые им не нужны.
define view ZI_Location
  as select from zchk_loch as Loc
{
  key Loc.location_uuid as LocationUuid,

      @Search.defaultSearchElement: true
      Loc.location_name as LocationName,

      @Search.defaultSearchElement: true
      Loc.location_code as LocationCode
}
