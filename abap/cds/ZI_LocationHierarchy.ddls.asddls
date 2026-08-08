@AbapCatalog.sqlViewName: 'ZICHKLOCHSQL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Check: Location hierarchy (tree navigation for LocationDialog)'

// Питает LocationDialog.fragment.xml (ext/util/LocationPicker.js) — плоская
// self-referencing иерархия (ParentLocationUuid), а не Adjacency List CDS
// hierarchy() built-in: клиент сам обходит уровни через
// $filter=ParentLocationUuid eq '...', ему не нужен целиком развёрнутый
// дерево-ответ. HasChildren считается push-down через exists()-подзапрос,
// не хранится денормализованно в таблице (иначе рассинхронизация при
// удалении последнего ребёнка).
define view ZI_LocationHierarchy
  as select from zchk_loch as Loc
{
  key Loc.location_uuid                as LocationUuid,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      Loc.location_name                as LocationName,

      @Search.defaultSearchElement: true
      Loc.location_code                as LocationCode,

      Loc.parent_location_uuid         as ParentLocationUuid,

      @Semantics.booleanFlag: true
      case when exists (
             select 1 from zchk_loch as Child
             where Child.parent_location_uuid = Loc.location_uuid
           )
        then abap_true
        else abap_false
      end                              as HasChildren,

      Loc.hierarchy_level              as Level
}
