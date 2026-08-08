@AbapCatalog.sqlViewName: 'ZICHKITEMSQL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Check: Check item (row of a check)'

// Аналог resolve_check_item(): Text = резолвленный текст по Code, а если
// Code не сопоставлен справочнику (строка пришла из интеграции без
// маппинга) — RawText как есть. Это ЕДИНСТВЕННАЯ из трёх дочерних сущностей
// с RawText-fallback — у ZI_Barrier его нет (см. её комментарий) — CDS
// push-down обязан воспроизводить эту асимметрию точно, не "унифицировать".
define view ZI_CheckItem
  as select from zchk_item as Item

    left outer join zchk_ctype as Ctype
      on Item.code = Ctype.check_type_code

    left outer join zchk_cres as Cres
      on Item.result = Cres.result_code
{
  key Item.root_id                                      as RootId,
  key Item.item_id                                       as ItemId,
      Item.code                                          as Code,
      Item.raw_text                                      as RawText,
      coalesce( Ctype.check_type_text, Item.raw_text, '' )
                                                            as Text,
      Item.comment                                       as Comment,
      Item.result                                        as Result,
      coalesce( Cres.result_text, '' )                   as ResultText,
      Item.last_changed_at                               as LastChangedAt
}
