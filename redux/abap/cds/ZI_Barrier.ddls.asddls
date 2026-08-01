@AbapCatalog.sqlViewName: 'ZICHKBARRSQL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Check: Barrier (row of a check)'

// Аналог resolve_barrier() — в отличие от ZI_CheckItem, БЕЗ RawText-fallback
// (у Barrier такого поля нет и не было в исходной модели, см. metadata.xml).
define view ZI_Barrier
  as select from zchk_barr as Barr

    left outer join zchk_btype as Btype
      on Barr.code = Btype.barrier_type_code

    left outer join zchk_cres as Cres
      on Barr.result = Cres.result_code
{
  key Barr.root_id                       as RootId,
  key Barr.barrier_id                     as BarrierId,
      Barr.code                          as Code,
      coalesce( Btype.barrier_type_text, '' )
                                           as Text,
      Barr.comment                       as Comment,
      Barr.result                        as Result,
      coalesce( Cres.result_text, '' )   as ResultText,
      Barr.last_changed_at               as LastChangedAt
}
