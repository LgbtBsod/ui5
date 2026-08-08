@AbapCatalog.sqlViewName: 'ZICHKIAGGSQL'
@AbapCatalog.compiler.compareFilter: true
@VDM.viewType: #HELPER
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Check: CheckItem aggregates per RootId (helper for ZI_CheckRoot)'

// Helper view, not published as its own OData EntitySet — ZI_CheckRoot
// associates to it.
//
// ChecksSuccessRate is computed ONCE here and reused by ZI_CheckRoot for both
// SuccessRateChecks (display) and ChecksCriticality (50/80 threshold bucket) —
// single source of the percentage, no duplicated rounding.
//
// group by runs over the full ZCHK_ITEM table without pre-filtering. Accepted
// as-is: ChecksAmount/ChecksSuccessRate are not in UI.SelectionFields (see
// annotations.xml), so they are never $filter targets from the SmartFilterBar,
// and HANA can push the RootId predicate from the outer join down through this
// view. Revisit (materialize into a calculation view with an index on root_id)
// only if these fields are exposed to $filter/$orderby from a raw OData client.
define view ZI_CheckItemAgg
  as select from zchk_item as Item
{
  key Item.root_id                                                 as RootId,

      count( * )                                                    as ChecksAmount,

      // 'X' = satisfactory — same code as ZCL_CHECK_DPC_EXT c_result_satisfactory.
      sum( case when Item.result = 'X' then 1 else 0 end )          as ChecksSuccessCount,

      sum( case when Item.result = '' then 1 else 0 end )           as ChecksErrorCount,

      case when count( * ) = 0 then cast( null as abap.dec( 5, 2 ) )
           else round( cast( sum( case when Item.result = 'X' then 1 else 0 end ) as abap.dec( 15, 2 ) )
                        / count( * ) * 100, 2 )
      end                                                            as ChecksSuccessRate
}
group by Item.root_id
