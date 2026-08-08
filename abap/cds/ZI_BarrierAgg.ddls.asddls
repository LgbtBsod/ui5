@AbapCatalog.sqlViewName: 'ZICHKBAGGSQL'
@AbapCatalog.compiler.compareFilter: true
@VDM.viewType: #HELPER
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Check: Barrier aggregates per RootId (helper for ZI_CheckRoot)'

// Mirror of ZI_CheckItemAgg for barriers — same reasoning for the single
// BarriersSuccessRate computation and the accepted full-table group by
// trade-off (see ZI_CheckItemAgg for both).
define view ZI_BarrierAgg
  as select from zchk_barr as Barr
{
  key Barr.root_id                                             as RootId,

      count( * )                                                as BarriersAmount,
      sum( case when Barr.result = 'X' then 1 else 0 end )      as BarriersSuccessCount,
      sum( case when Barr.result = '' then 1 else 0 end )       as BarriersErrorCount,

      case when count( * ) = 0 then cast( null as abap.dec( 5, 2 ) )
           else round( cast( sum( case when Barr.result = 'X' then 1 else 0 end ) as abap.dec( 15, 2 ) )
                        / count( * ) * 100, 2 )
      end                                                        as BarriersSuccessRate
}
group by Barr.root_id
