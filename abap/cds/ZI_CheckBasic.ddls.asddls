@AbapCatalog.sqlViewName: 'ZICHKBASCSQL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Check: Basic data (push-down text resolution)'

// Аналог resolve_check_basic() в serve.py — весь резолвинг код->текст и
// coalesce на *IntegrationName push-down сюда через association+left outer
// join, ничего из этого не считается в ABAP/JS. left outer join
// (не inner!) намеренно: строка CheckBasic обязана вернуться, даже если
// ObserverPernr/LocationKey/... ещё не заполнены (transient-запись до
// первого save) или ссылаются на удалённую с тех пор запись справочника.
define view ZI_CheckBasic
  as select from zchk_basic as Basic

    left outer join zchk_pers as Observer
      on Basic.observer_pernr = Observer.pernr

    left outer join zchk_pers as Observed
      on Basic.observed_pernr = Observed.pernr

    left outer join zchk_pklvl as Pkl
      on Basic.lpc_key = Pkl.pk_level

    left outer join zchk_prof as Prof
      on Basic.prof_key = Prof.profession_code

    left outer join zchk_tzone as Tz
      on Basic.timezone = Tz.time_zone_code

    left outer join zchk_loch as Loc
      on Basic.location_key = Loc.location_uuid
{
  key Basic.root_id                                          as RootId,
      Basic.date                                              as Date,
      Basic.time                                              as Time,
      Basic.timezone                                          as Timezone,
      coalesce( Tz.time_zone_text, '' )                       as TimezoneText,
      Basic.equipment                                         as Equipment,
      Basic.location_key                                      as LocationKey,

      // [Basic-proxy] Совпадает с логикой resolve_check_basic(): резолвленное
      // имя по ключу побеждает, если ключ есть и найден; иначе — то, что
      // реально хранится в строке (ручной/интеграционный текст).
      coalesce( Loc.location_name, Basic.location_name, '' )  as LocationName,
      Basic.location_text                                     as LocationText,

      Basic.observer_pernr                                   as ObserverPernr,
      coalesce( Observer.fio, Basic.observer_integration_name, '' )
                                                                as ObserverFullname,
      Basic.observer_position                                 as ObserverPosition,
      Basic.observer_orgunit                                  as ObserverOrgunit,
      Basic.observer_integration_name                         as ObserverIntegrationName,

      Basic.observed_pernr                                   as ObservedPernr,
      coalesce( Observed.fio, Basic.observed_integration_name, '' )
                                                                as ObservedFullname,
      Basic.observed_position                                 as ObservedPosition,
      Basic.observed_orgunit                                  as ObservedOrgunit,
      Basic.observed_integration_name                         as ObservedIntegrationName,

      Basic.lpc_key                                           as LpcKey,
      coalesce( Pkl.pk_level_text, '' )                       as LpcText,

      Basic.prof_key                                          as ProfKey,
      coalesce( Prof.profession_text, '' )                    as ProfText,

      Basic.last_changed_at                                   as LastChangedAt
}
