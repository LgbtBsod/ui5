@AbapCatalog.sqlViewName: 'ZICHKTZONSQL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Check: Time zone reference'

// В реальном ландшафте предпочтительнее переиспользовать стандартный
// справочник часовых поясов ABAP (TTZZ/TTZDF, доступен через CL_TZONE_*
// или системную CDS I_TimeZone, если активирована) вместо своей таблицы —
// здесь своя ZCHK_TZONE только потому, что список нужен объектно
// ограниченным (не все 600+ таймзон системы актуальны для этого
// приложения), см. serve_config.py REFERENCE_DATA.TimeZones — тот же
// сознательно урезанный список из ~9 записей.
define view ZI_TimeZone
  as select from zchk_tzone as TimeZone
{
  key TimeZone.time_zone_code as TimeZoneCode,

      @Search.defaultSearchElement: true
      TimeZone.time_zone_text as TimeZoneText
}
