@AbapCatalog.sqlViewName: 'ZICHKPROFSQL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Check: Profession reference (customizing)'

// В реальном HR-ландшафте это, скорее всего, проекция над T513
// (должности/Positions) или HR-CDS I_Job — своя таблица здесь оставлена
// намеренно простой (5 строк), т.к. приложение не привязано к
// орг.структуре HR напрямую (см. комментарий в ZI_Person.ddls.asddls).
define view ZI_Profession
  as select from zchk_prof as Profession
{
  key Profession.profession_code as ProfessionCode,

      @Search.defaultSearchElement: true
      Profession.profession_text as ProfessionText
}
