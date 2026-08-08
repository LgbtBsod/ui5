@AbapCatalog.sqlViewName: 'ZICHKPKLVSQL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Check: PK (protection level) reference (customizing)'

// Уровень КПР (LpcKey/LpcText на CheckRoot/CheckBasic). Значения этого
// справочника — единственный "легальный" источник допустимых LpcKey;
// ZCL_CHECK_DPC_EXT не валидирует LpcKey против него на CREATE (это
// делает F4/ValueList на клиенте) — при необходимости серверной валидации
// см. ZCL_CHECK_DPC_EXT=>CHECK_LPC_KEY_VALID в разделе "Не реализовано".
define view ZI_PkLevel
  as select from zchk_pklvl as PkLevel
{
  key PkLevel.pk_level      as PkLevel,

      @Search.defaultSearchElement: true
      PkLevel.pk_level_text as PkLevelText
}
