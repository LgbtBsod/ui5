@EndUserText.label: 'PCCT Required Fields'
@AbapCatalog.sqlViewName: 'ZVPCCTREQFL'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@VDM.viewType: #BASIC
define view entity ZC_PCCT_RequiredFields
  as select from zpcct_reqfld as RequiredFields
{
  key RequiredFields.mandt        as Client,
  key RequiredFields.variant_id   as VariantId,
  key RequiredFields.field_name   as FieldName,
      RequiredFields.scope_name   as ScopeName,
      RequiredFields.is_required  as IsRequired,
      RequiredFields.sort_order   as SortOrder
}
where
  RequiredFields.is_required = 'X'
