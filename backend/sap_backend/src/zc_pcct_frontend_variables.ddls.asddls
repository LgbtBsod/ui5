@EndUserText.label: 'PCCT Frontend Variables'
@AbapCatalog.sqlViewName: 'ZVPCCTFVAR'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@VDM.viewType: #BASIC
define view entity ZC_PCCT_FrontendVariables
  as select from zpcct_fe_var as Variables
{
  key Variables.mandt          as Client,
  key Variables.variant_id     as VariantId,
  key Variables.variable_name  as VariableName,
      Variables.variable_type  as VariableType,
      Variables.variable_value as VariableValue,
      Variables.is_enabled     as IsEnabled,
      Variables.sort_order     as SortOrder
}
where
  Variables.is_enabled = 'X'
