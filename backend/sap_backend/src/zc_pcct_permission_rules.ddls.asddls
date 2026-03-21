@EndUserText.label: 'PCCT Permission Rules'
@AbapCatalog.sqlViewName: 'ZVPCCTPRULE'
@AccessControl.authorizationCheck: #CHECK
@VDM.viewType: #BASIC
define view entity ZC_PCCT_PermissionRules
  as select from zpcct_prule as PermissionRules
{
  key PermissionRules.mandt          as Client,
  key PermissionRules.variant_id     as VariantId,
  key PermissionRules.rule_id        as RuleId,
      PermissionRules.activity       as Activity,
      PermissionRules.scope_name     as ScopeName,
      PermissionRules.scope_value    as ScopeValue,
      PermissionRules.effect_code    as EffectCode,
      PermissionRules.message_key    as MessageKey,
      PermissionRules.sort_order     as SortOrder
}
