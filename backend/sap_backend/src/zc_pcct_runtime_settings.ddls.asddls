@EndUserText.label: 'PCCT Runtime Settings Source'
@AbapCatalog.sqlViewName: 'ZVPCCTRTSET'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@VDM.viewType: #BASIC
define view entity ZC_PCCT_RuntimeSettings
  as select from zpcct_rt_cfg as Settings
{
  key Settings.mandt                     as Client,
  key Settings.profile_id                as ProfileId,
  key Settings.is_active                 as IsActive,
      Settings.environment               as Environment,
      Settings.heartbeat_ms              as HeartbeatMs,
      Settings.idle_ms                   as IdleMs,
      Settings.autosave_interval_ms      as AutosaveIntervalMs,
      Settings.lock_refresh_ms           as LockRefreshMs,
      Settings.analytics_refresh_ms      as AnalyticsRefreshMs,
      Settings.gcd_interval_ms           as GcdIntervalMs,
      Settings.network_grace_ms          as NetworkGraceMs,
      Settings.cache_tolerance_ms        as CacheToleranceMs,
      Settings.permission_rules_variant  as PermissionRulesVariant,
      Settings.required_fields_variant   as RequiredFieldsVariant,
      Settings.upload_policy_variant     as UploadPolicyVariant,
      Settings.frontend_vars_variant     as FrontendVarsVariant,
      Settings.valid_from                as ValidFrom,
      Settings.valid_to                  as ValidTo
}
where
      Settings.is_active = 'X'
  and Settings.valid_from <= $session.system_date
  and ( Settings.valid_to is initial or Settings.valid_to >= $session.system_date )
