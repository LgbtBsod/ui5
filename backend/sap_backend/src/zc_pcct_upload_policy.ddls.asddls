@EndUserText.label: 'PCCT Upload Policy'
@AbapCatalog.sqlViewName: 'ZVPCCTUPPOL'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@VDM.viewType: #BASIC
define view entity ZC_PCCT_UploadPolicy
  as select from zpcct_upl_pl as UploadPolicy
{
  key UploadPolicy.mandt              as Client,
  key UploadPolicy.variant_id         as VariantId,
  key UploadPolicy.mime_type          as MimeType,
      UploadPolicy.file_extension     as FileExtension,
      UploadPolicy.max_size_mb        as MaxSizeMb,
      UploadPolicy.max_file_count     as MaxFileCount,
      UploadPolicy.scan_required      as ScanRequired,
      UploadPolicy.is_enabled         as IsEnabled,
      UploadPolicy.sort_order         as SortOrder
}
where
  UploadPolicy.is_enabled = 'X'
