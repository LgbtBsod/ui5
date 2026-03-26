INTERFACE zif_zodata_message_codes PUBLIC.

  CONSTANTS lock_ok TYPE string VALUE 'LOCK_OK'.
  CONSTANTS validation_error TYPE string VALUE 'VALIDATION_ERROR'.
  CONSTANTS technical_error TYPE string VALUE 'TECHNICAL_ERROR'.
  CONSTANTS lock_missing TYPE string VALUE 'LOCK_MISSING'.
  CONSTANTS lock_expired TYPE string VALUE 'LOCK_EXPIRED'.
  CONSTANTS lock_stolen TYPE string VALUE 'LOCK_STOLEN'.
  CONSTANTS lock_not_owned_by_session TYPE string VALUE 'LOCK_NOT_OWNED_BY_SESSION'.
  CONSTANTS permission_denied TYPE string VALUE 'PERMISSION_DENIED'.
  CONSTANTS authorized TYPE string VALUE 'AUTHORIZED'.
  CONSTANTS read_only_auth TYPE string VALUE 'READ_ONLY_AUTH'.
  CONSTANTS no_view_auth TYPE string VALUE 'NO_VIEW_AUTH'.
  CONSTANTS no_edit_auth TYPE string VALUE 'NO_EDIT_AUTH'.
  CONSTANTS no_delete_auth TYPE string VALUE 'NO_DELETE_AUTH'.
  CONSTANTS no_create_auth TYPE string VALUE 'NO_CREATE_AUTH'.
  CONSTANTS release_save_failed TYPE string VALUE 'RELEASE_SAVE_FAILED'.
  CONSTANTS no_changes TYPE string VALUE 'NO_CHANGES'.
  CONSTANTS saved TYPE string VALUE 'SAVED'.
  CONSTANTS triggered TYPE string VALUE 'TRIGGERED'.

ENDINTERFACE.
