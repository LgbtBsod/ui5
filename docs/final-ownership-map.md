# Final Ownership Map

## Frontend ownership

- OData transport boundary: `app/infra/adapters/*`, `app/service/backend/*`
- UI/edit state: `JSONModel` owners, `ControllerStateRuntime`, `ModelStateRuntime`
- user-facing texts: `app/i18n/*.properties`
- frontend machine-readable codes: `app/constants/*Code*`, feature contracts
- UI routes, entity names, function imports, event names: `app/constants/*Contracts*`
- attachment productive upload seam: `AttachmentGatewayRuntime` + `AttachmentUploadRuntime`

## Backend ownership

- human-readable backend texts: `backend/sap_backend/src/zcl_zodata_message_texts.clas.abap`
- machine-readable backend codes: `zif_zodata_message_codes*`
- compatibility-only legacy key ingress: narrow ABAP/mock gateway boundary owners

## Canonical model rules

- root entity identity: `DB_KEY`
- child relation: `PARENT_KEY`
- root entity must not expose `PARENT_KEY`
- attachment persisted binaries open through `DownloadUrl` / `DocumentHandle`
- productive save flows must not persist attachment `Value` / base64 payloads
