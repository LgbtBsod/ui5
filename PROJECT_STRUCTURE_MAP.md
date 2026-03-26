# Project Structure Map

- `app/constants`: canonical frontend constants owners.
- `app/infra/adapters`: OData boundary, key normalization, and attachment mapping.
- `app/infra/navigation`: route/state coordination and analytics return intent handling.
- `app/service/features`: feature runtimes.
- `app/service/framework`: cross-cutting runtime helpers.
- `app/service/runtime/component`: component bootstrap, lifecycle, lock, and navigation orchestration.
- `backend/sap_backend/src`: canonical Gateway backend contracts and implementation.
