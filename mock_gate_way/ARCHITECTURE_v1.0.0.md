# Mock SAP Gateway v1.0.0 (Frozen Architecture)

## Version
- **Release**: `v1.0.0`
- **Status**: Backend architecture frozen for UI5 ODataModel V2 integration.

## Frozen layers
1. **API layer (`api/`)**
   - HTTP contracts and input/output handling only.
   - Business decisions delegated to services.
2. **Service layer (`services/`)**
   - All business logic: locks, ETag, state transitions, metadata generation.
3. **Model layer (`models.py`)**
   - SQLAlchemy entities and relationships only.
4. **Utilities (`utils/`)**
   - Shared protocol helpers (`etag`, `filter_parser`, `odata_response`, `time`).

## OData V2 compliance baseline
- `$metadata` endpoint is available through `api/metadata_api.py`.
- Metadata is generated dynamically from SQLAlchemy mappings in `services/metadata_builder.py`.
- Generated EDMX includes:
  - `EntityType`
  - `EntitySet`
  - `Association`
  - `NavigationProperty`
  - `ReferentialConstraint`
  - `FunctionImport` based on `services/action_registry.py`

## Actions contract
- `SubmitChecklist` (POST) -> `Edm.String`
- `UnlockChecklist` (POST) -> `Edm.Boolean`
- `SetChecklistStatus` (POST) -> `Edm.String`

## Concurrency contract
- Locking with TTL and heartbeat is implemented in `services/lock_service.py`.
- ETag / If-Match checks are implemented in checklist flows (`services/checklist_service.py` + `api/checklist_api.py`).
- Lock cleanup background task runs in app lifespan (`main.py`).

## API and app version pinning
- FastAPI application version is pinned to `1.0.0` in `main.py`.

## Out of scope for this release
- Docker packaging.
- Automated test suite.
