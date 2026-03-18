# Metadata Parity Checklist

Compare the productive Gateway `$metadata` against the mock contract and the frontend assumptions.

Service root under test: `/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/`

| Area | What to verify on mock metadata / docs | What must be confirmed on real Gateway `$metadata` | Status |
| --- | --- | --- | --- |
| Entity sets | Search, detail, attachment, permission, current-user, runtime-settings, lock, analytics entity sets/resources exist in the mock contract. | Real `$metadata` exposes the same entity sets/function imports actually consumed by the UI. | [ ] |
| Property names | Property names used in adapters, formatters, and builders match mock metadata. | Productive names are identical or adapter-boundary mapping is updated before go-live. | [ ] |
| Types | `Edm.String`, `Edm.Boolean`, `Edm.DateTime`, numeric analytics fields, and lock timestamps match mock expectations. | Productive `$metadata` confirms matching EDM types for all consumed fields. | [ ] |
| Nullable | Required flags for save/create payloads and lock/current-user resources are understood in mock docs. | Productive nullable settings do not break current payload builders or form validation. | [ ] |
| Binary/RAW16 keys | `RootKey`/`ParentKey`/DB checklist keys stay `Edm.Binary` in the mock contract. | Productive Gateway keeps the same RAW16/`Edm.Binary` typing and typed-literal key handling. | [ ] |
| Attachments | Attachment entity payload contains file metadata plus `Value` as `Edm.Binary` in mock docs. | Productive attachment metadata and save payload structure match or are adapted only in the adapter layer. | [ ] |
| Lock entities/functions | Mock contract documents acquire/heartbeat/release semantics and fields like `owner_session`, `lock_expires_at`, `server_now`. | Productive metadata plus runtime responses expose the same lock lifecycle contract. | [ ] |
| Current user / permissions / runtime settings | Mock docs define `CurrentUserSet`, `ChecklistCreatePermissionSet('CURRENT')`, and runtime settings payload fields. | Productive metadata/resources return those entities with matching identity semantics and field names. | [ ] |
| Analytics resources | Mock analytics resources expose aggregate payloads, compare-year indicators, and refresh state contract. | Productive analytics metadata/resources return the same contract and support filtered breakdown requests. | [ ] |
| Batch / concurrency | Mock service supports `$batch`, ETags, and lock-aware save semantics. | Productive Gateway confirms multipart batch handling, ETag behavior, and concurrency expectations. | [ ] |

## Required comparison inputs

- [ ] Mock contract reference: `backend/mock_gateway/README_ODATA.md`
- [ ] Mock implementation metadata builder / API output for the current branch.
- [ ] Productive or QA Gateway `$metadata` export from the actual target contour.
- [ ] One captured `$batch` request/response pair from the real Gateway.

## Sign-off reminders

- [ ] Do not mark parity as complete until the real Gateway `$metadata` has been checked.
- [ ] If a divergence is found, document whether it is fixed in Gateway or adapted in the frontend adapter boundary.
- [ ] Re-run smoke coverage for attachments, locks, analytics, and permissions after any metadata correction.
