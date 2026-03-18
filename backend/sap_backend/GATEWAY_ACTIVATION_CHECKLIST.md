# Gateway Activation Checklist

Service contract baseline:

- Service technical name: `Z_EHS_PRODUCTION_CONTROL_CKLT_SRV`
- Frontend manifest alias: `mainService`
- Service root URI: `/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/`
- OData version: `v2`

## Operator checklist

- [ ] Confirm the transport owner for service registration and ICF activation.
- [ ] Confirm the target system alias that the FLP / ICF contour must use.
- [ ] In `/IWFND/MAINT_SERVICE`, register `Z_EHS_PRODUCTION_CONTROL_CKLT_SRV` in the target client/system alias.
- [ ] Verify the technical service version and package assignment match the deployment transport.
- [ ] Activate the SICF node chain for `/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/` and dependent parent nodes if still inactive.
- [ ] Execute `$metadata` in `/IWFND/GW_CLIENT` and confirm HTTP 200 plus EDMX download.
- [ ] Verify `$batch` support in `/IWFND/GW_CLIENT` with a read request and a CSRF-protected modifying request.
- [ ] Fetch a CSRF token with `X-CSRF-Token: Fetch` and confirm modify flows accept the returned token.
- [ ] Verify lock endpoints/functions used by the app (`LockAcquire`, `LockHeartbeat`, `LockRelease`) respond on the registered alias.
- [ ] Confirm current-user and runtime-settings resources resolve without fallback adapters.
- [ ] Record who owns follow-up corrections if registration, alias, or SICF activation differs between QA and production.

## Evidence to capture

- [ ] Screenshot or export from `/IWFND/MAINT_SERVICE` showing the registered service and alias.
- [ ] SICF activation proof for the service path.
- [ ] `$metadata` response timestamp and system/client used.
- [ ] `$batch` request/response sample proving multipart handling.
- [ ] CSRF token fetch and modifying request proof.
- [ ] Lock lifecycle proof showing acquire, heartbeat, and release on the real Gateway contour.
